#' Merge DBSN layers from cached provincial geodatabases
#'
#' Searches the local dbsnR cache for provincial File Geodatabases
#' and merges equal layers across one or more provinces.
#'
#' Merge results can optionally be saved as RDS files. Subsequent calls
#' with the same provinces, layers, and options can load the cached merge
#' instead of reading the provincial geodatabases again.
#'
#' @param layer Character vector with the layer names to merge.
#'   If NULL, all available layers are merged separately.
#'
#' @param province Character vector with province codes, for example
#'   `c("RI", "AQ", "TE")`. If NULL, all provinces available in the
#'   cache are used.
#'
#' @param cache_dir Optional path to the dbsnR datasets cache.
#'   If NULL, the default dbsnR cache is used.
#'
#' @param add_source Logical. If TRUE, adds the columns `province`
#'   and `source_gdb`.
#'
#' @param cache Logical. If TRUE, merge results are stored as RDS files
#'   and reused in subsequent calls with the same parameters.
#'
#' @param overwrite Logical. If TRUE, an existing cached merge is ignored
#'   and replaced with a newly calculated merge.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return
#' If a single layer is requested, an sf object.
#'
#' If multiple layers are requested or `layer = NULL`, a named list
#' of sf objects.
#'
#' @details
#' Cached merge files are stored in the `merged` directory next to the
#' DBSN `datasets` cache.
#'
#' The cache filename identifies:
#' \itemize{
#'   \item the selected provinces;
#'   \item the selected layers;
#'   \item whether source columns were added.
#' }
#'
#' Use `overwrite = TRUE` after updating provincial source data or when
#' a merge must be recalculated.
#'
#' @examples
#' \dontrun{
#' loc_sg <- dbsn_merge(
#'   layer = "loc_sg",
#'   province = c(
#'     "AP", "AQ", "PG", "RI",
#'     "RM", "TE", "TR", "VT"
#'   ),
#'   cache = TRUE
#' )
#'
#' merged <- dbsn_merge(
#'   province = c("RI", "AQ", "TE"),
#'   cache = TRUE
#' )
#'
#' merged <- dbsn_merge(
#'   province = c("RI", "AQ", "TE"),
#'   cache = TRUE,
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
dbsn_merge <- function(
    layer = NULL,
    province = NULL,
    cache_dir = NULL,
    add_source = TRUE,
    cache = TRUE,
    overwrite = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate options
  # ---------------------------------------------------------------------------

  if (
    !is.logical(add_source) ||
    length(add_source) != 1L ||
    is.na(add_source)
  ) {
    stop(
      "`add_source` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(cache) ||
    length(cache) != 1L ||
    is.na(cache)
  ) {
    stop(
      "`cache` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(overwrite) ||
    length(overwrite) != 1L ||
    is.na(overwrite)
  ) {
    stop(
      "`overwrite` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(quiet) ||
    length(quiet) != 1L ||
    is.na(quiet)
  ) {
    stop(
      "`quiet` must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Identify datasets cache directory
  # ---------------------------------------------------------------------------

  if (is.null(cache_dir)) {

    local_appdata <- Sys.getenv(
      "LOCALAPPDATA"
    )

    if (nzchar(local_appdata)) {

      cache_dir <- file.path(
        local_appdata,
        "dbsnR",
        "datasets"
      )

    } else {

      cache_dir <- file.path(
        tools::R_user_dir(
          "dbsnR",
          which = "cache"
        ),
        "datasets"
      )
    }
  }

  cache_dir <- normalizePath(
    cache_dir,
    winslash = "/",
    mustWork = FALSE
  )

  if (!dir.exists(cache_dir)) {
    stop(
      "The dbsnR cache directory does not exist:\n",
      cache_dir,
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Define merged cache directory
  # ---------------------------------------------------------------------------

  merged_cache_dir <- file.path(
    dirname(cache_dir),
    "merged"
  )

  if (cache && !dir.exists(merged_cache_dir)) {

    created <- dir.create(
      merged_cache_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!created && !dir.exists(merged_cache_dir)) {
      stop(
        "Unable to create the merged cache directory:\n",
        merged_cache_dir,
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 4. Find provinces available in cache
  # ---------------------------------------------------------------------------

  province_dirs <- list.dirs(
    path = cache_dir,
    recursive = FALSE,
    full.names = TRUE
  )

  if (length(province_dirs) == 0L) {
    stop(
      "No province directories were found in the dbsnR cache.",
      call. = FALSE
    )
  }

  available_provinces <- basename(
    province_dirs
  )


  # ---------------------------------------------------------------------------
  # 5. Select requested provinces
  # ---------------------------------------------------------------------------

  if (is.null(province)) {

    selected_provinces <- available_provinces

  } else {

    if (
      !is.character(province) ||
      length(province) == 0L
    ) {
      stop(
        "`province` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    province <- unique(
      toupper(province)
    )

    missing_provinces <- setdiff(
      province,
      toupper(available_provinces)
    )

    if (length(missing_provinces) > 0L) {
      warning(
        "The following provinces were not found in the cache: ",
        paste(
          missing_provinces,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    selected_provinces <- available_provinces[
      toupper(available_provinces) %in% province
    ]
  }

  if (length(selected_provinces) == 0L) {
    stop(
      "None of the requested provinces is available in the cache.",
      call. = FALSE
    )
  }

  selected_provinces <- sort(
    unique(
      toupper(selected_provinces)
    )
  )

  province_dirs <- province_dirs[
    toupper(basename(province_dirs)) %in%
      selected_provinces
  ]


  # ---------------------------------------------------------------------------
  # 6. Find File Geodatabases
  # ---------------------------------------------------------------------------

  gdb_paths <- unlist(
    lapply(
      province_dirs,
      function(x) {

        dirs <- list.dirs(
          path = x,
          recursive = FALSE,
          full.names = TRUE
        )

        dirs[
          grepl(
            "\\.gdb$",
            dirs,
            ignore.case = TRUE
          )
        ]
      }
    ),
    use.names = FALSE
  )

  gdb_paths <- sort(
    unique(gdb_paths)
  )

  if (length(gdb_paths) == 0L) {
    stop(
      "No File Geodatabase was found for the selected provinces.",
      call. = FALSE
    )
  }

  if (!quiet) {
    message(
      "Found ",
      length(gdb_paths),
      " geodatabase(s): ",
      paste(
        basename(dirname(gdb_paths)),
        collapse = ", "
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Build layer catalogue
  # ---------------------------------------------------------------------------

  gdb_catalogue <- lapply(
    gdb_paths,
    function(gdb_path) {

      province_code <- toupper(
        basename(
          dirname(gdb_path)
        )
      )

      layer_names <- tryCatch(

        sf::st_layers(gdb_path)$name,

        error = function(e) {

          warning(
            "Unable to inspect geodatabase for province ",
            province_code,
            ": ",
            conditionMessage(e),
            call. = FALSE
          )

          character(0)
        }
      )

      if (length(layer_names) == 0L) {
        return(NULL)
      }

      data.frame(
        province = province_code,
        gdb_path = gdb_path,
        layer = layer_names,
        stringsAsFactors = FALSE
      )
    }
  )

  gdb_catalogue <- Filter(
    Negate(is.null),
    gdb_catalogue
  )

  if (length(gdb_catalogue) == 0L) {
    stop(
      "No readable layers were found in the selected geodatabases.",
      call. = FALSE
    )
  }

  gdb_catalogue <- do.call(
    rbind,
    gdb_catalogue
  )


  # ---------------------------------------------------------------------------
  # 8. Determine layers to merge
  # ---------------------------------------------------------------------------

  available_layers <- sort(
    unique(
      gdb_catalogue$layer
    )
  )

  merge_all <- is.null(layer)

  if (merge_all) {

    layers_to_merge <- available_layers

  } else {

    if (
      !is.character(layer) ||
      length(layer) == 0L
    ) {
      stop(
        "`layer` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    layer <- unique(layer)

    missing_layers <- setdiff(
      layer,
      available_layers
    )

    if (length(missing_layers) > 0L) {
      warning(
        "The following layers were not found: ",
        paste(
          missing_layers,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    layers_to_merge <- intersect(
      layer,
      available_layers
    )
  }

  if (length(layers_to_merge) == 0L) {
    stop(
      "None of the requested layers is available.",
      call. = FALSE
    )
  }

  layers_to_merge <- sort(
    unique(layers_to_merge)
  )


  # ---------------------------------------------------------------------------
  # 9. Build merged-cache filename
  # ---------------------------------------------------------------------------

  province_key <- paste(
    selected_provinces,
    collapse = "_"
  )

  if (merge_all) {

    layer_key <- "ALL"

  } else {

    layer_key <- paste(
      layers_to_merge,
      collapse = "_"
    )
  }

  source_key <- if (add_source) {
    "source"
  } else {
    "nosource"
  }

  cache_name <- paste(
    "merged",
    province_key,
    layer_key,
    source_key,
    sep = "__"
  )

  cache_name <- gsub(
    "[^A-Za-z0-9_.-]",
    "_",
    cache_name
  )

  cache_file <- file.path(
    merged_cache_dir,
    paste0(
      cache_name,
      ".rds"
    )
  )


  # ---------------------------------------------------------------------------
  # 10. Load existing cached merge
  # ---------------------------------------------------------------------------

  if (
    cache &&
    file.exists(cache_file) &&
    !overwrite
  ) {

    if (!quiet) {
      message(
        "Loading cached merge:\n",
        cache_file
      )
    }

    cached_result <- tryCatch(

      readRDS(cache_file),

      error = function(e) {
        stop(
          "Unable to read the cached merge:\n",
          cache_file,
          "\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    return(
      cached_result
    )
  }


  # ---------------------------------------------------------------------------
  # 11. Internal function to merge one layer
  # ---------------------------------------------------------------------------

  merge_one_layer <- function(layer_name) {

    sources <- gdb_catalogue[
      gdb_catalogue$layer == layer_name,
      ,
      drop = FALSE
    ]

    if (!quiet) {
      message(
        "Merging '",
        layer_name,
        "' from ",
        nrow(sources),
        " province(s)..."
      )
    }

    objects <- vector(
      "list",
      nrow(sources)
    )

    for (i in seq_len(nrow(sources))) {

      province_code <- sources$province[i]
      gdb_path <- sources$gdb_path[i]

      if (!quiet) {
        message(
          "  - ",
          province_code
        )
      }

      objects[[i]] <- tryCatch(

        {

          object <- sf::st_read(
            dsn = gdb_path,
            layer = layer_name,
            quiet = TRUE,
            stringsAsFactors = FALSE
          )

          if (nrow(object) == 0L) {

            if (!quiet) {
              message(
                "    empty, skipped"
              )
            }

            return(NULL)
          }

          if (add_source) {

            object$province <- rep(
              province_code,
              nrow(object)
            )

            object$source_gdb <- rep(
              basename(gdb_path),
              nrow(object)
            )
          }

          object
        },

        error = function(e) {

          warning(
            "Unable to read layer '",
            layer_name,
            "' for province ",
            province_code,
            ": ",
            conditionMessage(e),
            call. = FALSE
          )

          NULL
        }
      )
    }


    # -------------------------------------------------------------------------
    # Remove failed or empty reads
    # -------------------------------------------------------------------------

    objects <- Filter(
      Negate(is.null),
      objects
    )

    if (length(objects) == 0L) {

      warning(
        "Layer '",
        layer_name,
        "' could not be read from any province.",
        call. = FALSE
      )

      return(NULL)
    }


    # -------------------------------------------------------------------------
    # Harmonise CRS
    # -------------------------------------------------------------------------

    reference_crs <- sf::st_crs(
      objects[[1]]
    )

    if (!is.na(reference_crs)) {

      objects <- lapply(
        objects,
        function(object) {

          current_crs <- sf::st_crs(
            object
          )

          if (
            !is.na(current_crs) &&
            current_crs != reference_crs
          ) {

            object <- sf::st_transform(
              object,
              reference_crs
            )
          }

          object
        }
      )
    }


    # -------------------------------------------------------------------------
    # Merge
    # -------------------------------------------------------------------------

    merged <- dplyr::bind_rows(
      objects
    )

    rownames(merged) <- NULL

    merged
  }


  # ---------------------------------------------------------------------------
  # 12. Merge selected layers
  # ---------------------------------------------------------------------------

  result <- lapply(
    layers_to_merge,
    merge_one_layer
  )

  names(result) <- layers_to_merge

  result <- Filter(
    Negate(is.null),
    result
  )

  if (length(result) == 0L) {
    stop(
      "No layer could be merged successfully.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 13. Prepare returned object
  # ---------------------------------------------------------------------------

  if (
    !merge_all &&
    length(layer) == 1L &&
    length(result) == 1L
  ) {

    final_result <- result[[1]]

  } else {

    final_result <- result
  }


  # ---------------------------------------------------------------------------
  # 14. Save merge as RDS
  # ---------------------------------------------------------------------------

  if (cache) {

    if (!quiet) {
      message(
        "Saving merged cache:\n",
        cache_file
      )
    }

    tryCatch(

      saveRDS(
        final_result,
        file = cache_file,
        compress = TRUE
      ),

      error = function(e) {
        warning(
          "Unable to save the merged cache:\n",
          cache_file,
          "\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }


  # ---------------------------------------------------------------------------
  # 15. Return result
  # ---------------------------------------------------------------------------

  final_result
}
