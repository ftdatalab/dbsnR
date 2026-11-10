#' Merge DBSN layers from cached provincial geodatabases
#'
#' Searches the local dbsnR cache for provincial File Geodatabases
#' and merges equal layers across one or more provinces.
#'
#' @param layer Character vector with the layer names to merge.
#'   If NULL, all available layers are merged separately.
#' @param province Character vector with province codes, for example
#'   c("RI", "AQ", "TE"). If NULL, all provinces available in the
#'   cache are used.
#' @param cache_dir Optional path to the dbsnR datasets cache.
#'   If NULL, the default dbsnR cache is used.
#' @param add_source Logical. If TRUE, adds the columns `province`
#'   and `source_gdb`.
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return
#' If a single layer is requested, an sf object.
#' If multiple layers are requested or `layer = NULL`, a named list
#' of sf objects.
#'
#' @export
dbsn_merge <- function(
    layer = NULL,
    province = NULL,
    cache_dir = NULL,
    add_source = TRUE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Identify cache directory
  # ---------------------------------------------------------------------------

  if (is.null(cache_dir)) {

    local_appdata <- Sys.getenv("LOCALAPPDATA")

    if (nzchar(local_appdata)) {

      cache_dir <- file.path(
        local_appdata,
        "dbsnR",
        "datasets"
      )

    } else {

      cache_dir <- file.path(
        tools::R_user_dir("dbsnR", which = "cache"),
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
  # 2. Find provinces available in cache
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

  available_provinces <- basename(province_dirs)


  # ---------------------------------------------------------------------------
  # 3. Select requested provinces
  # ---------------------------------------------------------------------------

  if (is.null(province)) {

    selected_provinces <- available_provinces

  } else {

    if (!is.character(province) || length(province) == 0L) {
      stop(
        "`province` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    province <- unique(toupper(province))

    missing_provinces <- setdiff(
      province,
      toupper(available_provinces)
    )

    if (length(missing_provinces) > 0L) {
      warning(
        "The following provinces were not found in the cache: ",
        paste(missing_provinces, collapse = ", "),
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

  province_dirs <- province_dirs[
    basename(province_dirs) %in% selected_provinces
  ]


  # ---------------------------------------------------------------------------
  # 4. Find File Geodatabases
  # ---------------------------------------------------------------------------

  gdb_paths <- unlist(
    lapply(province_dirs, function(x) {

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
    }),
    use.names = FALSE
  )

  gdb_paths <- sort(unique(gdb_paths))

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
  # 5. Build layer catalogue
  # ---------------------------------------------------------------------------

  gdb_catalogue <- lapply(
    gdb_paths,
    function(gdb_path) {

      province_code <- basename(dirname(gdb_path))

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
  # 6. Determine layers to merge
  # ---------------------------------------------------------------------------

  available_layers <- sort(
    unique(gdb_catalogue$layer)
  )

  merge_all <- is.null(layer)

  if (merge_all) {

    layers_to_merge <- available_layers

  } else {

    if (!is.character(layer) || length(layer) == 0L) {
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
        paste(missing_layers, collapse = ", "),
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


  # ---------------------------------------------------------------------------
  # 7. Internal function to merge one layer
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

          x <- sf::st_read(
            dsn = gdb_path,
            layer = layer_name,
            quiet = TRUE,
            stringsAsFactors = FALSE
          )

          # Skip empty layers
          if (nrow(x) == 0L) {

            if (!quiet) {
              message(
                "    empty, skipped"
              )
            }

            return(NULL)
          }

          if (add_source) {

            x$province <- rep(
              province_code,
              nrow(x)
            )

            x$source_gdb <- rep(
              basename(gdb_path),
              nrow(x)
            )
          }

          x
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


    # Remove failed reads

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
        function(x) {

          current_crs <- sf::st_crs(x)

          if (
            !is.na(current_crs) &&
            current_crs != reference_crs
          ) {

            x <- sf::st_transform(
              x,
              reference_crs
            )
          }

          x
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
  # 8. Merge selected layers
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
  # 9. Return result
  # ---------------------------------------------------------------------------

  if (
    !merge_all &&
    length(layer) == 1L &&
    length(result) == 1L
  ) {

    return(result[[1]])
  }

  result
}
