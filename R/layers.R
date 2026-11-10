#' List DBSN layers available in the local cache
#'
#' Inspects the File Geodatabases stored in the dbsnR cache and returns
#' a catalogue of the available layers.
#'
#' @param province Optional character vector containing province codes,
#'   for example `c("RI", "AQ", "RM")`. If NULL, all provinces available
#'   in the cache are inspected.
#'
#' @param cache_dir Optional path to the dbsnR datasets cache.
#'   If NULL, the default dbsnR cache is used.
#'
#' @param details Logical. If TRUE, returns one row for every
#'   layer/province combination. If FALSE, returns one row per layer.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return A data.frame containing the available DBSN layers.
#'
#' @export
dbsn_layers <- function(
    province = NULL,
    cache_dir = NULL,
    details = FALSE,
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
  # 2. Find province directories
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
  # 3. Select provinces
  # ---------------------------------------------------------------------------

  if (!is.null(province)) {

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

    province_dirs <- province_dirs[
      toupper(basename(province_dirs)) %in% province
    ]
  }

  if (length(province_dirs) == 0L) {
    stop(
      "None of the requested provinces is available in the cache.",
      call. = FALSE
    )
  }


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
  # 5. Read layer catalogues
  # ---------------------------------------------------------------------------

  catalogue <- lapply(
    gdb_paths,
    function(gdb_path) {

      province_code <- basename(dirname(gdb_path))

      if (!quiet) {
        message("  - ", province_code)
      }

      layers <- tryCatch(

        sf::st_layers(gdb_path),

        error = function(e) {

          warning(
            "Unable to inspect geodatabase for province ",
            province_code,
            ": ",
            conditionMessage(e),
            call. = FALSE
          )

          NULL
        }
      )

      if (is.null(layers) || length(layers$name) == 0L) {
        return(NULL)
      }


      # Geometry type is normally available from st_layers()
      geometry_type <- if (
        "geomtype" %in% names(layers)
      ) {

        as.character(layers$geomtype)

      } else {

        rep(
          NA_character_,
          length(layers$name)
        )
      }


      data.frame(
        province = province_code,
        layer = layers$name,
        geometry_type = geometry_type,
        gdb = basename(gdb_path),
        stringsAsFactors = FALSE
      )
    }
  )


  # ---------------------------------------------------------------------------
  # 6. Remove unreadable geodatabases
  # ---------------------------------------------------------------------------

  catalogue <- Filter(
    Negate(is.null),
    catalogue
  )

  if (length(catalogue) == 0L) {
    stop(
      "No readable layers were found.",
      call. = FALSE
    )
  }

  catalogue <- do.call(
    rbind,
    catalogue
  )

  rownames(catalogue) <- NULL


  # ---------------------------------------------------------------------------
  # 7. Detailed output
  # ---------------------------------------------------------------------------

  if (details) {

    catalogue <- catalogue[
      order(
        catalogue$layer,
        catalogue$province
      ),
      ,
      drop = FALSE
    ]

    rownames(catalogue) <- NULL

    return(catalogue)
  }


  # ---------------------------------------------------------------------------
  # 8. Summary by layer
  # ---------------------------------------------------------------------------

  layer_names <- sort(
    unique(catalogue$layer)
  )

  result <- lapply(
    layer_names,
    function(layer_name) {

      x <- catalogue[
        catalogue$layer == layer_name,
        ,
        drop = FALSE
      ]

      provinces <- sort(
        unique(x$province)
      )

      geometry_types <- unique(
        x$geometry_type[
          !is.na(x$geometry_type)
        ]
      )

      data.frame(
        layer = layer_name,

        geometry_type = paste(
          geometry_types,
          collapse = ", "
        ),

        n_provinces = length(provinces),

        provinces = paste(
          provinces,
          collapse = ", "
        ),

        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    result
  )

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Return catalogue
  # ---------------------------------------------------------------------------

  result
}
