#' Show information about a DBSN layer
#'
#' Provides information about a DBSN layer stored in the local cache
#' or about an sf object already loaded in memory.
#'
#' When `x` is a character string, the function inspects the requested
#' layer in the provincial File Geodatabases stored in the dbsnR cache.
#'
#' When `x` is an sf object, the function summarizes the object directly.
#'
#' @param x A DBSN layer name or an sf object.
#'
#' @param province Optional character vector containing province codes,
#'   for example `c("RI", "AQ", "RM")`.
#'   Used only when `x` is a layer name.
#'
#' @param cache_dir Optional path to the dbsnR datasets cache.
#'   If NULL, the default dbsnR cache is used.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return A data.frame containing information about the layer.
#'
#' @export
dbsn_info <- function(
    x,
    province = NULL,
    cache_dir = NULL,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. sf object already loaded
  # ---------------------------------------------------------------------------

  if (inherits(x, "sf")) {

    geometry_type <- unique(
      as.character(
        sf::st_geometry_type(x)
      )
    )

    crs <- sf::st_crs(x)

    crs_name <- if (!is.null(crs$Name)) {
      crs$Name
    } else {
      NA_character_
    }

    epsg <- crs$epsg

    if ("province" %in% names(x)) {

      provinces <- sort(
        unique(
          as.character(x$province)
        )
      )

      province_counts <- table(
        x$province
      )

      result <- data.frame(
        province = names(province_counts),
        features = as.integer(province_counts),
        stringsAsFactors = FALSE
      )

    } else {

      provinces <- character(0)

      result <- data.frame(
        province = NA_character_,
        features = nrow(x),
        stringsAsFactors = FALSE
      )
    }

    attr(result, "total_features") <- nrow(x)

    attr(result, "geometry_type") <- paste(
      geometry_type,
      collapse = ", "
    )

    attr(result, "crs") <- crs_name

    attr(result, "epsg") <- epsg

    attr(result, "provinces") <- provinces

    if (!quiet) {

      message(
        "Features: ",
        format(
          nrow(x),
          big.mark = ","
        )
      )

      message(
        "Geometry: ",
        paste(
          geometry_type,
          collapse = ", "
        )
      )

      message(
        "CRS: ",
        crs_name
      )

      if (!is.null(epsg) && !is.na(epsg)) {
        message(
          "EPSG: ",
          epsg
        )
      }

      if (length(provinces) > 0L) {
        message(
          "Provinces: ",
          paste(
            provinces,
            collapse = ", "
          )
        )
      }
    }

    return(result)
  }


  # ---------------------------------------------------------------------------
  # 2. Validate layer name
  # ---------------------------------------------------------------------------

  if (
    !is.character(x) ||
    length(x) != 1L ||
    !nzchar(x)
  ) {

    stop(
      "`x` must be a DBSN layer name or an sf object.",
      call. = FALSE
    )
  }

  layer_name <- x


  # ---------------------------------------------------------------------------
  # 3. Identify cache directory
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
  # 4. Find province directories
  # ---------------------------------------------------------------------------

  province_dirs <- list.dirs(
    cache_dir,
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
  # 5. Select provinces
  # ---------------------------------------------------------------------------

  if (!is.null(province)) {

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

    province_dirs <- province_dirs[
      toupper(
        basename(province_dirs)
      ) %in% province
    ]
  }

  if (length(province_dirs) == 0L) {

    stop(
      "None of the requested provinces is available in the cache.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Find File Geodatabases
  # ---------------------------------------------------------------------------

  gdb_paths <- unlist(
    lapply(
      province_dirs,
      function(path) {

        dirs <- list.dirs(
          path,
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
      "No File Geodatabases were found.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Inspect requested layer
  # ---------------------------------------------------------------------------

  result <- vector(
    "list",
    length(gdb_paths)
  )

  for (i in seq_along(gdb_paths)) {

    gdb_path <- gdb_paths[i]

    province_code <- basename(
      dirname(gdb_path)
    )

    layers <- tryCatch(

      sf::st_layers(gdb_path),

      error = function(e) NULL
    )

    if (
      is.null(layers) ||
      !(layer_name %in% layers$name)
    ) {

      result[[i]] <- data.frame(
        province = province_code,
        features = NA_integer_,
        status = "not available",
        geometry_type = NA_character_,
        stringsAsFactors = FALSE
      )

      next
    }


    # -------------------------------------------------------------------------
    # Read layer
    # -------------------------------------------------------------------------

    if (!quiet) {

      message(
        "Inspecting ",
        layer_name,
        " - ",
        province_code
      )
    }

    layer <- tryCatch(

      sf::st_read(
        gdb_path,
        layer = layer_name,
        quiet = TRUE,
        stringsAsFactors = FALSE
      ),

      error = function(e) NULL
    )


    # -------------------------------------------------------------------------
    # Read failure
    # -------------------------------------------------------------------------

    if (is.null(layer)) {

      result[[i]] <- data.frame(
        province = province_code,
        features = NA_integer_,
        status = "read error",
        geometry_type = NA_character_,
        stringsAsFactors = FALSE
      )

      next
    }


    # -------------------------------------------------------------------------
    # Empty layer
    # -------------------------------------------------------------------------

    if (nrow(layer) == 0L) {

      result[[i]] <- data.frame(
        province = province_code,
        features = 0L,
        status = "empty",
        geometry_type = NA_character_,
        stringsAsFactors = FALSE
      )

      next
    }


    # -------------------------------------------------------------------------
    # Layer with data
    # -------------------------------------------------------------------------

    geometry_type <- unique(
      as.character(
        sf::st_geometry_type(layer)
      )
    )

    result[[i]] <- data.frame(
      province = province_code,
      features = nrow(layer),
      status = "available",
      geometry_type = paste(
        geometry_type,
        collapse = ", "
      ),
      stringsAsFactors = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Combine results
  # ---------------------------------------------------------------------------

  result <- do.call(
    rbind,
    result
  )

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Summary
  # ---------------------------------------------------------------------------

  total_features <- sum(
    result$features,
    na.rm = TRUE
  )

  attr(
    result,
    "layer"
  ) <- layer_name

  attr(
    result,
    "total_features"
  ) <- total_features


  if (!quiet) {

    message("")
    message(
      "Layer: ",
      layer_name
    )

    message(
      "Total features: ",
      format(
        total_features,
        big.mark = ","
      )
    )

    message(
      "Provinces with data: ",
      sum(
        result$status == "available"
      )
    )

    message(
      "Empty provinces: ",
      sum(
        result$status == "empty"
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return result
  # ---------------------------------------------------------------------------

  result
}
