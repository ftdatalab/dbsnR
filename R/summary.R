#' Summarise DBSN spatial data
#'
#' Creates a compact summary of a DBSN sf object or a named list
#' of sf objects.
#'
#' The summary includes the number of features, geometry type,
#' number of attributes, provinces, coordinate reference system,
#' EPSG code, and bounding box.
#'
#' @param x An sf object or a named list of sf objects.
#'
#' @param layer Optional name used when `x` is a single sf object.
#'   If NULL, `"dbsn"` is used.
#'
#' @param quiet Logical. If FALSE, displays a short summary message.
#'
#' @return A data.frame containing one row per spatial layer.
#'
#' @examples
#' \dontrun{
#' dbsn_summary(loc_sg)
#'
#' dbsn_summary(
#'   loc_sg,
#'   layer = "loc_sg"
#' )
#'
#' dbsn_summary(territorio)
#' }
#'
#' @export
dbsn_summary <- function(
    x,
    layer = NULL,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Internal summary function
  # ---------------------------------------------------------------------------

  summarise_one <- function(obj, layer_name) {

    if (!inherits(obj, "sf")) {
      stop(
        "All objects must be sf objects.",
        call. = FALSE
      )
    }

    # -------------------------------------------------------------------------
    # Features
    # -------------------------------------------------------------------------

    n_features <- nrow(obj)


    # -------------------------------------------------------------------------
    # Attributes
    # -------------------------------------------------------------------------

    geometry_column <- attr(
      obj,
      "sf_column"
    )

    attribute_names <- setdiff(
      names(obj),
      geometry_column
    )

    n_attributes <- length(
      attribute_names
    )


    # -------------------------------------------------------------------------
    # Geometry type
    # -------------------------------------------------------------------------

    if (n_features > 0L) {

      geometry_types <- unique(
        as.character(
          sf::st_geometry_type(obj)
        )
      )

      geometry_type <- paste(
        geometry_types,
        collapse = ", "
      )

    } else {

      geometry_type <- NA_character_
    }


    # -------------------------------------------------------------------------
    # Dimensions
    # -------------------------------------------------------------------------

    if (n_features > 0L) {

      geometry_dimension <- tryCatch(
        {

          coords <- sf::st_coordinates(
            sf::st_geometry(obj)[1]
          )

          if ("Z" %in% colnames(coords)) {
            "XYZ"
          } else {
            "XY"
          }
        },
        error = function(e) {
          NA_character_
        }
      )

    } else {

      geometry_dimension <- NA_character_
    }


    # -------------------------------------------------------------------------
    # Provinces
    # -------------------------------------------------------------------------

    if ("province" %in% names(obj)) {

      province_values <- sort(
        unique(
          as.character(
            obj$province[
              !is.na(obj$province)
            ]
          )
        )
      )

      n_provinces <- length(
        province_values
      )

      provinces <- paste(
        province_values,
        collapse = ", "
      )

    } else {

      n_provinces <- NA_integer_
      provinces <- NA_character_
    }


    # -------------------------------------------------------------------------
    # CRS
    # -------------------------------------------------------------------------

    crs <- sf::st_crs(obj)

    epsg <- if (
      !is.null(crs$epsg) &&
      !is.na(crs$epsg)
    ) {
      as.integer(crs$epsg)
    } else {
      NA_integer_
    }

    crs_name <- if (
      !is.null(crs$Name) &&
      !is.na(crs$Name)
    ) {
      crs$Name
    } else {
      NA_character_
    }


    # -------------------------------------------------------------------------
    # Bounding box
    # -------------------------------------------------------------------------

    if (n_features > 0L) {

      bbox <- sf::st_bbox(obj)

      xmin <- unname(
        bbox["xmin"]
      )

      ymin <- unname(
        bbox["ymin"]
      )

      xmax <- unname(
        bbox["xmax"]
      )

      ymax <- unname(
        bbox["ymax"]
      )

    } else {

      xmin <- NA_real_
      ymin <- NA_real_
      xmax <- NA_real_
      ymax <- NA_real_
    }


    # -------------------------------------------------------------------------
    # Result
    # -------------------------------------------------------------------------

    data.frame(
      layer = layer_name,
      features = n_features,
      attributes = n_attributes,
      geometry_type = geometry_type,
      dimension = geometry_dimension,
      n_provinces = n_provinces,
      provinces = provinces,
      epsg = epsg,
      crs = crs_name,
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      stringsAsFactors = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Single sf object
  # ---------------------------------------------------------------------------

  if (inherits(x, "sf")) {

    if (is.null(layer)) {
      layer <- "dbsn"
    }

    if (
      !is.character(layer) ||
      length(layer) != 1L ||
      !nzchar(layer)
    ) {
      stop(
        "`layer` must be NULL or a single non-empty character string.",
        call. = FALSE
      )
    }

    result <- summarise_one(
      x,
      layer
    )

    if (!quiet) {

      message(
        "Layer: ",
        layer
      )

      message(
        "Features: ",
        format(
          result$features,
          big.mark = ","
        )
      )

      message(
        "Geometry: ",
        result$geometry_type
      )

      if (!is.na(result$n_provinces)) {

        message(
          "Provinces: ",
          result$provinces
        )
      }

      if (!is.na(result$epsg)) {

        message(
          "CRS: EPSG:",
          result$epsg
        )
      }
    }

    return(result)
  }


  # ---------------------------------------------------------------------------
  # 3. Validate list
  # ---------------------------------------------------------------------------

  if (!is.list(x)) {

    stop(
      "`x` must be an sf object or a named list of sf objects.",
      call. = FALSE
    )
  }

  if (length(x) == 0L) {

    stop(
      "`x` contains no layers.",
      call. = FALSE
    )
  }

  if (
    is.null(names(x)) ||
    any(is.na(names(x))) ||
    any(!nzchar(names(x)))
  ) {

    stop(
      "`x` must be a named list.",
      call. = FALSE
    )
  }

  valid_sf <- vapply(
    x,
    function(obj) {
      inherits(obj, "sf")
    },
    logical(1)
  )

  if (!all(valid_sf)) {

    invalid <- names(x)[
      !valid_sf
    ]

    stop(
      "The following elements are not sf objects: ",
      paste(
        invalid,
        collapse = ", "
      ),
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Summarise list
  # ---------------------------------------------------------------------------

  result <- lapply(
    seq_along(x),
    function(i) {

      summarise_one(
        x[[i]],
        names(x)[i]
      )
    }
  )

  result <- do.call(
    rbind,
    result
  )

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 5. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "DBSN summary: ",
      nrow(result),
      " layer(s)"
    )

    message(
      "Total features: ",
      format(
        sum(result$features),
        big.mark = ","
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Return
  # ---------------------------------------------------------------------------

  result
}
