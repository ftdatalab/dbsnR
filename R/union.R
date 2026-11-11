#' Union DBSN spatial data
#'
#' Combines and optionally dissolves geometries from one or more sf objects.
#'
#' The input can be a single sf object or a list of sf objects.
#' When multiple sf objects are supplied, they are automatically transformed
#' to the CRS of the first layer before being combined.
#'
#' @param x An sf object or a list of sf objects.
#'
#' @param dissolve Logical. If TRUE, all geometries are dissolved into a
#'   single feature using `sf::st_union()`. If FALSE, the input features are
#'   combined without dissolving their geometries.
#'
#' @param quiet Logical. If FALSE, displays information about the union
#'   operation.
#'
#' @return
#' An sf object.
#'
#' When `dissolve = TRUE`, the result contains one feature representing the
#' geometric union of all input features.
#'
#' When `dissolve = FALSE`, individual features are retained.
#'
#' @details
#' When `x` is a list, all layers are transformed to the coordinate reference
#' system of the first layer before being combined.
#'
#' With `dissolve = FALSE`, layers should have compatible attribute
#' structures. Missing attributes are automatically filled with `NA`.
#'
#' @examples
#' \dontrun{
#' dbsn_union(
#'   forest
#' )
#'
#' dbsn_union(
#'   forest,
#'   dissolve = FALSE
#' )
#'
#' dbsn_union(
#'   list(
#'     forest,
#'     vegetation,
#'     agricultural_land
#'   )
#' )
#' }
#'
#' @export
dbsn_union <- function(
    x,
    dissolve = TRUE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate options
  # ---------------------------------------------------------------------------

  if (
    !is.logical(dissolve) ||
    length(dissolve) != 1L ||
    is.na(dissolve)
  ) {
    stop(
      "`dissolve` must be TRUE or FALSE.",
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
  # 2. Validate input
  # ---------------------------------------------------------------------------

  is_sf <- inherits(
    x,
    "sf"
  )

  is_sf_list <- is.list(x) &&
    length(x) > 0L &&
    all(
      vapply(
        x,
        function(z) inherits(z, "sf"),
        logical(1)
      )
    )

  if (!is_sf && !is_sf_list) {
    stop(
      "`x` must be an sf object or a list of sf objects.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Convert single sf object to list
  # ---------------------------------------------------------------------------

  if (is_sf) {
    layers <- list(x)
  } else {
    layers <- x
  }


  # ---------------------------------------------------------------------------
  # 4. Check CRS
  # ---------------------------------------------------------------------------

  layer_crs <- lapply(
    layers,
    sf::st_crs
  )

  missing_crs <- vapply(
    layer_crs,
    is.na,
    logical(1)
  )

  if (any(missing_crs)) {
    stop(
      "All input layers must have a valid CRS.",
      call. = FALSE
    )
  }

  target_crs <- layer_crs[[1]]


  # ---------------------------------------------------------------------------
  # 5. Transform layers to common CRS
  # ---------------------------------------------------------------------------

  for (i in seq_along(layers)) {

    if (sf::st_crs(layers[[i]]) != target_crs) {

      layers[[i]] <- sf::st_transform(
        layers[[i]],
        target_crs
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 6. Count input features
  # ---------------------------------------------------------------------------

  input_features <- sum(
    vapply(
      layers,
      nrow,
      integer(1)
    )
  )


  # ---------------------------------------------------------------------------
  # 7. Remove empty layers
  # ---------------------------------------------------------------------------

  non_empty <- vapply(
    layers,
    function(z) nrow(z) > 0L,
    logical(1)
  )

  layers <- layers[
    non_empty
  ]

  if (length(layers) == 0L) {
    stop(
      "The input contains no features.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Dissolved geometric union
  # ---------------------------------------------------------------------------

  if (dissolve) {

    geometries <- lapply(
      layers,
      sf::st_geometry
    )

    geometries <- do.call(
      c,
      geometries
    )

    union_geometry <- suppressWarnings(
      sf::st_union(
        geometries
      )
    )

    result <- sf::st_sf(
      geometry = union_geometry
    )
  }


  # ---------------------------------------------------------------------------
  # 9. Combine without dissolving
  # ---------------------------------------------------------------------------

  if (!dissolve) {

    all_columns <- unique(
      unlist(
        lapply(
          layers,
          function(z) {
            setdiff(
              names(z),
              attr(z, "sf_column")
            )
          }
        )
      )
    )

    for (i in seq_along(layers)) {

      sf_column <- attr(
        layers[[i]],
        "sf_column"
      )

      missing_columns <- setdiff(
        all_columns,
        names(layers[[i]])
      )

      if (length(missing_columns) > 0L) {

        for (column in missing_columns) {
          layers[[i]][[column]] <- NA
        }
      }

      attribute_columns <- setdiff(
        all_columns,
        sf_column
      )

      layers[[i]] <- layers[[i]][
        c(
          attribute_columns,
          sf_column
        )
      ]
    }

    result <- do.call(
      rbind,
      layers
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Reset row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 11. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Union of ",
      format(
        input_features,
        big.mark = ","
      ),
      " feature(s)."
    )

    if (dissolve) {

      message(
        "Geometries dissolved into 1 feature."
      )

    } else {

      message(
        "Geometries combined without dissolving."
      )

      message(
        "Result: ",
        format(
          nrow(result),
          big.mark = ","
        ),
        " feature(s)."
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 12. Return
  # ---------------------------------------------------------------------------

  result
}
