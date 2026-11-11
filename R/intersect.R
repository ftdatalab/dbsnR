#' Intersect DBSN spatial data
#'
#' Finds spatial intersections between two sf objects.
#'
#' The function can either select complete features from `x` that intersect
#' `y`, or geometrically clip features from `x` to their actual intersection
#' with `y`.
#'
#' If `x` and `y` use different coordinate reference systems, `y` is
#' automatically transformed to the CRS of `x`.
#'
#' @param x An sf object containing the features to be analysed.
#'
#' @param y An sf object containing the features used for the spatial
#'   intersection.
#'
#' @param method Intersection method. Either `"select"` or `"clip"`.
#'   `"select"` keeps complete features from `x` that intersect `y`.
#'   `"clip"` returns the actual geometric intersection between `x` and `y`.
#'
#' @param keep_y Logical. If TRUE, attributes from `y` are retained.
#'   When `method = "select"` and `keep_y = TRUE`, spatially joined
#'   attributes from `y` are returned and a feature from `x` may therefore
#'   appear more than once if it intersects multiple features in `y`.
#'
#' @param quiet Logical. If FALSE, displays information about the number
#'   of features before and after the intersection.
#'
#' @return
#' An sf object containing the intersecting features.
#'
#' With `method = "select"`, complete geometries from `x` are retained.
#' With `method = "clip"`, geometries are cut to their actual intersection
#' with `y`.
#'
#' @examples
#' \dontrun{
#' dbsn_intersect(
#'   sites,
#'   forest
#' )
#'
#' dbsn_intersect(
#'   roads,
#'   study_area,
#'   method = "clip"
#' )
#'
#' dbsn_intersect(
#'   roads,
#'   forest,
#'   method = "clip",
#'   keep_y = TRUE
#' )
#' }
#'
#' @export
dbsn_intersect <- function(
    x,
    y,
    method = c("select", "clip"),
    keep_y = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate method
  # ---------------------------------------------------------------------------

  method <- match.arg(method)


  # ---------------------------------------------------------------------------
  # 2. Validate input
  # ---------------------------------------------------------------------------

  if (!inherits(x, "sf")) {
    stop(
      "`x` must be an sf object.",
      call. = FALSE
    )
  }

  if (!inherits(y, "sf")) {
    stop(
      "`y` must be an sf object.",
      call. = FALSE
    )
  }

  if (!is.logical(keep_y) ||
      length(keep_y) != 1L ||
      is.na(keep_y)) {
    stop(
      "`keep_y` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(quiet) ||
      length(quiet) != 1L ||
      is.na(quiet)) {
    stop(
      "`quiet` must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Handle empty inputs
  # ---------------------------------------------------------------------------

  if (nrow(x) == 0L) {

    if (!quiet) {
      message("`x` contains no features.")
    }

    return(x)
  }

  if (nrow(y) == 0L) {

    if (!quiet) {
      message("`y` contains no features. No intersections found.")
    }

    return(
      x[FALSE, , drop = FALSE]
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Check CRS
  # ---------------------------------------------------------------------------

  x_crs <- sf::st_crs(x)
  y_crs <- sf::st_crs(y)

  if (is.na(x_crs)) {
    stop(
      "`x` must have a valid CRS.",
      call. = FALSE
    )
  }

  if (is.na(y_crs)) {
    stop(
      "`y` must have a valid CRS.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Transform y to x CRS
  # ---------------------------------------------------------------------------

  y_work <- y

  if (y_crs != x_crs) {

    y_work <- sf::st_transform(
      y_work,
      x_crs
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Select intersecting features
  # ---------------------------------------------------------------------------

  if (method == "select") {

    if (!keep_y) {

      idx <- lengths(
        sf::st_intersects(
          x,
          y_work
        )
      ) > 0L

      result <- x[
        idx,
        ,
        drop = FALSE
      ]

    } else {

      result <- suppressWarnings(
        sf::st_join(
          x,
          y_work,
          join = sf::st_intersects,
          left = FALSE,
          largest = FALSE
        )
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 7. Calculate geometric intersection
  # ---------------------------------------------------------------------------

  if (method == "clip") {

    if (keep_y) {

      result <- suppressWarnings(
        sf::st_intersection(
          x,
          y_work
        )
      )

    } else {

      y_geometry <- sf::st_geometry(
        y_work
      )

      y_geometry <- sf::st_union(
        y_geometry
      )

      result <- suppressWarnings(
        sf::st_intersection(
          x,
          y_geometry
        )
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 8. Reset row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Intersected ",
      format(
        nrow(x),
        big.mark = ","
      ),
      " feature(s) with ",
      format(
        nrow(y),
        big.mark = ","
      ),
      " feature(s)."
    )

    message(
      "Result: ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " feature(s)."
    )

    message(
      "Method: ",
      method,
      "."
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return
  # ---------------------------------------------------------------------------

  result
}
