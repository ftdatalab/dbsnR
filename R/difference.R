#' Calculate the spatial difference between DBSN layers
#'
#' Subtracts the geometries of one sf object from another.
#'
#' The geometries in `y` are combined into a single subtraction geometry
#' and removed from each feature of `x`. The attributes and coordinate
#' reference system of `x` are preserved.
#'
#' If `x` and `y` use different coordinate reference systems, `y` is
#' automatically transformed to the CRS of `x`.
#'
#' @param x An sf object containing the features from which geometries
#'   will be subtracted.
#'
#' @param y An sf object containing the geometries to subtract from `x`.
#'
#' @param keep_empty Logical. If TRUE, features from `x` that become empty
#'   after the difference operation are retained as empty geometries.
#'   If FALSE, empty geometries are removed.
#'
#' @param quiet Logical. If FALSE, displays information about the spatial
#'   difference operation.
#'
#' @return
#' An sf object containing the parts of `x` that are not covered by `y`.
#' The attributes and CRS of `x` are preserved.
#'
#' @details
#' All geometries in `y` are combined using `sf::st_union()` before being
#' subtracted from `x`.
#'
#' A feature of `x` may:
#' \itemize{
#'   \item remain unchanged if it does not intersect `y`;
#'   \item be partially cut if it partially intersects `y`;
#'   \item become empty if it is completely covered by `y`.
#' }
#'
#' By default, features with empty geometries are removed from the result.
#'
#' @examples
#' \dontrun{
#' dbsn_difference(
#'   study_area,
#'   buildings
#' )
#'
#' dbsn_difference(
#'   accessible_area,
#'   rivers
#' )
#'
#' dbsn_difference(
#'   study_area,
#'   excluded_areas,
#'   keep_empty = TRUE
#' )
#' }
#'
#' @export
dbsn_difference <- function(
    x,
    y,
    keep_empty = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate input
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

  if (
    !is.logical(keep_empty) ||
    length(keep_empty) != 1L ||
    is.na(keep_empty)
  ) {
    stop(
      "`keep_empty` must be TRUE or FALSE.",
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
  # 2. Handle empty inputs
  # ---------------------------------------------------------------------------

  if (nrow(x) == 0L) {

    if (!quiet) {
      message("`x` contains no features.")
    }

    return(x)
  }

  if (nrow(y) == 0L) {

    if (!quiet) {
      message(
        "`y` contains no features. Returning `x` unchanged."
      )
    }

    return(x)
  }


  # ---------------------------------------------------------------------------
  # 3. Check CRS
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
  # 4. Transform y to x CRS
  # ---------------------------------------------------------------------------

  y_work <- y

  if (y_crs != x_crs) {

    y_work <- sf::st_transform(
      y_work,
      x_crs
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Combine subtraction geometries
  # ---------------------------------------------------------------------------

  y_geometry <- sf::st_geometry(
    y_work
  )

  y_union <- sf::st_union(
    y_geometry
  )


  # ---------------------------------------------------------------------------
  # 6. Calculate spatial difference
  # ---------------------------------------------------------------------------

  result <- suppressWarnings(
    sf::st_difference(
      x,
      y_union
    )
  )


  # ---------------------------------------------------------------------------
  # 7. Identify empty geometries
  # ---------------------------------------------------------------------------

  empty_geometry <- sf::st_is_empty(
    result
  )

  empty_count <- sum(
    empty_geometry
  )


  # ---------------------------------------------------------------------------
  # 8. Remove empty geometries if requested
  # ---------------------------------------------------------------------------

  if (!keep_empty && any(empty_geometry)) {

    result <- result[
      !empty_geometry,
      ,
      drop = FALSE
    ]
  }


  # ---------------------------------------------------------------------------
  # 9. Reset row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 10. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Subtracted ",
      format(
        nrow(y),
        big.mark = ","
      ),
      " feature(s) from ",
      format(
        nrow(x),
        big.mark = ","
      ),
      " feature(s)."
    )

    message(
      "Empty geometries created: ",
      format(
        empty_count,
        big.mark = ","
      ),
      "."
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


  # ---------------------------------------------------------------------------
  # 11. Return
  # ---------------------------------------------------------------------------

  result
}
