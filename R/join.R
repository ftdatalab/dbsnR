#' Spatial join of DBSN data
#'
#' Performs a spatial join between two sf objects.
#'
#' Attributes from `y` are added to the features of `x` according to a
#' spatial relationship. The geometry of `x` is preserved.
#'
#' If `x` and `y` use different coordinate reference systems, `y` is
#' automatically transformed to the CRS of `x`.
#'
#' @param x An sf object whose geometry and attributes are retained.
#'
#' @param y An sf object containing attributes to be joined to `x`.
#'
#' @param relation Spatial relationship used for the join. One of
#'   `"intersects"`, `"within"`, `"contains"`, `"touches"`, or `"overlaps"`.
#'
#' @param left Logical. If TRUE, all features of `x` are retained.
#'   If FALSE, only features of `x` having a spatial match in `y`
#'   are returned.
#'
#' @param largest Logical. If TRUE, when a feature of `x` matches multiple
#'   features of `y`, only the feature of `y` having the largest spatial
#'   overlap is retained.
#'
#' @param suffix Character vector of length two specifying suffixes used
#'   when `x` and `y` contain columns with identical names.
#'
#' @param quiet Logical. If FALSE, displays information about the join.
#'
#' @return
#' An sf object containing the geometry of `x` and attributes joined
#' from `y`.
#'
#' @examples
#' \dontrun{
#' dbsn_join(
#'   sites,
#'   municipalities
#' )
#'
#' dbsn_join(
#'   sites,
#'   municipalities,
#'   relation = "within"
#' )
#'
#' dbsn_join(
#'   roads,
#'   forest,
#'   relation = "intersects",
#'   largest = TRUE
#' )
#' }
#'
#' @export
dbsn_join <- function(
    x,
    y,
    relation = c(
      "intersects",
      "within",
      "contains",
      "touches",
      "overlaps"
    ),
    left = TRUE,
    largest = FALSE,
    suffix = c(".x", ".y"),
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate relation
  # ---------------------------------------------------------------------------

  relation <- match.arg(relation)


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

  if (
    !is.logical(left) ||
    length(left) != 1L ||
    is.na(left)
  ) {
    stop(
      "`left` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(largest) ||
    length(largest) != 1L ||
    is.na(largest)
  ) {
    stop(
      "`largest` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.character(suffix) ||
    length(suffix) != 2L ||
    anyNA(suffix)
  ) {
    stop(
      "`suffix` must be a character vector of length two.",
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
  # 3. Validate data
  # ---------------------------------------------------------------------------

  if (nrow(x) == 0L) {

    if (!quiet) {
      message("`x` contains no features.")
    }

    return(x)
  }

  if (nrow(y) == 0L) {

    if (!quiet) {
      message("`y` contains no features.")
    }

    if (!left) {
      return(
        x[FALSE, , drop = FALSE]
      )
    }
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
  # 6. Select spatial predicate
  # ---------------------------------------------------------------------------

  join_function <- switch(
    relation,
    intersects = sf::st_intersects,
    within     = sf::st_within,
    contains   = sf::st_contains,
    touches    = sf::st_touches,
    overlaps   = sf::st_overlaps
  )


  # ---------------------------------------------------------------------------
  # 7. Spatial join
  # ---------------------------------------------------------------------------

  result <- suppressWarnings(
    sf::st_join(
      x,
      y_work,
      join = join_function,
      left = left,
      largest = largest,
      suffix = suffix
    )
  )


  # ---------------------------------------------------------------------------
  # 8. Reset row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Joined ",
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
      "Relation: ",
      relation,
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
  # 10. Return
  # ---------------------------------------------------------------------------

  result
}
