#' Calculate distance to the nearest feature
#'
#' Calculates the minimum distance from each feature in `x` to the nearest
#' feature in `y`.
#'
#' Both objects must have a coordinate reference system. If their CRS differ,
#' `y` is automatically transformed to the CRS of `x`.
#'
#' @param x An `sf` object containing the features for which distances
#'   are calculated.
#' @param y An `sf` object containing the reference features.
#'
#' @return The original `x` object with three additional columns:
#'   \itemize{
#'     \item `nearest_id`: row index of the nearest feature in `y`.
#'     \item `distance_m`: minimum distance to `y` in metres.
#'     \item `distance_km`: minimum distance to `y` in kilometres.
#'   }
#'
#' @details
#' Distances are calculated using `sf::st_distance()`.
#'
#' A projected coordinate reference system is required so that distances
#' can be interpreted in metres.
#'
#' If a feature in `x` intersects or touches a feature in `y`, the minimum
#' distance is zero.
#'
#' @examples
#' \dontrun{
#'
#' result <- dbsn_distance(
#'   x = loc_sg,
#'   y = road
#' )
#'
#' result[, c(
#'   "nearest_id",
#'   "distance_m",
#'   "distance_km"
#' )]
#'
#' }
#'
#' @export

dbsn_distance <- function(x, y) {

  # -----------------------------------------------------------------------
  # Input validation
  # -----------------------------------------------------------------------

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

  if (nrow(x) == 0) {
    stop(
      "`x` contains no features.",
      call. = FALSE
    )
  }

  if (nrow(y) == 0) {
    stop(
      "`y` contains no features.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # CRS validation
  # -----------------------------------------------------------------------

  if (is.na(sf::st_crs(x))) {
    stop(
      "`x` has no CRS.",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(y))) {
    stop(
      "`y` has no CRS.",
      call. = FALSE
    )
  }

  if (sf::st_is_longlat(x)) {
    stop(
      "`x` uses a geographic CRS (longitude/latitude). ",
      "Please transform it to a projected CRS.",
      call. = FALSE
    )
  }

  if (sf::st_crs(y) != sf::st_crs(x)) {

    y <- sf::st_transform(
      y,
      sf::st_crs(x)
    )

    message(
      "`y` transformed to the CRS of `x`."
    )
  }


  # -----------------------------------------------------------------------
  # Empty geometries
  # -----------------------------------------------------------------------

  empty_x <- sf::st_is_empty(x)

  if (any(empty_x)) {

    n_empty <- sum(empty_x)

    x <- x[
      !empty_x,
      ,
      drop = FALSE
    ]

    message(
      n_empty,
      if (n_empty == 1) {
        " empty geometry removed from `x`."
      } else {
        " empty geometries removed from `x`."
      }
    )
  }

  if (nrow(x) == 0) {
    stop(
      "No geometries remain in `x` after removing empty geometries.",
      call. = FALSE
    )
  }


  empty_y <- sf::st_is_empty(y)

  if (any(empty_y)) {

    n_empty <- sum(empty_y)

    y <- y[
      !empty_y,
      ,
      drop = FALSE
    ]

    message(
      n_empty,
      if (n_empty == 1) {
        " empty geometry removed from `y`."
      } else {
        " empty geometries removed from `y`."
      }
    )
  }

  if (nrow(y) == 0) {
    stop(
      "No geometries remain in `y` after removing empty geometries.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Geometry validation
  # -----------------------------------------------------------------------

  valid_x <- sf::st_is_valid(x)

  invalid_x <- is.na(valid_x) | !valid_x

  if (any(invalid_x)) {

    n_invalid <- sum(invalid_x)

    repaired <- sf::st_make_valid(
      x[
        invalid_x,
        ,
        drop = FALSE
      ]
    )

    sf::st_geometry(x)[invalid_x] <-
      sf::st_geometry(repaired)

    message(
      n_invalid,
      if (n_invalid == 1) {
        " invalid geometry repaired in `x`."
      } else {
        " invalid geometries repaired in `x`."
      }
    )
  }


  valid_y <- sf::st_is_valid(y)

  invalid_y <- is.na(valid_y) | !valid_y

  if (any(invalid_y)) {

    n_invalid <- sum(invalid_y)

    repaired <- sf::st_make_valid(
      y[
        invalid_y,
        ,
        drop = FALSE
      ]
    )

    sf::st_geometry(y)[invalid_y] <-
      sf::st_geometry(repaired)

    message(
      n_invalid,
      if (n_invalid == 1) {
        " invalid geometry repaired in `y`."
      } else {
        " invalid geometries repaired in `y`."
      }
    )
  }


  # -----------------------------------------------------------------------
  # Final geometry validation
  # -----------------------------------------------------------------------

  valid_x <- sf::st_is_valid(x)
  valid_y <- sf::st_is_valid(y)

  if (any(is.na(valid_x) | !valid_x)) {
    stop(
      "Some geometries in `x` remain invalid after attempting repair.",
      call. = FALSE
    )
  }

  if (any(is.na(valid_y) | !valid_y)) {
    stop(
      "Some geometries in `y` remain invalid after attempting repair.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Calculate distance matrix
  # -----------------------------------------------------------------------

  distances <- sf::st_distance(
    x,
    y
  )


  # -----------------------------------------------------------------------
  # Find nearest feature
  # -----------------------------------------------------------------------

  nearest_id <- max.col(
    -as.matrix(distances),
    ties.method = "first"
  )


  # -----------------------------------------------------------------------
  # Extract minimum distance
  # -----------------------------------------------------------------------

  distance_m <- vapply(
    seq_len(nrow(x)),
    function(i) {

      as.numeric(
        distances[
          i,
          nearest_id[i]
        ]
      )
    },
    numeric(1)
  )


  # -----------------------------------------------------------------------
  # Add results
  # -----------------------------------------------------------------------

  x$nearest_id <- nearest_id

  x$distance_m <- distance_m

  x$distance_km <- distance_m / 1000


  # -----------------------------------------------------------------------
  # Message
  # -----------------------------------------------------------------------

  message(
    "Calculated nearest distance for ",
    nrow(x),
    " feature(s) to ",
    nrow(y),
    " reference feature(s)."
  )


  # -----------------------------------------------------------------------
  # Return
  # -----------------------------------------------------------------------

  x
}
