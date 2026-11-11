#' Find nearest DBSN feature
#'
#' Finds the nearest feature in one sf object for each feature in another
#' sf object and calculates the corresponding distance in metres.
#'
#' The geometry and attributes of `x` are preserved. Two columns are added
#' to the result: `nearest_id`, identifying the nearest feature in `y`, and
#' `distance_m`, containing the distance in metres.
#'
#' If `x` and `y` use different coordinate reference systems, `y` is
#' automatically transformed to the CRS of `x`.
#'
#' When the input data use geographic coordinates, nearest-feature
#' calculations are performed in EPSG:6875 (RDN2008 / Italy zone (N-E))
#' so that distances are expressed in metres.
#'
#' @param x An sf object containing the features from which nearest
#'   neighbours are searched.
#'
#' @param y An sf object containing the candidate nearest features.
#'
#' @param id_y Optional character string specifying a column in `y` to use
#'   as the identifier of the nearest feature. When NULL, the row position
#'   of the nearest feature in `y` is used.
#'
#' @param max_distance Maximum allowed distance in metres. Nearest features
#'   farther than this distance are returned with `nearest_id = NA` and
#'   `distance_m = NA`. Default is Inf.
#'
#' @param quiet Logical. If FALSE, displays information about the nearest
#'   feature calculation.
#'
#' @return
#' An sf object with the same features and geometry as `x`, with two
#' additional columns:
#' \itemize{
#'   \item `nearest_id`: identifier of the nearest feature in `y`;
#'   \item `distance_m`: distance to the nearest feature in metres.
#' }
#'
#' @examples
#' \dontrun{
#' dbsn_nearest(
#'   sites,
#'   roads
#' )
#'
#' dbsn_nearest(
#'   sites,
#'   loc_sg,
#'   id_y = "loc_sg_top"
#' )
#'
#' dbsn_nearest(
#'   sites,
#'   roads,
#'   max_distance = 5000
#' )
#' }
#'
#' @export
dbsn_nearest <- function(
    x,
    y,
    id_y = NULL,
    max_distance = Inf,
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

  if (nrow(x) == 0L) {
    stop(
      "`x` contains no features.",
      call. = FALSE
    )
  }

  if (nrow(y) == 0L) {
    stop(
      "`y` contains no features.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Validate CRS
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
  # 3. Validate id_y
  # ---------------------------------------------------------------------------

  if (!is.null(id_y)) {

    if (
      !is.character(id_y) ||
      length(id_y) != 1L ||
      is.na(id_y) ||
      !nzchar(id_y)
    ) {
      stop(
        "`id_y` must be NULL or a single column name.",
        call. = FALSE
      )
    }

    if (!id_y %in% names(y)) {
      stop(
        "Column `",
        id_y,
        "` was not found in `y`.",
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 4. Validate maximum distance
  # ---------------------------------------------------------------------------

  if (
    !is.numeric(max_distance) ||
    length(max_distance) != 1L ||
    is.na(max_distance) ||
    max_distance < 0
  ) {
    stop(
      "`max_distance` must be a non-negative numeric value.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Prepare geometries for distance calculation
  # ---------------------------------------------------------------------------

  x_calc <- x
  y_calc <- y

  if (sf::st_is_longlat(x_calc)) {

    x_calc <- sf::st_transform(
      x_calc,
      6875
    )

    y_calc <- sf::st_transform(
      y_calc,
      6875
    )

  } else {

    if (sf::st_crs(y_calc) != sf::st_crs(x_calc)) {

      y_calc <- sf::st_transform(
        y_calc,
        sf::st_crs(x_calc)
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 6. Find nearest features
  # ---------------------------------------------------------------------------

  nearest_idx <- sf::st_nearest_feature(
    x_calc,
    y_calc
  )


  # ---------------------------------------------------------------------------
  # 7. Calculate distances
  # ---------------------------------------------------------------------------

  distance <- sf::st_distance(
    x_calc,
    y_calc[nearest_idx, , drop = FALSE],
    by_element = TRUE
  )

  distance_m <- as.numeric(distance)


  # ---------------------------------------------------------------------------
  # 8. Create nearest identifiers
  # ---------------------------------------------------------------------------

  if (is.null(id_y)) {

    nearest_id <- nearest_idx

  } else {

    nearest_id <- y[[id_y]][nearest_idx]
  }


  # ---------------------------------------------------------------------------
  # 9. Apply maximum distance
  # ---------------------------------------------------------------------------

  outside <- distance_m > max_distance

  if (any(outside)) {

    nearest_id[outside] <- NA

    distance_m[outside] <- NA_real_
  }


  # ---------------------------------------------------------------------------
  # 10. Create result
  # ---------------------------------------------------------------------------

  result <- x

  result$nearest_id <- nearest_id
  result$distance_m <- distance_m


  # ---------------------------------------------------------------------------
  # 11. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    matched <- sum(
      !is.na(result$distance_m)
    )

    message(
      "Nearest features found for ",
      format(
        matched,
        big.mark = ","
      ),
      " of ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " feature(s)."
    )

    if (is.finite(max_distance)) {

      message(
        "Maximum distance: ",
        format(
          max_distance,
          big.mark = ","
        ),
        " m."
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 12. Return
  # ---------------------------------------------------------------------------

  result
}
