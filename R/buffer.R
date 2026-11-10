#' Create a buffer around a spatial object
#'
#' Creates a buffer of a specified distance around an sf or sfc object.
#' Geographic coordinates are automatically transformed to a projected
#' CRS before buffering.
#'
#' @param x An sf or sfc object.
#' @param distance Buffer distance in metres.
#' @param crs Optional projected CRS used to calculate the buffer.
#'   If NULL, EPSG:6875 (RDN2008 / Italy zone) is used for geographic
#'   coordinates.
#' @param dissolve Logical. If TRUE, overlapping buffers are merged
#'   into a single geometry.
#'
#' @return An sf object containing the buffer geometry.
#'
#' @export
dbsn_buffer <- function(
    x,
    distance,
    crs = NULL,
    dissolve = TRUE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate input
  # ---------------------------------------------------------------------------

  if (!inherits(x, "sf") && !inherits(x, "sfc")) {
    stop(
      "`x` must be an sf or sfc object.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(distance) ||
    length(distance) != 1L ||
    is.na(distance) ||
    distance <= 0
  ) {
    stop(
      "`distance` must be a positive numeric value in metres.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Convert sfc to sf
  # ---------------------------------------------------------------------------

  if (inherits(x, "sfc")) {
    x <- sf::st_sf(
      geometry = x
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Check CRS
  # ---------------------------------------------------------------------------

  input_crs <- sf::st_crs(x)

  if (is.na(input_crs)) {
    stop(
      "`x` must have a valid CRS.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Choose CRS for buffering
  # ---------------------------------------------------------------------------

  if (sf::st_is_longlat(x)) {

    if (is.null(crs)) {
      crs <- 6875
    }

    x_buffer <- sf::st_transform(
      x,
      crs
    )

  } else {

    if (!is.null(crs)) {

      x_buffer <- sf::st_transform(
        x,
        crs
      )

    } else {

      x_buffer <- x
    }
  }


  # ---------------------------------------------------------------------------
  # 5. Create buffer
  # ---------------------------------------------------------------------------

  buffer <- sf::st_buffer(
    x_buffer,
    dist = distance
  )


  # ---------------------------------------------------------------------------
  # 6. Dissolve buffers
  # ---------------------------------------------------------------------------

  if (dissolve) {

    buffer <- sf::st_union(
      buffer
    )

    buffer <- sf::st_sf(
      geometry = buffer
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Return buffer
  # ---------------------------------------------------------------------------

  buffer
}
