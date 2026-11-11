#' Create a buffer around DBSN spatial features
#'
#' Creates a buffer around an sf or sfc object using a distance expressed
#' in metres.
#'
#' If the input data use geographic coordinates, the geometry is temporarily
#' transformed to a projected metric CRS for the buffer calculation and the
#' result is then transformed back to the original CRS.
#'
#' By default, EPSG:6875 (RDN2008 / Italy zone (N-E)) is used as the metric
#' CRS when the input uses longitude/latitude coordinates.
#'
#' @param x An sf or sfc object.
#'
#' @param distance Positive numeric value specifying the buffer distance
#'   in metres.
#'
#' @param crs Optional projected CRS used internally for the buffer
#'   calculation. If NULL and `x` uses geographic coordinates,
#'   EPSG:6875 is used.
#'
#' @param dissolve Logical. If TRUE, all buffer geometries are dissolved
#'   into one feature. If FALSE, individual buffers and their attributes
#'   are retained.
#'
#' @return
#' An sf object containing the buffer geometries.
#'
#' The returned object uses the same CRS as the original input `x`.
#'
#' @examples
#' \dontrun{
#' buffer <- dbsn_buffer(
#'   x,
#'   distance = 500
#' )
#'
#' buffer <- dbsn_buffer(
#'   x,
#'   distance = 1000,
#'   dissolve = FALSE
#' )
#' }
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
    !is.finite(distance) ||
    distance <= 0
  ) {
    stop(
      "`distance` must be a positive finite numeric value in metres.",
      call. = FALSE
    )
  }

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

  input_crs <- sf::st_crs(
    x
  )

  if (is.na(input_crs)) {
    stop(
      "`x` must have a valid CRS.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Choose CRS for metric calculation
  # ---------------------------------------------------------------------------

  if (sf::st_is_longlat(x)) {

    if (is.null(crs)) {
      calculation_crs <- sf::st_crs(
        6875
      )
    } else {
      calculation_crs <- sf::st_crs(
        crs
      )
    }

    if (is.na(calculation_crs)) {
      stop(
        "`crs` must be a valid coordinate reference system.",
        call. = FALSE
      )
    }

    if (sf::st_is_longlat(calculation_crs)) {
      stop(
        "`crs` must be a projected CRS with metric units.",
        call. = FALSE
      )
    }

    x_buffer <- sf::st_transform(
      x,
      calculation_crs
    )

  } else {

    if (!is.null(crs)) {

      calculation_crs <- sf::st_crs(
        crs
      )

      if (is.na(calculation_crs)) {
        stop(
          "`crs` must be a valid coordinate reference system.",
          call. = FALSE
        )
      }

      if (sf::st_is_longlat(calculation_crs)) {
        stop(
          "`crs` must be a projected CRS with metric units.",
          call. = FALSE
        )
      }

      x_buffer <- sf::st_transform(
        x,
        calculation_crs
      )

    } else {

      calculation_crs <- input_crs
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
  # 7. Restore original CRS
  # ---------------------------------------------------------------------------

  if (sf::st_crs(buffer) != input_crs) {

    buffer <- sf::st_transform(
      buffer,
      input_crs
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Reset row names
  # ---------------------------------------------------------------------------

  rownames(buffer) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Return
  # ---------------------------------------------------------------------------

  buffer
}
