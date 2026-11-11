#' Calculate the length of DBSN linear features
#'
#' Calculates the length of LINESTRING and MULTILINESTRING features
#' contained in an sf object.
#'
#' The original geometry and attributes are preserved. Two columns are added
#' to the result: `length_m`, containing the length in metres, and
#' `length_km`, containing the length in kilometres.
#'
#' If the input data use geographic coordinates, lengths are calculated
#' internally in EPSG:6875 (RDN2008 / Italy zone (N-E)) and the original
#' geometries and CRS are preserved in the returned object.
#'
#' @param x An sf object containing LINESTRING or MULTILINESTRING geometries.
#'
#' @param quiet Logical. If FALSE, displays information about the length
#'   calculation.
#'
#' @return
#' An sf object with the same geometry and attributes as `x`, with two
#' additional columns:
#' \itemize{
#'   \item `length_m`: feature length in metres;
#'   \item `length_km`: feature length in kilometres.
#' }
#'
#' @examples
#' \dontrun{
#' roads_length <- dbsn_length(
#'   roads
#' )
#'
#' roads_length[, c(
#'   "length_m",
#'   "length_km"
#' )]
#' }
#'
#' @export
dbsn_length <- function(
    x,
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
  # 2. Handle empty input
  # ---------------------------------------------------------------------------

  if (nrow(x) == 0L) {

    result <- x

    result$length_m <- numeric(0)
    result$length_km <- numeric(0)

    if (!quiet) {
      message(
        "`x` contains no features."
      )
    }

    return(
      result
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Validate geometry type
  # ---------------------------------------------------------------------------

  geometry_types <- unique(
    as.character(
      sf::st_geometry_type(
        x
      )
    )
  )

  allowed_types <- c(
    "LINESTRING",
    "MULTILINESTRING"
  )

  invalid_types <- setdiff(
    geometry_types,
    allowed_types
  )

  if (length(invalid_types) > 0L) {
    stop(
      "`x` must contain only LINESTRING or MULTILINESTRING geometries. ",
      "Found: ",
      paste(
        invalid_types,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Check CRS
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
  # 5. Prepare geometries for metric calculation
  # ---------------------------------------------------------------------------

  x_calc <- x

  if (sf::st_is_longlat(x_calc)) {

    x_calc <- sf::st_transform(
      x_calc,
      6875
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Calculate lengths
  # ---------------------------------------------------------------------------

  feature_length <- sf::st_length(
    x_calc
  )

  length_m <- as.numeric(
    feature_length
  )

  length_km <- length_m / 1000


  # ---------------------------------------------------------------------------
  # 7. Create result
  # ---------------------------------------------------------------------------

  result <- x

  result$length_m <- length_m
  result$length_km <- length_km


  # ---------------------------------------------------------------------------
  # 8. Reset row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    total_length_m <- sum(
      result$length_m,
      na.rm = TRUE
    )

    total_length_km <- total_length_m / 1000

    message(
      "Calculated length for ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " feature(s)."
    )

    message(
      "Total length: ",
      format(
        round(
          total_length_km,
          3
        ),
        big.mark = ",",
        nsmall = 3
      ),
      " km."
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return
  # ---------------------------------------------------------------------------

  result
}
