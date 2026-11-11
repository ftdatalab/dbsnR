#' Calculate the area of DBSN polygon features
#'
#' Calculates the area of POLYGON and MULTIPOLYGON features contained
#' in an sf object.
#'
#' The original geometry and attributes are preserved. Three columns are
#' added to the result: `area_m2`, `area_ha`, and `area_km2`.
#'
#' If the input data use geographic coordinates, areas are calculated
#' internally in EPSG:6875 (RDN2008 / Italy zone (N-E)) and the original
#' geometries and CRS are preserved in the returned object.
#'
#' @param x An sf object containing POLYGON or MULTIPOLYGON geometries.
#'
#' @param quiet Logical. If FALSE, displays information about the area
#'   calculation.
#'
#' @return
#' An sf object with the same geometry and attributes as `x`, with three
#' additional columns:
#' \itemize{
#'   \item `area_m2`: feature area in square metres;
#'   \item `area_ha`: feature area in hectares;
#'   \item `area_km2`: feature area in square kilometres.
#' }
#'
#' @examples
#' \dontrun{
#' areas <- dbsn_area(
#'   loc_sg
#' )
#'
#' areas[, c(
#'   "area_m2",
#'   "area_ha",
#'   "area_km2"
#' )]
#' }
#'
#' @export
dbsn_area <- function(
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

    result$area_m2 <- numeric(0)
    result$area_ha <- numeric(0)
    result$area_km2 <- numeric(0)

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
    "POLYGON",
    "MULTIPOLYGON"
  )

  invalid_types <- setdiff(
    geometry_types,
    allowed_types
  )

  if (length(invalid_types) > 0L) {
    stop(
      "`x` must contain only POLYGON or MULTIPOLYGON geometries. ",
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
  # 6. Calculate areas
  # ---------------------------------------------------------------------------

  feature_area <- sf::st_area(
    x_calc
  )

  area_m2 <- as.numeric(
    feature_area
  )

  area_ha <- area_m2 / 10000

  area_km2 <- area_m2 / 1000000


  # ---------------------------------------------------------------------------
  # 7. Create result
  # ---------------------------------------------------------------------------

  result <- x

  result$area_m2 <- area_m2
  result$area_ha <- area_ha
  result$area_km2 <- area_km2


  # ---------------------------------------------------------------------------
  # 8. Reset row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    total_area_m2 <- sum(
      result$area_m2,
      na.rm = TRUE
    )

    total_area_ha <- total_area_m2 / 10000

    total_area_km2 <- total_area_m2 / 1000000

    message(
      "Calculated area for ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " feature(s)."
    )

    message(
      "Total area: ",
      format(
        round(
          total_area_ha,
          3
        ),
        big.mark = ",",
        nsmall = 3
      ),
      " ha (",
      format(
        round(
          total_area_km2,
          3
        ),
        big.mark = ",",
        nsmall = 3
      ),
      " km2)."
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return
  # ---------------------------------------------------------------------------

  result
}
