#' Create a corridor around linear DBSN features
#'
#' Creates a spatial corridor around one or more linear features.
#'
#' The value supplied to `distance` represents the corridor width on each
#' side of the line. For example, `distance = 500` creates a corridor extending
#' 500 metres on the left and 500 metres on the right of the line, for an
#' approximate total width of 1,000 metres.
#'
#' Optionally, an sf object can be supplied through `objects`. In this case,
#' the function returns the features that intersect or are completely within
#' the corridor.
#'
#' If the input line uses geographic coordinates, the corridor is calculated
#' internally in EPSG:6875 (RDN2008 / Italy zone (N-E)) and then transformed
#' back to the original CRS.
#'
#' @param x An sf or sfc object containing LINESTRING or MULTILINESTRING
#'   geometries.
#'
#' @param distance Positive numeric value specifying the corridor distance
#'   in metres on each side of the line.
#'
#' @param objects Optional sf object containing features to select using the
#'   corridor. If NULL, the corridor polygon itself is returned.
#'
#' @param relation Spatial relationship used when `objects` is supplied.
#'   Either `"intersects"` or `"within"`.
#'   `"intersects"` selects features that touch or partially overlap the
#'   corridor. `"within"` selects only features completely contained in the
#'   corridor.
#'
#' @param quiet Logical. If FALSE, displays information about the corridor
#'   and the number of selected features.
#'
#' @return
#' If `objects` is NULL, an sf object containing one dissolved corridor
#' polygon is returned.
#'
#' If `objects` is supplied, an sf object containing the selected features
#' from `objects` is returned. Their original geometries, attributes, and CRS
#' are preserved.
#'
#' @details
#' All input line geometries are combined before the buffer is calculated.
#' Therefore, the returned corridor consists of one feature, which may have
#' a POLYGON or MULTIPOLYGON geometry.
#'
#' The function does not clip the geometries in `objects`. It only selects
#' complete features according to the requested spatial relationship.
#'
#' @examples
#' \dontrun{
#' corridor <- dbsn_corridor(
#'   road,
#'   distance = 500
#' )
#'
#' nearby_sites <- dbsn_corridor(
#'   road,
#'   distance = 500,
#'   objects = archaeological_sites
#' )
#'
#' contained_sites <- dbsn_corridor(
#'   road,
#'   distance = 1000,
#'   objects = archaeological_sites,
#'   relation = "within"
#' )
#' }
#'
#' @export
dbsn_corridor <- function(
    x,
    distance,
    objects = NULL,
    relation = c("intersects", "within"),
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate relation
  # ---------------------------------------------------------------------------

  relation <- match.arg(
    relation
  )


  # ---------------------------------------------------------------------------
  # 2. Validate x
  # ---------------------------------------------------------------------------

  if (!inherits(x, "sf") && !inherits(x, "sfc")) {
    stop(
      "`x` must be an sf or sfc object.",
      call. = FALSE
    )
  }

  if (inherits(x, "sfc")) {

    x <- sf::st_sf(
      geometry = x
    )
  }

  if (nrow(x) == 0L) {
    stop(
      "`x` contains no features.",
      call. = FALSE
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
  # 4. Validate distance
  # ---------------------------------------------------------------------------

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


  # ---------------------------------------------------------------------------
  # 5. Validate objects
  # ---------------------------------------------------------------------------

  if (
    !is.null(objects) &&
    !inherits(objects, "sf")
  ) {
    stop(
      "`objects` must be NULL or an sf object.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Validate quiet
  # ---------------------------------------------------------------------------

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
  # 7. Check CRS
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

  if (
    !is.null(objects) &&
    is.na(sf::st_crs(objects))
  ) {
    stop(
      "`objects` must have a valid CRS.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Select metric CRS
  # ---------------------------------------------------------------------------

  if (sf::st_is_longlat(x)) {

    calculation_crs <- sf::st_crs(
      6875
    )

    x_work <- sf::st_transform(
      x,
      calculation_crs
    )

  } else {

    calculation_crs <- input_crs
    x_work <- x
  }


  # ---------------------------------------------------------------------------
  # 9. Combine input lines
  # ---------------------------------------------------------------------------

  line_geometry <- sf::st_union(
    sf::st_geometry(
      x_work
    )
  )


  # ---------------------------------------------------------------------------
  # 10. Create corridor
  # ---------------------------------------------------------------------------

  corridor_geometry <- sf::st_buffer(
    line_geometry,
    dist = distance
  )

  corridor_work <- sf::st_sf(
    distance_m = distance,
    total_width_m = distance * 2,
    geometry = corridor_geometry
  )


  # ---------------------------------------------------------------------------
  # 11. Return corridor when objects are not supplied
  # ---------------------------------------------------------------------------

  if (is.null(objects)) {

    corridor_result <- corridor_work

    if (calculation_crs != input_crs) {

      corridor_result <- sf::st_transform(
        corridor_result,
        input_crs
      )
    }

    rownames(corridor_result) <- NULL

    if (!quiet) {

      message(
        "Corridor created: ",
        format(
          distance,
          big.mark = ","
        ),
        " m on each side of the line."
      )

      message(
        "Approximate total width: ",
        format(
          distance * 2,
          big.mark = ","
        ),
        " m."
      )
    }

    return(
      corridor_result
    )
  }


  # ---------------------------------------------------------------------------
  # 12. Handle empty objects
  # ---------------------------------------------------------------------------

  if (nrow(objects) == 0L) {

    if (!quiet) {
      message(
        "`objects` contains no features."
      )
    }

    return(objects)
  }


  # ---------------------------------------------------------------------------
  # 13. Transform objects to calculation CRS
  # ---------------------------------------------------------------------------

  objects_work <- objects

  if (
    sf::st_crs(objects_work) != calculation_crs
  ) {

    objects_work <- sf::st_transform(
      objects_work,
      calculation_crs
    )
  }


  # ---------------------------------------------------------------------------
  # 14. Select objects located in the corridor
  # ---------------------------------------------------------------------------

  if (relation == "intersects") {

    selected <- lengths(
      sf::st_intersects(
        objects_work,
        corridor_work
      )
    ) > 0L

  } else {

    selected <- lengths(
      sf::st_within(
        objects_work,
        corridor_work
      )
    ) > 0L
  }


  # ---------------------------------------------------------------------------
  # 15. Preserve original object geometries and CRS
  # ---------------------------------------------------------------------------

  result <- objects[
    selected,
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 16. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Corridor created: ",
      format(
        distance,
        big.mark = ","
      ),
      " m on each side of the line."
    )

    message(
      "Relation: ",
      relation,
      "."
    )

    message(
      "Selected ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " of ",
      format(
        nrow(objects),
        big.mark = ","
      ),
      " feature(s)."
    )
  }


  # ---------------------------------------------------------------------------
  # 17. Return
  # ---------------------------------------------------------------------------

  result
}
