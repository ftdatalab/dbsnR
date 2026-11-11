#' Locate features along a line
#'
#' Calculates the position of spatial features along a reference line.
#'
#' Each feature in `objects` is converted to a representative point when
#' necessary and projected onto the nearest position of `line`.
#'
#' The output contains the distance from the start of the line in metres,
#' kilometres, and as a percentage of the total line length.
#'
#' @param line An `sf` object containing exactly one LINESTRING or
#'   MULTILINESTRING feature.
#' @param objects An `sf` object containing the features to locate along
#'   the line.
#'
#' @return The original `objects` object with three additional columns:
#'   \itemize{
#'     \item `distance_m`: distance from the start of the line in metres.
#'     \item `distance_km`: distance from the start of the line in kilometres.
#'     \item `distance_pct`: position along the line as percentage of its
#'       total length.
#'   }
#'
#' @details
#' The reference line must use a projected coordinate reference system.
#'
#' If `objects` uses a different CRS, it is automatically transformed to
#' the CRS of `line`.
#'
#' POINT geometries are used directly. Other supported geometry types are
#' converted to representative points using `sf::st_point_on_surface()`.
#'
#' The position along the line is calculated with
#' `sf::st_line_locate_point()`.
#'
#' @examples
#' \dontrun{
#'
#' result <- dbsn_along(
#'   line = test_road,
#'   objects = loc_sg
#' )
#'
#' result[, c(
#'   "distance_m",
#'   "distance_km",
#'   "distance_pct"
#' )]
#'
#' }
#'
#' @export

dbsn_along <- function(line, objects) {

  # -----------------------------------------------------------------------
  # Input validation
  # -----------------------------------------------------------------------

  if (!inherits(line, "sf")) {
    stop(
      "`line` must be an sf object.",
      call. = FALSE
    )
  }

  if (!inherits(objects, "sf")) {
    stop(
      "`objects` must be an sf object.",
      call. = FALSE
    )
  }

  if (nrow(line) == 0) {
    stop(
      "`line` contains no features.",
      call. = FALSE
    )
  }

  if (nrow(objects) == 0) {
    stop(
      "`objects` contains no features.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # CRS validation
  # -----------------------------------------------------------------------

  if (is.na(sf::st_crs(line))) {
    stop(
      "`line` has no CRS.",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(objects))) {
    stop(
      "`objects` has no CRS.",
      call. = FALSE
    )
  }

  if (sf::st_is_longlat(line)) {
    stop(
      "`line` uses a geographic CRS (longitude/latitude). ",
      "Please transform it to a projected CRS.",
      call. = FALSE
    )
  }

  if (sf::st_crs(objects) != sf::st_crs(line)) {

    objects <- sf::st_transform(
      objects,
      sf::st_crs(line)
    )

    message(
      "`objects` transformed to the CRS of `line`."
    )
  }


  # -----------------------------------------------------------------------
  # Line validation
  # -----------------------------------------------------------------------

  if (nrow(line) != 1) {
    stop(
      "`line` must contain exactly one feature. ",
      "Combine multiple features before using dbsn_along().",
      call. = FALSE
    )
  }

  line_type <- as.character(
    sf::st_geometry_type(line)
  )

  allowed_line_types <- c(
    "LINESTRING",
    "MULTILINESTRING"
  )

  if (!all(line_type %in% allowed_line_types)) {
    stop(
      "`line` must contain a LINESTRING or MULTILINESTRING geometry.",
      call. = FALSE
    )
  }

  if (any(sf::st_is_empty(line))) {
    stop(
      "`line` contains an empty geometry.",
      call. = FALSE
    )
  }

  line_valid <- sf::st_is_valid(line)

  if (any(is.na(line_valid) | !line_valid)) {
    stop(
      "`line` contains an invalid geometry. ",
      "Repair it before using dbsn_along().",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Objects geometry validation
  # -----------------------------------------------------------------------

  allowed_object_types <- c(
    "POINT",
    "MULTIPOINT",
    "LINESTRING",
    "MULTILINESTRING",
    "POLYGON",
    "MULTIPOLYGON"
  )

  object_types <- unique(
    as.character(sf::st_geometry_type(objects))
  )

  unsupported <- setdiff(
    object_types,
    allowed_object_types
  )

  if (length(unsupported) > 0) {
    stop(
      "`objects` contains unsupported geometry type(s): ",
      paste(unsupported, collapse = ", "),
      ".",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Remove empty geometries
  # -----------------------------------------------------------------------

  empty <- sf::st_is_empty(objects)

  if (any(empty)) {

    n_empty <- sum(empty)

    objects <- objects[
      !empty,
      ,
      drop = FALSE
    ]

    message(
      n_empty,
      if (n_empty == 1) {
        " empty geometry removed from `objects`."
      } else {
        " empty geometries removed from `objects`."
      }
    )
  }

  if (nrow(objects) == 0) {
    stop(
      "No geometries remain in `objects` after removing empty geometries.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Repair invalid geometries
  # -----------------------------------------------------------------------

  valid <- sf::st_is_valid(objects)

  invalid <- is.na(valid) | !valid

  if (any(invalid)) {

    n_invalid <- sum(invalid)

    repaired <- sf::st_make_valid(
      objects[
        invalid,
        ,
        drop = FALSE
      ]
    )

    sf::st_geometry(objects)[invalid] <-
      sf::st_geometry(repaired)

    message(
      n_invalid,
      if (n_invalid == 1) {
        " invalid geometry repaired in `objects`."
      } else {
        " invalid geometries repaired in `objects`."
      }
    )
  }


  # -----------------------------------------------------------------------
  # Final geometry validation
  # -----------------------------------------------------------------------

  if (any(sf::st_is_empty(objects))) {
    stop(
      "Some geometries are empty after geometry repair.",
      call. = FALSE
    )
  }

  valid_final <- sf::st_is_valid(objects)

  if (any(is.na(valid_final) | !valid_final)) {
    stop(
      "Some geometries remain invalid after attempting repair.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Prepare reference line
  # -----------------------------------------------------------------------

  line_geom <- sf::st_geometry(line)

  # Convert MULTILINESTRING to LINESTRING where possible
  if (as.character(sf::st_geometry_type(line))[1] ==
      "MULTILINESTRING") {

    line_geom <- sf::st_cast(
      line_geom,
      "LINESTRING"
    )

    if (length(line_geom) != 1) {
      stop(
        "`line` contains a disconnected MULTILINESTRING. ",
        "A single continuous line is required.",
        call. = FALSE
      )
    }
  }


  # -----------------------------------------------------------------------
  # Calculate total line length
  # -----------------------------------------------------------------------

  total_length <- as.numeric(
    sf::st_length(line_geom)
  )

  if (
    length(total_length) != 1 ||
    is.na(total_length) ||
    total_length <= 0
  ) {
    stop(
      "`line` has zero or invalid length.",
      call. = FALSE
    )
  }


  # -----------------------------------------------------------------------
  # Create representative points
  # -----------------------------------------------------------------------

  geom_types <- as.character(
    sf::st_geometry_type(objects)
  )

  points <- vector(
    "list",
    nrow(objects)
  )

  for (i in seq_len(nrow(objects))) {

    geom <- sf::st_geometry(objects[i, ])

    if (geom_types[i] == "POINT") {

      points[[i]] <- geom[[1]]

    } else {

      point <- suppressWarnings(
        sf::st_point_on_surface(geom)
      )

      # MULTIPOINT or other result must become a single POINT
      if (
        as.character(
          sf::st_geometry_type(point)
        )[1] != "POINT"
      ) {

        point <- sf::st_centroid(
          sf::st_union(point)
        )
      }

      points[[i]] <- point[[1]]
    }
  }

  points <- sf::st_sfc(
    points,
    crs = sf::st_crs(line)
  )


  # -----------------------------------------------------------------------
  # Locate points along line
  # -----------------------------------------------------------------------

  locations <- vapply(
    seq_along(points),
    function(i) {

      as.numeric(
        sf::st_line_locate_point(
          line_geom,
          points[i]
        )
      )
    },
    numeric(1)
  )


  # -----------------------------------------------------------------------
  # Calculate distances
  # -----------------------------------------------------------------------

  # st_line_locate_point() returns a fractional position
  # between 0 and 1 along the line.

  locations <- pmax(
    0,
    pmin(1, locations)
  )

  distance_m <- locations * total_length


  # -----------------------------------------------------------------------
  # Add results
  # -----------------------------------------------------------------------

  objects$distance_m <- distance_m

  objects$distance_km <-
    distance_m / 1000

  objects$distance_pct <-
    locations * 100


  # -----------------------------------------------------------------------
  # Message
  # -----------------------------------------------------------------------

  message(
    "Located ",
    nrow(objects),
    " feature(s) along a ",
    round(total_length, 1),
    " m line."
  )


  # -----------------------------------------------------------------------
  # Return
  # -----------------------------------------------------------------------

  objects
}
