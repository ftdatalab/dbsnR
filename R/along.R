#' Locate features along a linear route
#'
#' Calculates the position of spatial features along a linear route.
#'
#' For each feature in `objects`, the function identifies the nearest point
#' on the route and calculates:
#' \itemize{
#'   \item the distance travelled along the route from its starting point;
#'   \item the lateral distance between the feature and the route.
#' }
#'
#' The direction of the route is important. The along-route distance is
#' measured from the first vertex of the input line to its last vertex.
#'
#' If the input data use geographic coordinates, calculations are performed
#' internally in EPSG:6875 (RDN2008 / Italy zone (N-E)). The original
#' geometries, attributes, and CRS of `objects` are preserved.
#'
#' @param x An sf or sfc object containing one LINESTRING or
#'   MULTILINESTRING route.
#'
#' @param objects An sf object containing the features to locate along
#'   the route.
#'
#' @param max_distance Optional maximum lateral distance in metres.
#'   Features farther than this distance are returned with `along_fraction`,
#'   `along_m`, `along_km`, and `distance_m` set to NA.
#'   Default is Inf.
#'
#' @param sort Logical. If TRUE, the result is ordered by increasing
#'   distance along the route.
#'
#' @param quiet Logical. If FALSE, displays information about the
#'   calculation.
#'
#' @return
#' An sf object containing the original geometries and attributes of
#' `objects`, with four additional columns:
#' \itemize{
#'   \item `along_fraction`: relative position along the route, from 0 to 1;
#'   \item `along_m`: distance along the route in metres;
#'   \item `along_km`: distance along the route in kilometres;
#'   \item `distance_m`: shortest lateral distance between the feature
#'     and the route, in metres.
#' }
#'
#' @details
#' Multiple route features in `x` are combined before the calculation.
#' The resulting geometry must form one continuous LINESTRING.
#'
#' The position along the route is calculated by projecting the nearest
#' route point onto the segments of the LINESTRING and measuring the
#' cumulative segment length from the first route vertex.
#'
#' The original geometries in `objects` are not modified.
#'
#' If `x` contains multiple connected line features, their order and
#' orientation after merging may differ from the original input order.
#' For an unambiguous progression direction, a single ordered LINESTRING
#' is recommended.
#'
#' @examples
#' \dontrun{
#'
#' located_sites <- dbsn_along(
#'   route,
#'   archaeological_sites
#' )
#'
#' located_sites <- dbsn_along(
#'   route,
#'   archaeological_sites,
#'   max_distance = 500,
#'   sort = TRUE
#' )
#' }
#'
#' @export
dbsn_along <- function(
    x,
    objects,
    max_distance = Inf,
    sort = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate route input
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
      "`x` contains no route features.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Validate route geometry type
  # ---------------------------------------------------------------------------

  route_types <- unique(
    as.character(
      sf::st_geometry_type(x)
    )
  )

  allowed_types <- c(
    "LINESTRING",
    "MULTILINESTRING"
  )

  invalid_types <- setdiff(
    route_types,
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
  # 3. Validate objects
  # ---------------------------------------------------------------------------

  if (!inherits(objects, "sf")) {
    stop(
      "`objects` must be an sf object.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Validate options
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

  if (
    !is.logical(sort) ||
    length(sort) != 1L ||
    is.na(sort)
  ) {
    stop(
      "`sort` must be TRUE or FALSE.",
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
  # 5. Handle empty objects
  # ---------------------------------------------------------------------------

  if (nrow(objects) == 0L) {

    result <- objects

    result$along_fraction <- numeric(0)
    result$along_m <- numeric(0)
    result$along_km <- numeric(0)
    result$distance_m <- numeric(0)

    if (!quiet) {
      message(
        "`objects` contains no features."
      )
    }

    return(result)
  }


  # ---------------------------------------------------------------------------
  # 6. Check CRS
  # ---------------------------------------------------------------------------

  route_crs <- sf::st_crs(x)
  objects_crs <- sf::st_crs(objects)

  if (is.na(route_crs)) {
    stop(
      "`x` must have a valid CRS.",
      call. = FALSE
    )
  }

  if (is.na(objects_crs)) {
    stop(
      "`objects` must have a valid CRS.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Select metric CRS
  # ---------------------------------------------------------------------------

  if (sf::st_is_longlat(x)) {

    calculation_crs <- sf::st_crs(
      6875
    )

  } else {

    calculation_crs <- route_crs
  }

  route_work <- sf::st_transform(
    x,
    calculation_crs
  )

  objects_work <- sf::st_transform(
    objects,
    calculation_crs
  )


  # ---------------------------------------------------------------------------
  # 8. Create one continuous route
  # ---------------------------------------------------------------------------

  if (nrow(route_work) == 1L) {

    route_geometry <- sf::st_geometry(
      route_work
    )

    if (
      as.character(
        sf::st_geometry_type(route_geometry)
      ) == "MULTILINESTRING"
    ) {

      route_geometry <- sf::st_line_merge(
        route_geometry
      )
    }

  } else {

    route_geometry <- sf::st_union(
      sf::st_geometry(
        route_work
      )
    )

    route_geometry <- sf::st_line_merge(
      route_geometry
    )
  }

  route_type <- as.character(
    sf::st_geometry_type(
      route_geometry
    )
  )

  if (
    length(route_type) != 1L ||
    route_type != "LINESTRING"
  ) {
    stop(
      "`x` must form one continuous LINESTRING after merging. ",
      "Disconnected route parts do not define an unambiguous progression.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 9. Extract route coordinates
  # ---------------------------------------------------------------------------

  route_coordinates <- sf::st_coordinates(
    route_geometry
  )

  route_coordinates <- route_coordinates[
    ,
    c("X", "Y"),
    drop = FALSE
  ]

  if (nrow(route_coordinates) < 2L) {
    stop(
      "The route must contain at least two vertices.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Prepare route segments
  # ---------------------------------------------------------------------------

  segment_start <- route_coordinates[
    -nrow(route_coordinates),
    ,
    drop = FALSE
  ]

  segment_end <- route_coordinates[
    -1L,
    ,
    drop = FALSE
  ]

  segment_vector <- segment_end - segment_start

  segment_length_squared <- rowSums(
    segment_vector^2
  )

  segment_length <- sqrt(
    segment_length_squared
  )

  valid_segments <- (
    is.finite(segment_length) &
      segment_length > 0
  )

  if (!any(valid_segments)) {
    stop(
      "The route must have a positive finite length.",
      call. = FALSE
    )
  }

  segment_start <- segment_start[
    valid_segments,
    ,
    drop = FALSE
  ]

  segment_vector <- segment_vector[
    valid_segments,
    ,
    drop = FALSE
  ]

  segment_length_squared <- segment_length_squared[
    valid_segments
  ]

  segment_length <- segment_length[
    valid_segments
  ]


  # ---------------------------------------------------------------------------
  # 11. Calculate cumulative route distances
  # ---------------------------------------------------------------------------

  cumulative_start <- c(0,cumsum(segment_length))

  cumulative_start <- cumulative_start[seq_along(segment_length)]

  route_length_m <- sum(
    segment_length
  )

  if (
    is.na(route_length_m) ||
    !is.finite(route_length_m) ||
    route_length_m <= 0
  ) {
    stop(
      "The route must have a positive finite length.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 12. Find nearest point on route for each object
  # ---------------------------------------------------------------------------

  object_geometries <- sf::st_geometry(
    objects_work
  )

  repeated_route <- rep(
    route_geometry,
    length(object_geometries)
  )

  nearest_connections <- sf::st_nearest_points(
    object_geometries,
    repeated_route,
    pairwise = TRUE
  )


  # ---------------------------------------------------------------------------
  # 13. Extract nearest route coordinates and lateral distances
  # ---------------------------------------------------------------------------

  nearest_route_coordinates <- matrix(
    NA_real_,
    nrow = length(nearest_connections),
    ncol = 2L,
    dimnames = list(
      NULL,
      c("X", "Y")
    )
  )

  distance_m <- numeric(
    length(nearest_connections)
  )

  for (i in seq_along(nearest_connections)) {

    connection_coordinates <- sf::st_coordinates(
      nearest_connections[i]
    )

    nearest_route_coordinates[i, ] <-
      connection_coordinates[
        nrow(connection_coordinates),
        c("X", "Y")
      ]

    distance_m[i] <- as.numeric(
      sf::st_length(
        nearest_connections[i]
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 14. Internal function: locate one point along route
  # ---------------------------------------------------------------------------

  locate_point <- function(point_coordinates) {

    point_matrix <- matrix(
      point_coordinates,
      nrow = nrow(segment_start),
      ncol = 2L,
      byrow = TRUE
    )

    point_offset <- point_matrix -
      segment_start

    projection_fraction <- rowSums(
      point_offset * segment_vector
    ) / segment_length_squared

    projection_fraction <- pmin(
      pmax(
        projection_fraction,
        0
      ),
      1
    )

    projected_coordinates <-
      segment_start +
      segment_vector * projection_fraction

    difference <-
      projected_coordinates -
      point_matrix

    squared_distance <- rowSums(
      difference^2
    )

    nearest_segment <- which.min(
      squared_distance
    )

    cumulative_start[nearest_segment] +
      projection_fraction[nearest_segment] *
      segment_length[nearest_segment]
  }


  # ---------------------------------------------------------------------------
  # 15. Calculate position along route
  # ---------------------------------------------------------------------------

  along_m <- vapply(
    seq_len(
      nrow(nearest_route_coordinates)
    ),
    function(i) {

      locate_point(
        nearest_route_coordinates[i, ]
      )
    },
    numeric(1)
  )

  along_fraction <- along_m /
    route_length_m

  along_fraction <- pmin(
    pmax(
      along_fraction,
      0
    ),
    1
  )

  along_km <- along_m / 1000


  # ---------------------------------------------------------------------------
  # 16. Apply maximum lateral distance
  # ---------------------------------------------------------------------------

  outside <- distance_m >
    max_distance

  if (any(outside)) {

    along_fraction[outside] <- NA_real_
    along_m[outside] <- NA_real_
    along_km[outside] <- NA_real_
    distance_m[outside] <- NA_real_
  }


  # ---------------------------------------------------------------------------
  # 17. Create result
  # ---------------------------------------------------------------------------

  result <- objects

  result$along_fraction <- along_fraction
  result$along_m <- along_m
  result$along_km <- along_km
  result$distance_m <- distance_m


  # ---------------------------------------------------------------------------
  # 18. Sort result if requested
  # ---------------------------------------------------------------------------

  if (sort) {

    order_index <- order(
      result$along_m,
      na.last = TRUE
    )

    result <- result[
      order_index,
      ,
      drop = FALSE
    ]
  }

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 19. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    located <- sum(
      !is.na(result$along_m)
    )

    message(
      "Route length: ",
      format(
        round(
          route_length_m / 1000,
          3
        ),
        big.mark = ",",
        nsmall = 3
      ),
      " km."
    )

    message(
      "Located ",
      format(
        located,
        big.mark = ","
      ),
      " of ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " feature(s) along the route."
    )

    if (is.finite(max_distance)) {

      message(
        "Maximum lateral distance: ",
        format(
          max_distance,
          big.mark = ","
        ),
        " m."
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 20. Return
  # ---------------------------------------------------------------------------

  result
}
