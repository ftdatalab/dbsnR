test_that("dbsn_along locates points correctly along a line", {

  # -----------------------------------------------------------------------
  # Reference line: 10 km
  # -----------------------------------------------------------------------

  test_line <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(
          c(
            0,     0,
            10000, 0
          ),
          ncol = 2,
          byrow = TRUE
        )
      ),
      crs = 6875
    )
  )

  # -----------------------------------------------------------------------
  # Points at known positions along the line
  # -----------------------------------------------------------------------

  test_points <- sf::st_as_sf(
    data.frame(
      name = c(
        "start",
        "quarter",
        "middle",
        "three_quarters",
        "end"
      ),
      x = c(
        0,
        2500,
        5000,
        7500,
        10000
      ),
      y = c(
        500,
        300,
        1000,
        -800,
        400
      )
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  # -----------------------------------------------------------------------
  # Run function
  # -----------------------------------------------------------------------

  result <- dbsn_along(
    line = test_line,
    objects = test_points
  )

  # -----------------------------------------------------------------------
  # Object checks
  # -----------------------------------------------------------------------

  expect_s3_class(
    result,
    "sf"
  )

  expect_equal(
    nrow(result),
    5
  )

  expect_true(
    all(
      c(
        "distance_m",
        "distance_km",
        "distance_pct"
      ) %in% names(result)
    )
  )

  # -----------------------------------------------------------------------
  # Distance checks
  # -----------------------------------------------------------------------

  expect_equal(
    result$distance_m,
    c(
      0,
      2500,
      5000,
      7500,
      10000
    ),
    tolerance = 1e-6
  )

  expect_equal(
    result$distance_km,
    c(
      0,
      2.5,
      5,
      7.5,
      10
    ),
    tolerance = 1e-6
  )

  expect_equal(
    result$distance_pct,
    c(
      0,
      25,
      50,
      75,
      100
    ),
    tolerance = 1e-6
  )

  # -----------------------------------------------------------------------
  # CRS and geometry preservation
  # -----------------------------------------------------------------------

  expect_equal(
    sf::st_crs(result),
    sf::st_crs(test_line)
  )

  expect_equal(
    as.character(sf::st_geometry_type(result)),
    rep("POINT", 5)
  )
})

test_that("dbsn_along validates its inputs", {

  # Valid reference data
  line <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(
          c(
            0, 0,
            1000, 0
          ),
          ncol = 2,
          byrow = TRUE
        )
      ),
      crs = 6875
    )
  )

  points <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 500,
      y = 100
    ),
    coords = c("x", "y"),
    crs = 6875
  )


  # -----------------------------------------------------------------------
  # Non-sf inputs
  # -----------------------------------------------------------------------

  expect_error(
    dbsn_along(
      line = data.frame(x = 1),
      objects = points
    ),
    "`line` must be an sf object"
  )

  expect_error(
    dbsn_along(
      line = line,
      objects = data.frame(x = 1)
    ),
    "`objects` must be an sf object"
  )


  # -----------------------------------------------------------------------
  # Wrong geometry type for line
  # -----------------------------------------------------------------------

  polygon <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(
              0, 0,
              1000, 0,
              1000, 1000,
              0, 1000,
              0, 0
            ),
            ncol = 2,
            byrow = TRUE
          )
        )
      ),
      crs = 6875
    )
  )

  expect_error(
    dbsn_along(
      line = polygon,
      objects = points
    ),
    "LINESTRING"
  )


  # -----------------------------------------------------------------------
  # Geographic CRS
  # -----------------------------------------------------------------------

  line_wgs84 <- sf::st_transform(
    line,
    4326
  )

  points_wgs84 <- sf::st_transform(
    points,
    4326
  )

  expect_error(
    dbsn_along(
      line = line_wgs84,
      objects = points_wgs84
    ),
    "geographic CRS"
  )


  # -----------------------------------------------------------------------
  # Multiple line features
  # -----------------------------------------------------------------------

  two_lines <- rbind(
    line,
    line
  )

  expect_error(
    dbsn_along(
      line = two_lines,
      objects = points
    ),
    "exactly one feature"
  )
})
