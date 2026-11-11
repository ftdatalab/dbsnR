test_that("dbsn_distance calculates nearest distances correctly", {

  # -----------------------------------------------------------------------
  # Test features
  # -----------------------------------------------------------------------

  x <- sf::st_as_sf(
    data.frame(
      name = c("A", "B", "C"),
      x = c(0, 3000, 10000),
      y = c(0, 0, 0)
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  y <- sf::st_as_sf(
    data.frame(
      id = c("Y1", "Y2"),
      x = c(1000, 8000),
      y = c(0, 0)
    ),
    coords = c("x", "y"),
    crs = 6875
  )


  # -----------------------------------------------------------------------
  # Run function
  # -----------------------------------------------------------------------

  result <- dbsn_distance(
    x = x,
    y = y
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
    3
  )

  expect_true(
    all(
      c(
        "nearest_id",
        "distance_m",
        "distance_km"
      ) %in% names(result)
    )
  )


  # -----------------------------------------------------------------------
  # Nearest feature checks
  # -----------------------------------------------------------------------

  expect_equal(
    result$nearest_id,
    c(1, 1, 2)
  )


  # -----------------------------------------------------------------------
  # Distance checks
  # -----------------------------------------------------------------------

  expect_equal(
    result$distance_m,
    c(
      1000,
      2000,
      2000
    ),
    tolerance = 1e-6
  )

  expect_equal(
    result$distance_km,
    c(
      1,
      2,
      2
    ),
    tolerance = 1e-6
  )


  # -----------------------------------------------------------------------
  # Original attributes
  # -----------------------------------------------------------------------

  expect_equal(
    result$name,
    c("A", "B", "C")
  )


  # -----------------------------------------------------------------------
  # CRS preservation
  # -----------------------------------------------------------------------

  expect_equal(
    sf::st_crs(result),
    sf::st_crs(x)
  )


  # -----------------------------------------------------------------------
  # Geometry preservation
  # -----------------------------------------------------------------------

  expect_equal(
    as.character(
      sf::st_geometry_type(result)
    ),
    rep("POINT", 3)
  )

})


test_that("dbsn_distance returns zero for intersecting features", {

  x <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 500,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  y <- sf::st_sf(
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

  result <- dbsn_distance(
    x = x,
    y = y
  )

  expect_equal(
    result$distance_m,
    0,
    tolerance = 1e-6
  )

  expect_equal(
    result$distance_km,
    0,
    tolerance = 1e-6
  )

  expect_equal(
    result$nearest_id,
    1
  )

})


test_that("dbsn_distance validates inputs", {

  # -----------------------------------------------------------------------
  # Valid objects used for validation tests
  # -----------------------------------------------------------------------

  x <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 0,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  y <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 1000,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 6875
  )


  # -----------------------------------------------------------------------
  # x must be sf
  # -----------------------------------------------------------------------

  expect_error(
    dbsn_distance(
      x = data.frame(id = 1),
      y = y
    ),
    "`x` must be an sf object"
  )


  # -----------------------------------------------------------------------
  # y must be sf
  # -----------------------------------------------------------------------

  expect_error(
    dbsn_distance(
      x = x,
      y = data.frame(id = 1)
    ),
    "`y` must be an sf object"
  )


  # -----------------------------------------------------------------------
  # Geographic CRS
  # -----------------------------------------------------------------------

  x_wgs84 <- sf::st_transform(
    x,
    4326
  )

  y_wgs84 <- sf::st_transform(
    y,
    4326
  )

  expect_error(
    dbsn_distance(
      x = x_wgs84,
      y = y_wgs84
    ),
    "geographic CRS"
  )

})


test_that("dbsn_distance transforms y to the CRS of x", {

  # -----------------------------------------------------------------------
  # x in EPSG:6875
  # -----------------------------------------------------------------------

  x <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 7000000,
      y = 4700000
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  # Create y in same CRS first
  y <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 7001000,
      y = 4700000
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  # Transform y to a different CRS
  y_different_crs <- sf::st_transform(
    y,
    4326
  )


  # -----------------------------------------------------------------------
  # Run function
  # -----------------------------------------------------------------------

  result <- suppressMessages(
    dbsn_distance(
      x = x,
      y = y_different_crs
    )
  )


  # -----------------------------------------------------------------------
  # Check result
  # -----------------------------------------------------------------------

  expect_equal(
    sf::st_crs(result),
    sf::st_crs(x)
  )

  expect_equal(
    result$distance_m,
    1000,
    tolerance = 0.1
  )

})
