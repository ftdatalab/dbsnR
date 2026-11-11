test_that("dbsn_plot supports POINT geometries", {

  x <- sf::st_as_sf(
    data.frame(
      id = 1,
      x = 0,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 6875
  )

  expect_invisible(
    dbsn_plot(x)
  )

})


test_that("dbsn_plot supports LINESTRING geometries", {

  x <- sf::st_sf(
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

  expect_invisible(
    dbsn_plot(x)
  )

})


test_that("dbsn_plot supports POLYGON geometries", {

  x <- sf::st_sf(
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

  expect_invisible(
    dbsn_plot(x)
  )

})


test_that("dbsn_plot supports MULTIPOINT geometries", {

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_multipoint(
        matrix(
          c(
            0, 0,
            1000, 1000
          ),
          ncol = 2,
          byrow = TRUE
        )
      ),
      crs = 6875
    )
  )

  expect_invisible(
    dbsn_plot(x)
  )

})


test_that("dbsn_plot supports MULTILINESTRING geometries", {

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_multilinestring(
        list(
          matrix(
            c(
              0, 0,
              1000, 0
            ),
            ncol = 2,
            byrow = TRUE
          ),
          matrix(
            c(
              0, 1000,
              1000, 1000
            ),
            ncol = 2,
            byrow = TRUE
          )
        )
      ),
      crs = 6875
    )
  )

  expect_invisible(
    dbsn_plot(x)
  )

})


test_that("dbsn_plot supports MULTIPOLYGON geometries", {

  p1 <- list(
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

  p2 <- list(
    matrix(
      c(
        2000, 0,
        3000, 0,
        3000, 1000,
        2000, 1000,
        2000, 0
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_multipolygon(
        list(p1, p2)
      ),
      crs = 6875
    )
  )

  expect_invisible(
    dbsn_plot(x)
  )

})


test_that("dbsn_plot rejects non-sf objects", {

  expect_error(
    dbsn_plot(
      data.frame(x = 1)
    ),
    "`x` must be an sf object"
  )

})


test_that("dbsn_plot rejects empty sf objects", {

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      crs = 6875
    )
  )

  expect_error(
    dbsn_plot(x),
    "contains no features"
  )

})
