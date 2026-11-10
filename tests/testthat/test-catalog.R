test_that("dbsn_catalog returns the LOC_SG catalogue", {

  x <- dbsn_catalog("loc_sg")

  expect_type(x, "list")
  expect_equal(nrow(x$classe), 1)
  expect_equal(nrow(x$attributi), 5)
  expect_equal(nrow(x$domini), 58)

})
