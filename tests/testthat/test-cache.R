test_that("dbsn_cache_dir returns a valid cache directory", {

  path <- dbsn_cache_dir()

  expect_type(path, "character")
  expect_length(path, 1)
  expect_true(dir.exists(path))

})
