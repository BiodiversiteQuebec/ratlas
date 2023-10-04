test_that("get observations works", {
  results <- get_observations(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true('sf' %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("combining filters works", {
  results <- get_observations(year_obs = 2014, id_taxa = 6450, id_datasets = 70)
  testthat::expect_true(nrow(results) > 1)
})

test_that("no geometry", {
  results <- get_observations(limit = 10, geometry = FALSE)
  testthat::expect_type(results, "list")
  testthat::expect_false('sf' %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("within quebec", {
  results <- get_observations(limit = 10, within_quebec = TRUE)
  testthat::expect_true(nrow(results) == 10)
  testthat::expect_true(all(results$within_quebec == TRUE))
})


# Test with id_region as parameter
test_that("get observations with id_region works", {
  results <- get_observations(region_fid = 858284, year = 2013)
  testthat::expect_true(nrow(results) > 1)
})