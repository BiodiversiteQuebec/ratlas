test_that("get observations works", {
  results <- get_observations(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true("sf" %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("combining filters works", {
  results <- get_observations(year_obs = 2021, id_taxa_obs = 983253, id_datasets = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10)
  testthat::expect_true(nrow(results) > 1)
})

test_that("no geometry", {
  results <- get_observations(limit = 10, geometry = FALSE)
  testthat::expect_type(results, "list")
  testthat::expect_false("sf" %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("within quebec", {
  results <- get_observations(limit = 10, within_quebec = TRUE)
  testthat::expect_true(nrow(results) == 10)
  testthat::expect_true(all(results$within_quebec == TRUE))
})


# Test with id_region as parameter
test_that("get observations with id_region works", {
  results <- get_observations(region_fid = 858284, year = 2013, limit = 10)
  testthat::expect_true(nrow(results) > 1)
})