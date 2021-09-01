test_that("get timeseries works", {
  results <- get_timeseries(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) == 10)
})