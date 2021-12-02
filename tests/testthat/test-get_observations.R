test_that("get observations works", {
  results <- get_observations(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) == 10)
})

test_that("combining filters works", {
  results <- get_observations(year_obs = 2014, id_taxa = 6450, id_datasets = 70)
  testthat::expect_true(nrow(results) > 1)
})
