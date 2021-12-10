test_that("get montreal observations works", {
  results <- get_mtl_bird_observations(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) == 10)
})

test_that("combining filters works", {
  results <- get_mtl_bird_observations(year_obs = 2014, id_taxa = 6450)
  testthat::expect_true(nrow(results) > 1)
})

test_that("Bug geom empty returned 20211207", {
  obs <- get_mtl_bird_observations(
    id_taxa = 6414, year = 2010:2021)
  testthat::expect_true(nrow(obs) == 0)
})