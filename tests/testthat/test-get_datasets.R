test_that("Return all datasets works", {
  results <- get_datasets()
  testthat::expect_true(nrow(results) > 1)

})

test_that("filter by id works", {
  testthat::expect_true(nrow(get_datasets(id = 100)) == 1)
  testthat::expect_true(nrow(get_datasets(id = c(100, 101))) == 2)
})

test_that("filter by table col works", {
  results <- get_datasets(original_source = "eBird")
  testthat::expect_true(nrow(results) > 1)
})