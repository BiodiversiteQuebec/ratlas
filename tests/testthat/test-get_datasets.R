test_that("Return all datasets works", {
  results <- get_datasets(limit = 5)
  testthat::expect_true(nrow(results) > 1)

})

test_that("filter by id works", {
  testthat::expect_true(nrow(get_datasets(id = "adb12ba6-4e55-4e4a-8a63-235c48be7865")) == 1)
  testthat::expect_true(nrow(get_datasets(id = c("adb12ba6-4e55-4e4a-8a63-235c48be7865", "94890ff9-7da0-4547-88e2-98054e7e3c4f"))) == 2)
})

test_that("filter by table col works", {
  results <- get_datasets(source_alias = "POE")
  testthat::expect_true(nrow(results) == 1)
})