testthat::test_that("get bird occurrences", {
  results <- ratlas::get_bird_occurrences(192)
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) >= 1)
  testthat::expect_true("sfc" %in% class(results$geom[1]))
})