test_that("List all taxa works", {
  results <- list_bird_taxa()
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 10)
})


test_that("Filter taxa by rank works", {
  results <- list_bird_taxa(rank = "species")
  testthat::expect_true(unique(results$rank) == "species")
})