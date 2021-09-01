test_that("get bird presence absence works", {
  Sys.setenv("DEBUG" = TRUE)
  results <- get_bird_presence_absence(bird_taxa_id = 188)
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 10)
})

test_that("get bird presence fails without id_taxa", {
  testthat::expect_error(
    get_bird_presence_absence()
    )
})