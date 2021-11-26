test_that("get mtl bird presence absence works", {
  results <- get_mtl_bird_presence_absence(taxa_name = 'Spinus tristis')
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 10)
})


test_that("get bird presence fails without taxa_name", {
  testthat::expect_error(
    get_mtl_bird_presence_absence()
    )
})