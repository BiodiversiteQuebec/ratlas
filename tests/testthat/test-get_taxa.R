test_that("get taxa works", {
  results <- get_taxa()
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 100)
})

test_that("filter by id works", {
  testthat::expect_true(nrow(get_taxa(id = 6450)) == 1)
  testthat::expect_true(nrow(get_taxa(id = c(6450, 8354))) == 2)
})

test_that("filter by scientific_name works", {
  results <- get_taxa(scientific_name = "Cyanocitta cristata")
  testthat::expect_true(nrow(results) == 1)
  results <- get_taxa(scientific_name = c(
    "Cyanocitta cristata",
    "Falco peregrinus"))
  testthat::expect_true(nrow(results) == 2)
})
