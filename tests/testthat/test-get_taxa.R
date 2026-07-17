test_that("get taxa fails with no input", {
  testthat::expect_error(get_taxa(), regexp = "Bad input")
})

test_that("get taxa fails with non accepted group_name value", {
  testthat::expect_error(get_taxa(group_name = "Amphibiens"), regexp = "Bad input")
})

test_that("Bad name errors with no match", {
  testthat::expect_error(get_taxa(scientific_name = "Binto"), regexp = "No match found")
})

test_that("Bad name warning with other valid names", {
  testthat::expect_warning(get_taxa(scientific_name = c("Binto", "Dryobates villosus")), regexp = "No match found for Binto")
})

test_that("filter by id works", {
  testthat::expect_true(nrow(get_taxa(id = 6450)) == 1)
  testthat::expect_true(nrow(get_taxa(id = c(6450, 8354))) == 2)
})

test_that("filter by scientific_name works", {
  results <- get_taxa(scientific_name = "Cyanocitta cristata")
  testthat::expect_true(length(unique(results$valid_scientific_name)) == 1)
  results <- get_taxa(scientific_name = c("Cyanocitta cristata", "Dryobates villosus"))
  testthat::expect_true(length(unique(results$valid_scientific_name)) == 2)
})

test_that("Long list of `id_taxa_obs` works", {
  results <- get_taxa(scientific_name = "Plantae")
  testthat::expect_true(nrow(results) > 0)
})
