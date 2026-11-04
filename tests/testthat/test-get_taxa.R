test_that("get taxa fails with no input", {
  testthat::expect_error(get_taxa(), regexp = "Bad input")
})

test_that("get taxa fails with non accepted group_name value", {
  testthat::expect_error(get_taxa(group_name = "Amphibiens"), regexp = "Bad input")
})

test_that("Bad name errors with no match and no warnings", {
  testthat::expect_no_warning(
    testthat::expect_error(get_taxa(scientific_name = "Binto"), regexp = "No match found for Binto")
  )
})

test_that("Bad name warning with other valid names", {
  testthat::expect_warning(get_taxa(scientific_name = c("Binto", "Dryobates villosus")), regexp = "No match found for Binto")
})

test_that("filter by id works", {
  testthat::expect_true(nrow(get_taxa(id_taxa_obs = 6450)) == 1)
  testthat::expect_true(nrow(get_taxa(id_taxa_obs = c(6450, 8354))) == 2)
})

test_that("filter by scientific_name works", {
  results <- get_taxa(scientific_name = "Cyanocitta cristata")
  testthat::expect_true(length(unique(results$valid_scientific_name)) == 1)
  results <- get_taxa(scientific_name = c("Cyanocitta cristata", "Dryobates villosus"))
  testthat::expect_true(length(unique(results$valid_scientific_name)) == 2)
})

test_that("filter by vernacular_name works", {
  results_fr <- get_taxa(vernacular_name = "Pic chevelu")
  results_en <- get_taxa(vernacular_name = "Hairy woodpecker (eastern)")
  testthat::expect_true(all(results_fr$valid_scientific_name == "Dryobates villosus"))
  testthat::expect_true(all(results_en$valid_scientific_name == "Dryobates villosus"))
})

test_that("Long list of `id_taxa_obs` works", {
  results <- get_taxa(scientific_name = "Plantae")
  testthat::expect_true(nrow(results) > 0)
})

test_that("Synonyms resolve to valid names", {
  results <- get_taxa(scientific_name = "Picoides villosus")
  testthat::expect_true(all(results$valid_scientific_name == "Dryobates villosus"))
  testthat::expect_true("Picoides villosus" %in% results$observed_scientific_name)
})

test_that("Partial matches work", {
  testthat::expect_message(results <- get_taxa(scientific_name = "Leuconotopicus vi"), "Matched")
  testthat::expect_true(all(c("Leuconotopicus villosus", "Dryobates villosus", "Dendrocopos villosus", "Picoides villosus") %in% results$observed_scientific_name))
  testthat::expect_true(all(results$valid_scientific_name == "Dryobates villosus"))
})

test_that("Ambiguous names error out", {
  testthat::expect_error(get_taxa(scientific_name = "Cani"), "ambiguous")
})

test_that("Exact matches win over partial matches", {
  testthat::expect_no_message(results <- get_taxa(scientific_name = "Canis"))
  testthat::expect_true("Canis" %in% results$valid_scientific_name)
})

test_that("Synonyms don't produce duplicate rows", {
  one_name <- get_taxa(scientific_name = "Dryobates villosus")
  two_name <- get_taxa(scientific_name = c("Dryobates villosus", "Leuconotopicus villosus"))
  testthat::expect_equal(nrow(one_name), nrow(two_name))
})

test_that("Filters with ... works correctly on names", {
  all_canis <- get_taxa(scientific_name = "Canis")
  genus_canis <- get_taxa(scientific_name = "Canis", rank = "genus")
  testthat::expect_true(nrow(genus_canis) < nrow(all_canis))
  testthat::expect_true(all(genus_canis$rank == "genus"))
})

test_that("Filters with ... works correctly on group_name", {
  all_amphibians <- get_taxa(group_name = "AMPHIBIANS")
  genus_amphibians <- get_taxa(group_name = "AMPHIBIANS", rank = "genus")
  testthat::expect_true(nrow(genus_amphibians) < nrow(all_amphibians))
  testthat::expect_true(all(genus_amphibians$rank == "genus"))
})

test_that("Different input parameters return same columns", {
  cols <- names(get_taxa(id_taxa_obs = 6450))
  testthat::expect_identical(names(get_taxa(scientific_name = "Dryobates villosus")), cols)
  testthat::expect_identical(names(get_taxa(group_name = "AMPHIBIANS")), cols)
})