test_that("get list of taxa ref", {
  taxa <- get_taxa_global_names(c("Cyanocitta cristata", "Grus canadensis"))
  testthat::expect_true(length(taxa) == 2)
})

test_that("get singleton of taxa ref", {
  taxa <- get_taxa_global_names("Cyanocitta cristata")
  testthat::expect_true(length(taxa) == 1)
  pref_results <- dplyr::bind_rows(taxa[[1]]$preferredResults)
  testthat::expect_true(
    all(pref_results$dataSourceTitleShort == c(
      "Catalogue of Life", "GBIF Backbone Taxonomy"))
      )
})