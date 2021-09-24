test_that("taxa works", {
  taxa <- Taxa(
    scientific_name = "Cyanocitta cristata",
    rank = "species",
    valid = TRUE,
    valid_taxa_id = NULL,
    authorship = "Linnaeus, 1758",
    gbif = NULL,
    col = 123,
    family = "Oiseaux"
    # qc_status = NULL,
    # exotic = NULL
  )
  testthat::expect_equal(length(taxa), 12)
  })

test_that("taxa from global names works", {
  taxa <- Taxa.from_global_names("Antigone canadensis")
  testthat::expect_false(taxa$valid)
  testthat::expect_false(is.null(taxa$valid_taxa_id))
  testthat::expect_false(is.null(taxa$gbif))

  taxa <- Taxa.from_global_names("Cyanocitta cristata")
  testthat::expect_true(taxa$valid)
  testthat::expect_true(is.null(taxa$valid_taxa_id))
  testthat::expect_false(is.null(taxa$col))

  taxa <- Taxa.from_global_names("Symphyotrichum anticostense")
  testthat::expect_true(taxa$qc_status != "")

  taxa <- Taxa.from_global_names("Perdix perdix")
  testthat::expect_true(taxa$exotic)

  # taxa <- Taxa.from_global_names("Phragmites australis subs. australis")
  # testthat::expect_true(taxa$exotic)
  })