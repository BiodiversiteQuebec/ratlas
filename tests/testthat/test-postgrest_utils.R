test_that("whole numbers are not sent in scientific notation", {
  testthat::expect_equal(postgrest_escape(1e5), "100000")
  testthat::expect_equal(postgrest_escape(c(1e6, 2.5)), c("1000000", "2.5"))
  testthat::expect_equal(postgrest_query_filter(list(id_taxa_obs = 100000))$id_taxa_obs, "eq.100000")
})
