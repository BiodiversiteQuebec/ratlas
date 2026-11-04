test_that("read function data works", {
  function_name <- "obs_summary"
  schema <- "atlas_api"
  output_flatten <- FALSE

  data <- db_call_function(function_name, schema, output_flatten)
  expect_true(nrow(data) > 0)
})

test_that("read function data works with filter", {
  function_name <- "obs_summary"
  schema <- "atlas_api"
  output_flatten <- FALSE
  id_regions <- 749
  region_type <- "admin"

  data <- db_call_function(function_name, schema, output_flatten,
                           id_regions = id_regions,
                           region_type = region_type)
  expect_true(nrow(data) > 0)
})