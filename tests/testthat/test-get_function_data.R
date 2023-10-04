test_that("read function data works", {
  name = 'obs_summary'
  schema = 'atlas_api'
  output_flatten = FALSE
  
  data <- get_function_data(name, schema, output_flatten)
  expect_true(nrow(data) > 0)
})

test_that("read function data works with filter", {
  name = 'obs_summary'
  schema = 'atlas_api'
  output_flatten = FALSE
  region_fid = 749
  region_type = 'admin'

  data <- get_function_data(name, schema, output_flatten,
                             region_fid = region_fid,
                             region_type = region_type)
  expect_true(nrow(data) > 0)
})