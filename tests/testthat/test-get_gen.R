test_that("taxa endpoint works", {
  results <- get_gen(endpoint = "taxa")
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 100)
})

test_that("pagination works", {
  results <- get_gen(endpoint = "taxa", .page_count = 100)
  testthat::expect_true(nrow(results) > 100)
})

test_that("no pagination on limited query", {
  results <- get_gen(endpoint = "taxa", limit = 100)
  testthat::expect_true(nrow(results) == 100)
})

test_that("error on bad schema argument", {
  testthat::expect_error(
    get_gen(endpoint = "taxa", .schema = "bad_schema"),
    regexp = "Bad input")
})

test_that("endpoint in api schema works", {
  results <- get_gen(
    endpoint = "bird_sampling_points",
    .schema = "api",
    limit = 100)
  testthat::expect_true(nrow(results) == 100)
})