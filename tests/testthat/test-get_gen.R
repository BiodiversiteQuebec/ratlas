test_that("taxa endpoint works", {
  results <- get_gen(endpoint = "taxa")
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 100)
})

test_that("pagination works", {
  results <- get_gen(endpoint = "taxa", .page_limit = 1000)
  testthat::expect_true(nrow(results) > 1000)
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

test_that("endpoint in api schema works", {
  results <- get_gen(
    endpoint = "bird_sampling_points",
    .schema = "api",
    limit = 100)
  testthat::expect_true(nrow(results) == 100)
})

test_that("filter by column works", {
  results <- get_gen(
    endpoint = "taxa",
    id = 100)
  testthat::expect_equal(nrow(results), 1)

  results <- get_gen(
    endpoint = "taxa",
    id = c(100, 101))
  testthat::expect_equal(nrow(results), 2)
})

test_that("NULL parameters are not passed", {
  results <- get_gen(
    endpoint = "observations",
    month_obs = NULL,
    limit=1)
  testthat::expect_equal(nrow(results), 1)
})