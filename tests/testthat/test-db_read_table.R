test_that("taxa endpoint works", {
  results <- db_read_table(table_name = "taxa")
  testthat::expect_type(results, "list")
  testthat::expect_true(nrow(results) > 100)
})

test_that("pagination works", {
  results <- db_read_table(table_name = "taxa", .page_limit = 1000)
  testthat::expect_true(nrow(results) > 1000)
})

test_that("no pagination on limited query", {
  results <- db_read_table(table_name = "taxa", limit = 100)
  testthat::expect_true(nrow(results) == 100)
})

test_that("error on bad schema argument", {
  testthat::expect_error(
    db_read_table(table_name = "taxa", schema = "bad_schema"),
    regexp = "Bad input")
})

test_that("endpoint in api schema works", {
  results <- db_read_table(
    table_name = "bird_sampling_points",
    schema = "api",
    limit = 100)
  testthat::expect_true(nrow(results) == 100)
})

test_that("endpoint in api schema works", {
  results <- db_read_table(
    table_name = "bird_sampling_points",
    schema = "api",
    limit = 100)
  testthat::expect_true(nrow(results) == 100)
})

test_that("filter by column works", {
  results <- db_read_table(
    table_name = "taxa",
    id = 100)
  testthat::expect_equal(nrow(results), 1)

  results <- db_read_table(
    table_name = "taxa",
    id = c(100, 101))
  testthat::expect_equal(nrow(results), 2)
})

test_that("NULL parameters are not passed", {
  results <- db_read_table(
    table_name = "observations",
    month_obs = NULL,
    limit=1)
  testthat::expect_equal(nrow(results), 1)
})

test_that("select works", {
  results <- db_read_table(
    table_name = "taxa",
    select = c("id", "scientific_name"),
    limit = 1)
  testthat::expect_equal(nrow(results), 1)
  testthat::expect_equal(ncol(results), 2)
})
