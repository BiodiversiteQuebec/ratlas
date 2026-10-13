test_that("get observations works", {
  results <- get_observations(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true("sf" %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("combining filters works", {
  results <- get_observations(year_obs = 2021, id_taxa_obs = 983253, id_datasets = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10)
  testthat::expect_true(nrow(results) > 1)
})

test_that("no geometry", {
  results <- get_observations(limit = 10, geometry = FALSE)
  testthat::expect_type(results, "list")
  testthat::expect_false("sf" %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("within quebec", {
  results <- get_observations(limit = 10, within_quebec = TRUE)
  testthat::expect_true(nrow(results) == 10)
  testthat::expect_true(all(results$within_quebec == TRUE))
})

# Test with id_region as parameter
test_that("get observations with id_region works", {
  results <- get_observations(region_fid = 858284, year = 2013, limit = 10)
  testthat::expect_true(nrow(results) > 1)
})

test_that("no limit fetches every page without duplicates", {
  results <- get_observations(id_taxa_obs = 983253, year_obs = 2021)
  testthat::expect_true(nrow(results) > 0)
  testthat::expect_equal(anyDuplicated(results$id), 0)
})

# Polygon filter
sherbrooke <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = -72.0, ymin = 45.35, xmax = -71.8, ymax = 45.45), crs = 4326
))

test_that("polygon returns only observations inside it", {
  results <- get_observations(polygon = sherbrooke, year = 2020, limit = 100)
  testthat::expect_true("sf" %in% class(results))
  testthat::expect_equal(nrow(results), 100)

  # Planar check, like PostGIS. With s2, edges are great circles and points
  # close to the boundary can fall outside.
  old <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old)))
  testthat::expect_true(all(lengths(sf::st_intersects(results, sherbrooke)) > 0))
})

test_that("polygon in another CRS gives the same observations", {
  results_wgs <- get_observations(polygon = sherbrooke, year = 2020, limit = 100)
  results_lcc <- get_observations(polygon = sf::st_transform(sherbrooke, 32198), year = 2020, limit = 100)
  testthat::expect_setequal(results_lcc$id, results_wgs$id)
})

test_that("polygon without limit fetches every page without duplicates", {
  results <- get_observations(polygon = sherbrooke, year = 2020, month_obs = 6)
  testthat::expect_true(nrow(results) > 0)
  testthat::expect_equal(anyDuplicated(results$id), 0)
})

test_that("polygon combines with other filters", {
  sample <- get_observations(polygon = sherbrooke, year = 2020, limit = 1)
  taxon <- sample$id_taxa_obs[1]

  results <- get_observations(polygon = sherbrooke, year = 2020, id_taxa_obs = taxon, limit = 50)
  testthat::expect_true(nrow(results) > 0)
  testthat::expect_true(all(results$id_taxa_obs == taxon))
  testthat::expect_true(all(results$year_obs == 2020))
})

test_that("polygon without geometry returns a tibble", {
  results <- get_observations(polygon = sherbrooke, year = 2020, limit = 10, geometry = FALSE)
  testthat::expect_false("sf" %in% class(results))
  testthat::expect_false(any(grepl("^geom", names(results))))
  testthat::expect_equal(nrow(results), 10)
})

test_that("polygon with no match returns an empty tibble", {
  atlantic <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -40, ymin = 35, xmax = -39, ymax = 36), crs = 4326
  ))
  with_limit <- get_observations(polygon = atlantic, limit = 10)
  without_limit <- get_observations(polygon = atlantic)
  testthat::expect_s3_class(with_limit, "tbl_df")
  testthat::expect_equal(nrow(with_limit), 0)
  testthat::expect_equal(nrow(without_limit), 0)
})

test_that("polygon and region_fid cannot be combined", {
  testthat::expect_error(
    get_observations(polygon = sherbrooke, region_fid = 858284),
    "not both"
  )
})