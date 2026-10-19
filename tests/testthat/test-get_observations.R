test_that("get observations works", {
  results <- get_observations(limit = 10)
  testthat::expect_type(results, "list")
  testthat::expect_true("sf" %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

test_that("combining filters works", {
  # year_obs is passed through `...` on purpose: it must not be dropped
  # by the named `year` argument being NULL
  results <- get_observations(year_obs = 2021, id_taxa_obs = 983253, id_datasets = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10)
  testthat::expect_true(nrow(results) > 1)
  testthat::expect_true(all(results$year_obs == 2021))
  testthat::expect_true(all(results$id_taxa_obs == 983253))
  testthat::expect_true(all(results$id_datasets == "4fa7b334-ce0d-4e88-aaae-2e0c138d049e"))
})

test_that("filter by id_observations works", {
  sample <- get_observations(year = 2021, limit = 2)
  results <- get_observations(id_observations = sample$id_observations)
  testthat::expect_setequal(results$id_observations, sample$id_observations)
})

test_that("filter by scientific_name works", {
  sample <- get_observations(year = 2021, limit = 1)
  name <- sample$valid_scientific_name[1]
  results <- get_observations(scientific_name = name, year = 2021, limit = 10)
  testthat::expect_true(nrow(results) > 0)
  testthat::expect_true(all(results$valid_scientific_name == name))
})

test_that("get_taxa() output is accepted for taxa parameters", {
  taxa <- get_taxa(scientific_name = "Cyanocitta cristata")

  by_name <- get_observations(scientific_name = taxa, limit = 10)
  testthat::expect_true(nrow(by_name) > 0)
  testthat::expect_true(all(by_name$valid_scientific_name %in% taxa$valid_scientific_name))

  by_id <- get_observations(id_taxa_obs = taxa, limit = 10)
  testthat::expect_true(nrow(by_id) > 0)
  testthat::expect_true(all(by_id$id_taxa_obs %in% taxa$id_taxa_obs))
})

test_that("data frame is accepted for id_datasets", {
  datasets <- data.frame(id_datasets = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
  results <- get_observations(id_datasets = datasets, limit = 10)
  testthat::expect_true(nrow(results) > 0)
  testthat::expect_true(all(results$id_datasets == datasets$id_datasets))
})

test_that("no geometry", {
  results <- get_observations(limit = 10, geometry = FALSE)
  testthat::expect_type(results, "list")
  testthat::expect_false("sf" %in% class(results))
  testthat::expect_true(nrow(results) == 10)
})

# Test with region_fid as parameter
test_that("get observations with region_fid works", {
  results <- get_observations(region_fid = 858284, year = 2013, limit = 10)
  testthat::expect_true(nrow(results) > 1)
  testthat::expect_true(all(results$year_obs == 2013))
})

test_that("region_fid without geometry returns a tibble", {
  results <- get_observations(region_fid = 858284, year = 2013, limit = 10, geometry = FALSE)
  testthat::expect_false("sf" %in% class(results))
  testthat::expect_false(any(grepl("^geom", names(results))))
  testthat::expect_equal(nrow(results), 10)
})

test_that("get_regions() output is accepted for region_fid", {
  regions <- get_regions(region_fid = 858284, geometry = FALSE)
  from_df <- get_observations(region_fid = regions, year = 2013, limit = 10)
  from_id <- get_observations(region_fid = 858284, year = 2013, limit = 10)
  testthat::expect_true(nrow(from_df) > 0)
  testthat::expect_setequal(from_df$id_observations, from_id$id_observations)
})

test_that("no limit fetches every page without duplicates", {
  results <- get_observations(id_taxa_obs = 929577, year_obs = 2008, .page_limit = 100)
  testthat::expect_true(nrow(results) > 100)
  testthat::expect_equal(anyDuplicated(results$id_observations), 0)
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
  testthat::expect_setequal(results_lcc$id_observations, results_wgs$id_observations)
})

test_that("polygon without limit fetches every page without duplicates", {
  results <- get_observations(polygon = sherbrooke, year = 2020, month_obs = 6)
  testthat::expect_true(nrow(results) > 0)
  testthat::expect_equal(anyDuplicated(results$id_observations), 0)
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

# Input validation (these fail before any request is sent)
test_that("polygon and region_fid cannot be combined", {
  testthat::expect_error(
    get_observations(polygon = sherbrooke, region_fid = 858284),
    "not both"
  )
})

test_that("id_taxa_obs and scientific_name cannot be combined", {
  testthat::expect_error(
    get_observations(id_taxa_obs = 983253, scientific_name = "Cyanocitta cristata"),
    "not both"
  )
})

test_that("data frame without the expected column is rejected", {
  wrong <- data.frame(foo = 1)
  testthat::expect_error(get_observations(region_fid = wrong), "region_fid")
  testthat::expect_error(get_observations(scientific_name = wrong), "valid_scientific_name")
  testthat::expect_error(get_observations(id_taxa_obs = wrong), "id_taxa_obs")
  testthat::expect_error(get_observations(id_datasets = wrong), "id_datasets")
})

test_that("empty values are rejected", {
  testthat::expect_error(get_observations(id_taxa_obs = integer(0)), "empty value for id_taxa_obs")
  testthat::expect_error(get_observations(id_taxa_obs = data.frame(id_taxa_obs = integer(0))), "empty value for id_taxa_obs")
  testthat::expect_error(get_observations(month_obs = NULL), "empty value for month_obs")
})
