make_box <- function(xmin, ymin, xmax, ymax, crs = 4326) {
  sf::st_as_sfc(sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                            crs = sf::st_crs(crs)))
}

make_poly <- function(...) sf::st_sfc(sf::st_polygon(list(...)), crs = 4326)

sherbrooke_box <- function() make_box(-72.0, 45.35, -71.8, 45.45)

test_that("bbox is converted to a WKT polygon", {
  bbox <- sf::st_bbox(c(xmin = -72, ymin = 45.35, xmax = -71.8, ymax = 45.45), crs = 4326)
  testthat::expect_equal(
    as_wkt_polygon(bbox),
    "POLYGON ((-72 45.35, -71.8 45.35, -71.8 45.45, -72 45.45, -72 45.35))"
  )
})

test_that("sf and sfc inputs give the same WKT", {
  box <- sherbrooke_box()
  testthat::expect_equal(as_wkt_polygon(sf::st_sf(a = 1, geometry = box)), as_wkt_polygon(box))
})

test_that("polygon in another CRS is transformed to 4326", {
  box <- sherbrooke_box()
  wkt_lcc <- as_wkt_polygon(sf::st_transform(box, 32198))
  coords <- sf::st_coordinates(sf::st_as_sfc(wkt_lcc, crs = 4326))
  testthat::expect_equal(range(coords[, "X"]), c(-72, -71.8), tolerance = 1e-8)
  testthat::expect_equal(range(coords[, "Y"]), c(45.35, 45.45), tolerance = 1e-8)
})

test_that("multiple features are merged into a MULTIPOLYGON", {
  two <- c(sherbrooke_box(), make_box(-71.30, 46.78, -71.15, 46.85))
  testthat::expect_match(as_wkt_polygon(two), "^MULTIPOLYGON")
})

test_that("holes are kept", {
  outer <- rbind(c(-72.0, 45.35), c(-71.8, 45.35), c(-71.8, 45.45), c(-72.0, 45.45), c(-72.0, 45.35))
  hole <- rbind(c(-71.95, 45.38), c(-71.95, 45.42), c(-71.85, 45.42), c(-71.85, 45.38), c(-71.95, 45.38))
  geom <- sf::st_as_sfc(as_wkt_polygon(make_poly(outer, hole)), crs = 4326)
  testthat::expect_length(geom[[1]], 2) # outer ring + hole
})

test_that("self-intersecting polygon is repaired", {
  bowtie <- make_poly(rbind(c(-72.0, 45.35), c(-71.8, 45.45), c(-71.8, 45.35), c(-72.0, 45.45), c(-72.0, 45.35)))
  wkt <- as_wkt_polygon(bowtie)
  testthat::expect_match(wkt, "^MULTIPOLYGON")
  testthat::expect_true(sf::st_is_valid(sf::st_as_sfc(wkt, crs = 4326)))
})

test_that("bad inputs are rejected", {
  box <- sherbrooke_box()
  testthat::expect_error(as_wkt_polygon(sf::st_set_crs(box, NA)), "no CRS")
  testthat::expect_error(
    as_wkt_polygon(sf::st_sfc(sf::st_point(c(-71.9, 45.4)), crs = 4326)),
    "POLYGON or MULTIPOLYGON"
  )
  testthat::expect_error(
    as_wkt_polygon("POLYGON((-72 45.35, -71.8 45.35, -71.8 45.45, -72 45.45, -72 45.35))"),
    "sf, sfc or bbox"
  )
})
