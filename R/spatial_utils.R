as_wkt_polygon <- function(polygon) {
  if (!inherits(polygon, c("sf", "sfc", "bbox"))) stop("Bad input: `polygon` must be an sf, sfc or bbox object")
  if (inherits(polygon, "bbox")) polygon <- sf::st_as_sfc(polygon)

  geom <- sf::st_geometry(polygon)
  if (is.na(sf::st_crs(geom))) stop("Bad input: `polygon` has no CRS")

  # Repair and merge in planar lon/lat, like PostGIS does with geometry(4326).
  # Dropping the CRS sends these steps to GEOS instead of s2.
  geom <- sf::st_transform(geom, 4326) |>
    sf::st_set_crs(NA) |>
    sf::st_make_valid() |>
    sf::st_union() |>
    sf::st_set_crs(4326)

  if (!as.character(sf::st_geometry_type(geom)) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("Bad input: `polygon` must be a POLYGON or MULTIPOLYGON")
  }
  sf::st_as_text(geom, digits = 15)
}