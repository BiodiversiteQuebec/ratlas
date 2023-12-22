# Create random data
random_value <- list(
  "integer" = function(int_range, ...) {
    ceiling(
      runif(1, int_range[[1]], int_range[[2]])
    )
  },
  "numeric" = function(...) runif(1, 1, 3000),
  "character" = function(...) paste(sample(letters, 16, TRUE), collapse = ""),
  "logical" = function(...) runif(1, 0, 2) > 1,
  "geometry" = function(...) {
    sf::st_as_text(
      sf::st_point(c(runif(1, -79, -57), runif(1, 45, 62)))
    )
  },
  "date" = function(...) {
    as.Date.numeric(
      runif(1, -25567, 18627),
      origin = "1970-01-01"
    )
  },
  "time" = function(...) {
    paste(
      formatC(runif(3, 0, 24), width = 2, format = "d", flag = "0"),
      collapse = ":"
    )
  },
  "enum_qc" = function(...) NULL,
  "enum_ranks" = function(...) "species",
  "enum_sp_categories" = function(...) NULL
)

schema_tables <- list(
  observations = list(
    id = list(type = "integer"),
    org_parent_event = list(type = "character"),
    org_event = list(type = "character"),
    org_id_obs = list(type = "character"),
    id_datasets = list(type = "integer", int_range = c(1, 500)),
    year_obs = list(type = "integer"),
    month_obs = list(type = "integer"),
    day_obs = list(type = "integer"),
    time_obs = list(type = "time"),
    id_taxa = list(type = "integer", int_range = c(1, 4365)),
    id_variables = list(type = "integer", int_range = c(1, 9)),
    obs_value = list(type = "numeric"),
    issue = list(type = "logical"),
    geom = list(type = "geometry")
  ),
  taxa = list(
        id = list(type = "integer"),
        scientific_name = list(type = "character"),
        rank = list(type = "enum_ranks"),
        valid = list(type = "logical"),
        valid_taxa_id = list(type = "integer"),
        gbif = list(type = "integer"),
        col = list(type = "integer"),
        family = list(type = "character"),
        species_gr = list(type = "enum_sp_categories"),
        authorship = list(type = "character"),
        updated_at = list(type = "date"),
        qc_status = list(type = "enum_qc"),
        exotic = list(type = "logical")
    )
)
random_data <- function(table_name, nrows) {
  row_schema <- schema_tables[[table_name]]
  out <- list()
  for (i in 1:nrows) {
    row <- lapply(
      row_schema, function(column) {
        if (is.null(column[["int_range"]])) column[["int_range"]] <- c(1, 6000)
        random_value[[column[["type"]]]](column[["int_range"]])
      }
    )
    row["id"] <- NULL
    out <- dplyr::bind_rows(out, row)
  }
  return(out)
}

test_that("injection observations works", {
  # singleton, list
  table_name <- "observations"
  data <- as.list(random_data(table_name, 1))
  response <- db_write_table(table_name, data, host = "https://staging.biodiversite-quebec.ca/api/v2")
  testthat::expect_true(httr2::resp_status(response) == 201)

  # singleton, data.frame
  data <- random_data(table_name, 1)
  response <- db_write_table(table_name, data, host = "https://staging.biodiversite-quebec.ca/api/v2")
  testthat::expect_true(httr2::resp_status(response) == 201)

  # multiple lines, data.frame
  data <- random_data(table_name, 20)
  response <- db_write_table(table_name, data, host = "https://staging.biodiversite-quebec.ca/api/v2")
  testthat::expect_true(httr2::resp_status(response) == 201)
})

test_that("injection taxa works", {
  # singleton, list
  table_name <- "taxa"
  data <- as.list(random_data(table_name, 1))
  response <- db_write_table(table_name, data, host = "https://staging.biodiversite-quebec.ca/api/v2")
  testthat::expect_true(httr2::resp_status(response) == 201)
})

test_that("Duplicate rows fails", {
  data <- list(
    id = 39363658L,
    org_parent_event = NA,
    org_event = "S5637032",
    org_id_obs = "URN:CornellLabOfOrnithology:EBIRD:OBS79380280",
    id_datasets = 61L,
    year_obs = 2009L,
    month_obs = 11L,
    day_obs = 24L,
    time_obs = "09:15:00",
    id_taxa = 216L,
    id_variables = 1L,
    obs_value = 1L,
    issue = NA,
    geom = sf::st_as_text(sf::st_point(c(-77.5786, 43.0629)))
  )
  testthat::expect_error(
    db_write_table("observations", data, host = "https://staging.biodiversite-quebec.ca/api/v2")
    )
})

test_that("Unexistant foreign key fails", {
  data <- random_data('observations', 1)
  data[1, 'id_variables'] = 30000
  testthat::expect_error(
    db_write_table("observations", data, host = "https://staging.biodiversite-quebec.ca/api/v2")
    )
  })
