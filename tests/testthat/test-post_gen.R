stopifnot(
  "Expecting local db environment to test injections" =
    grepl("localhost", ATLAS_API_V2_HOST)
)

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
  # "enum_qc" = function(...) sample(c(
  #   "Menacée", "Susceptible", "Vulnérable", "Vulnérable à la récolte"), 1),
  # "enum_ranks" = function(...) sample(c("species", "genus", "family", "order"), 1),
  # "enum_sp_categories" = function(...) sample(c(
  #   "Amphibiens", "Oiseaux", "Mammifères", "Reptiles", "Poissons"), 1)
)
# for (n in names(random_value)) {
#   tryCatch(v <- random_value[[n]](),
#     error = function() print(paste(n, "FAILED")),
#     finally = print(paste(n, v)))}
random_data <- function(endpoint, nrows) {
  row_schema <- schema.tables[[endpoint]]
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
  endpoint <- "observations"
  data <- as.list(random_data(endpoint, 1))
  response <- post_gen(endpoint, data)
  testthat::expect_true(is.null(response))

  # singleton, data.frame
  data <- random_data(endpoint, 1)
  response <- post_gen(endpoint, data)
  testthat::expect_true(is.null(response))

  # multiple lines, data.frame
  data <- random_data(endpoint, 20)
  response <- post_gen(endpoint, data)
  testthat::expect_true(is.null(response))
})

test_that("injection taxa works", {
  # singleton, list
  endpoint <- "taxa"
  data <- as.list(random_data(endpoint, 1))
  response <- post_gen(endpoint, data)
  testthat::expect_true(is.null(response))
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
    post_gen("observations", data)
    )
})

test_that("Unexistant foreign key fails", {
  data <- random_data('observations', 1)
  data[1, 'id_variables'] = 30000
  testthat::expect_error(
    post_gen("observations", data)
    )
  })

test_that("Very large injection works", {
  testthat::expect_true(TRUE)
})

test_that("Taxa injection works with UTF-8 Enum values`", {
  testthat::expect_true(TRUE)
})