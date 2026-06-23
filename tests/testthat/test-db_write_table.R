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
  "enum_ranks" = function(...) "species"
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
    id_taxa_obs = list(type = "integer", int_range = c(1, 4365)),
    id_variables = list(type = "integer", int_range = c(1, 9)),
    obs_value = list(type = "numeric"),
    issue = list(type = "logical"),
    geom = list(type = "geometry")
  ),
  taxa_obs = list(
    id = list(type = "integer"),
    scientific_name = list(type = "character"),
    rank = list(type = "enum_ranks"),
    authorship = list(type = "character"),
    parent_scientific_name = list(type = "character"),
    updated_at = list(type = "date")
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

# Dummy host/token. The mock layer intercepts every request inside the block
# before it leaves the machine, so nothing is ever sent over the network. We
# deliberately use a non-routable dummy host (NOT the real staging URL) so that
# if a mock were ever mis-scoped, the test fails to connect instead of writing
# to a real database. We only ever assert on the *constructed* request, never
# connect to this host.
TEST_HOST <- "https://dummy.test/api/v2"
TEST_TOKEN <- "fake-test-token"

capturing_mock <- function(env) {
  function(req) {
    env$req <- req
    httr2::response(
      status_code = 201,
      method = "POST",
      headers = list("content-type" = "application/json"),
      body = raw(0)
    )
  }
}

# Mock that returns a PostgREST-style error response.
error_mock <- function(status_code, body) {
  function(req) {
    httr2::response_json(status_code = status_code, body = body)
  }
}

test_that("POST request to observations is built correctly", {
  env <- new.env()

  # singleton, list
  data <- as.list(random_data("observations", 1))
  response <- httr2::with_mocked_responses(
    mock = capturing_mock(env),
    db_write_table("observations", data, .host = TEST_HOST, .token = TEST_TOKEN)
  )

  # Response is surfaced (empty POST body -> raw response returned)
  testthat::expect_true(httr2::resp_status(response) == 201)

  # Request was built as expected. The URL carries the pagination query
  # (?limit=...), so match the path rather than anchoring on end-of-string.
  testthat::expect_match(env$req$url, "/observations(\\?|$)")
  # postgrest_post() does not set $method explicitly; httr2 infers POST from the
  # JSON body at perform time, so query the effective method via req_get_method.
  testthat::expect_equal(httr2::req_get_method(env$req), "POST")
  testthat::expect_equal(env$req$body$type, "json")
  testthat::expect_equal(
    httr2::req_get_headers(env$req, "reveal")$Authorization,
    paste("Bearer", TEST_TOKEN)
  )
  # Default schema is sent as the Content-Profile header
  testthat::expect_equal(env$req$headers$`Content-Profile`, "public")
})

test_that("data.frame payloads (single and multiple rows) are sent verbatim", {
  env <- new.env()

  # singleton, data.frame
  data <- random_data("observations", 1)
  httr2::with_mocked_responses(
    mock = capturing_mock(env),
    db_write_table("observations", data, .host = TEST_HOST, .token = TEST_TOKEN)
  )
  testthat::expect_equal(nrow(env$req$body$data), 1)

  # multiple lines, data.frame
  data <- random_data("observations", 20)
  httr2::with_mocked_responses(
    mock = capturing_mock(env),
    db_write_table("observations", data, .host = TEST_HOST, .token = TEST_TOKEN)
  )
  testthat::expect_equal(nrow(env$req$body$data), 20)
})

test_that("POST request to taxa_obs is built correctly", {
  env <- new.env()
  data <- as.list(random_data("taxa_obs", 1))
  response <- httr2::with_mocked_responses(
    mock = capturing_mock(env),
    db_write_table("taxa_obs", data, .host = TEST_HOST, .token = TEST_TOKEN)
  )
  testthat::expect_true(httr2::resp_status(response) == 201)
  testthat::expect_match(env$req$url, "/taxa_obs(\\?|$)")
})

test_that("schema argument is passed as the Content-Profile header", {
  env <- new.env()
  data <- as.list(random_data("taxa_obs", 1))
  httr2::with_mocked_responses(
    mock = capturing_mock(env),
    db_write_table("taxa_obs", data, schema = "api",
                   .host = TEST_HOST, .token = TEST_TOKEN)
  )
  testthat::expect_equal(env$req$headers$`Content-Profile`, "api")
})

test_that("bad schema argument fails before any request is made", {
  testthat::expect_error(
    db_write_table("observations", list(), schema = "bad_schema",
                   .host = TEST_HOST, .token = TEST_TOKEN),
    regexp = "Bad input"
  )
})

test_that("Duplicate rows fails", {
  # PostgREST returns 409 with a unique-violation payload on duplicate keys
  data <- as.list(random_data("observations", 1))
  testthat::expect_error(
    httr2::with_mocked_responses(
      mock = error_mock(
        409,
        list(
          code = "23505",
          message = "duplicate key value violates unique constraint",
          details = "Key (id)=(39363658) already exists."
        )
      ),
      db_write_table("observations", data, .host = TEST_HOST, .token = TEST_TOKEN)
    ),
    regexp = "HTTP error: 409"
  )
})

test_that("Unexistant foreign key fails", {
  # PostgREST returns 409 with a foreign-key-violation payload
  data <- random_data("observations", 1)
  data[1, "id_variables"] <- 30000
  testthat::expect_error(
    httr2::with_mocked_responses(
      mock = error_mock(
        409,
        list(
          code = "23503",
          message = "insert or update on table violates foreign key constraint",
          details = "Key (id_variables)=(30000) is not present in table \"variables\"."
        )
      ),
      db_write_table("observations", data, .host = TEST_HOST, .token = TEST_TOKEN)
    ),
    regexp = "HTTP error: 409"
  )
})