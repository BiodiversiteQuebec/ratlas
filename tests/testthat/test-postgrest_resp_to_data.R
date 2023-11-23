test_that("POST server response parsed correctly", {

  # Generate a fake server response following a successful POST
  fake_response <- list(
    url = "https://staging.biodiversite-quebec.ca/api/v2/datasets",
    status_code = 201,
    headers = list("Content-Type" = "<unknown>"),
    times = list(),
    content = raw(),
    response = raw(),
    cookies = list(),
    config = list()
  )
  # Set the class to "response" to simulate an httr response object
  class(fake_response) <- "response"

  out <- postgrest_resp_to_data(fake_response)

  testthat::expect_type(out, "character")
  testthat::expect_true(length(out) == 1)
})
