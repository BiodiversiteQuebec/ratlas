test_that("POST server response parsed correctly", {

  fake_response <- httr2::response(status_code = 201, method = "POST",
    url = "https://staging.biodiversite-quebec.ca/api/v2/taxa_obs",
  )

  out <- postgrest_resp_to_data(fake_response)

  testthat::expect_true(out$method == "POST")
  testthat::expect_true(httr2::resp_status(out) == 201)
})
