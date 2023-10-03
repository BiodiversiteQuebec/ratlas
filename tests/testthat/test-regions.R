test_that("Single regions works", {
    results <- get_regions(fid = 858284)
    expect_true(nrow(results) == 1)

    # Expect sf object
    expect_true(any(class(results) == "sf"))
})

test_that("Multiple regions works", {
    results <- get_regions(fid = c(858284, 858285))
    expect_true(nrow(results) == 2)
})

test_that("Exclude geometry works", {
    results <- get_regions(fid = 858284, geometry = FALSE)
    expect_true(nrow(results) == 1)
    # No geom column
    expect_true(! "geom" %in% colnames(results))

    # Expect not sf object
    expect_true(all(class(results) != "sf"))
})