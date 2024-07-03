# usethis::create_package("~/dev/ratlas")
usethis::use_build_ignore("dev.R")
usethis::use_build_ignore("dev_scratch")
usethis::use_mit_license(copyright_holder = "Vincent Beauregard")

usethis::use_dev_package("usethis", type = "Suggests", remote = NULL)
usethis::use_dev_package("testthat", type = "Suggests", remote = NULL)
usethis::use_dev_package("devtools", type = "Imports", remote = NULL)
usethis::use_dev_package("pkgdown", type = "Suggests", remote = NULL)
usethis::use_dev_package("rmarkdown", type = "Suggests", remote = NULL)
# usethis::use_dev_package("pandoc", type = "Suggests", remote = NULL)

# Use packages for development in vscode environment
usethis::use_dev_package("languageserver", type = "Suggests", remote = NULL)
usethis::use_dev_package("vscDebugger", type = "Suggests",
    remote = "github::ManuelHentschel/vscDebugger")

# Use packages for development using Jupyter
usethis::use_dev_package("IRkernel", type = "Suggests", remote = NULL)

# Package dependencies
usethis::use_package("dplyr", type = "Imports")
usethis::use_package("httr2", type = "Imports")
usethis::use_package("sf",  type = "Imports")
usethis::use_package("tidyr",  type = "Imports")
usethis::use_package("foreach",  type = "Imports")
usethis::use_package("doParallel", type = "Imports")

# Package modules and tests
usethis::use_r("db_read_table")
usethis::use_test("db_read_table")

usethis::use_r("db_write_table")
usethis::use_test("db_write_table")

usethis::use_r("db_call_function")
usethis::use_test("db_write_table")

usethis::use_r("get_datasets")
usethis::use_test("get_datasets")

usethis::use_r("get_taxa")
usethis::use_test("get_taxa")

usethis::use_r("get_observations")
usethis::use_test("get_observations")

usethis::use_r("get_timeseries")
usethis::use_test("get_timeseries")

usethis::use_r("get_regions")
usethis::use_test("get_regions")

# Create vignettes
usethis::use_vignette("download-obs")
usethis::use_vignette("download-regions-observations", title = "Download observations by region")
usethis::use_vignette("database-access-functions", title = "Functions to directly access the database ressources")

# Use pkgdown for package website
usethis::use_pkgdown()

# Use raw data
usethis::use_data_raw("taxa_data")
source("data-raw//taxa_data.R")

# Load, test and build package and documentation
devtools::load_all()
# devtools::test()
devtools::document()
devtools::install()
# rmarkdown::render("./vignettes/download-obs.Rmd")
# rmarkdown::render("./vignettes/download-regions-observations.Rmd")
# Render vignette "./vignettes/clara_region_bird_counts.Rmd" into pkgdown website
pkgdown::build_article("download-obs")
pkgdown::build_article("download-regions-observations")
pkgdown::build_article("database-access-functions")
pkgdown::build_articles_index()


pkgdown::build_site(examples = FALSE, lazy = TRUE)
