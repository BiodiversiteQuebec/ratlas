# usethis::create_package("~/dev/ratlas")
usethis::use_build_ignore("dev.R")
usethis::use_build_ignore("dev_scratch")
usethis::use_mit_license(copyright_holder = "Vincent Beauregard")

usethis::use_dev_package("usethis", type = "Imports", remote = NULL)
usethis::use_dev_package("testthat", type = "Imports", remote = NULL)
usethis::use_dev_package("devtools", type = "Imports", remote = NULL)
usethis::use_dev_package("IRkernel", type = "Imports", remote = NULL)
usethis::use_dev_package("languageserver", type = "Imports", remote = NULL)
usethis::use_dev_package("vscDebugger", type = "Imports",
    remote = "github::ManuelHentschel/vscDebugger")

usethis::use_package("dplyr", type = "Imports")
usethis::use_package("httr", type = "Imports")
usethis::use_package("sf",  type = "Imports")
usethis::use_package("tidyr",  type = "Imports")
usethis::use_package("foreach",  type = "Imports")
usethis::use_package("doParallel", type = "Imports")

usethis::use_r("get_gen")
usethis::use_test("get_gen")

usethis::use_r("post_gen")
usethis::use_test("post_gen")

usethis::use_r("get_datasets")
usethis::use_test("get_datasets")

usethis::use_r("get_taxa")
usethis::use_test("get_taxa")

usethis::use_r("get_taxa_group")
usethis::use_test("get_taxa_group")

usethis::use_r("get_observations")
usethis::use_test("get_observations")

usethis::use_r("get_timeseries")
usethis::use_test("get_timeseries")

usethis::use_r("get_taxa_ref")
usethis::use_test("get_taxa_ref")

devtools::document()
devtools::load_all()
devtools::test()
devtools::install()