# usethis::create_package("~/dev/ratlas")
usethis::use_build_ignore("dev.R")
usethis::use_build_ignore("dev_scratch")
usethis::use_mit_license(copyright_holder = "Vincent Beauregard")

usethis::use_dev_package("usethis", type = "Imports", remote = NULL)
usethis::use_dev_package("devtools", type = "Imports", remote = NULL)


usethis::use_package("Imports", type = "Depends")
usethis::use_package("Imports", type = "Depends")
usethis::use_package("Imports",  type = "Depends")

usethis::use_r("get_occurences")
usethis::use_test("get_occurences")

usethis::use_r("get_taxa")
usethis::use_test("get_taxa")

usethis::use_r("get_timeseries")
usethis::use_test("get_timeseries")

devtools::document()
devtools::install()