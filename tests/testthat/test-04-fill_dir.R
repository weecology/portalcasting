context("Test filling functions")

main <- "./testing"

test_that("fill_raw doesn't download when not missing and told not to", {
  skip_on_cran() # download is held back on cran
  expect_equal(fill_raw(main = main, only_if_missing = TRUE), NULL)
})

test_that("fill_data sets up the data sub", {
  skip_on_cran() # download is held back on cran
  expect_equal(fill_data(main = main), NULL)
  main_dir <- list.files(main_path(main))
  main_dir <- sort(main_dir)
  expected_main_dir <- c("casts", "data", "dir_config.yaml", "models",
                         "raw", "tmp")
  expect_equal(main_dir, expected_main_dir)
})

test_that("fill_casts fills the casts folder", {
  skip_on_cran() # download is held back on cran
  expect_equal(fill_casts(main = main), NULL)
  expect_message(fill_casts(main = main, verbose = TRUE))
})