context("directory filling functions")

# given the directory was created in test-03

main <- "./testing"

test_that("fill_raw fills raw but doesn't when not missing and told not to", {

  # download is held back on cran

    skip_on_cran() 

  expect_equal(fill_raw(main = main, only_if_missing = TRUE), NULL)
  expect_equal(fill_raw(main = main, only_if_missing = TRUE), NULL)

})


test_that("fill_data sets up the data sub", {

  # download is held back on cran

    skip_on_cran() 

  expect_equal(fill_data(main = main), NULL)
  main_dir <- list.files(main_path(main))
  main_dir <- sort(main_dir)
  expected_main_dir <- c("casts", "data", "dir_config.yaml", "fits", "models",
                         "raw", "tmp")
  expect_equal(main_dir, expected_main_dir)

})


test_that("fill_casts fills the casts folder", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(xx <- fill_casts(main = main, verbose = TRUE))
  expect_equal(xx, NULL)

})


test_that("fill_models adds the models to their folder", {

  expect_message(fill_models(main = main))
  prefabs <- paste0(prefab_models(), ".R")
  mods <- list.files(models_path(main = main))

  expect_equal(sort(mods), sort(prefabs))

})


