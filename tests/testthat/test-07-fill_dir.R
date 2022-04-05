context(desc = "directory filling functions")

# given the directory was created in test-03

main <- "./testing"

test_that(desc = "fill_raw fills raw but doesn't when not missing and told not to",
          code = {

  # download is held back on cran

    skip_on_cran() 

  expect_equal(fill_raw(main = main), NULL)

})


test_that(desc = "fill_casts fills the casts folder",
          code = {

  # download is held back on cran

    skip_on_cran() 

  expect_message(xx <- fill_casts(main = main, verbose = TRUE))
  expect_equal(xx, NULL)

})




test_that(desc = "fill_data sets up the data sub",
          code = {

  # download is held back on cran

    skip_on_cran() 

  expect_equal(fill_data(main = main), NULL)
  main_dir <- list.files(main)
  main_dir <- sort(main_dir)
  expected_main_dir <- c("casts", "data", "dir_config.yaml", "fits", "models",
                         "raw", "tmp")
  expect_equal(main_dir, expected_main_dir)

})




test_that(desc = "write_model_controls writes out the controls file",
          code = {
  expect_is(write_model_controls(main     = main), "list")

}) 


test_that(desc = "fill_models adds the models to their folder",
          code = {

  expect_message(fill_models(main = main))
  prefabs <- paste0(prefab_models(), ".R")
  mods <- list.files(file.path(main, "models"))

  expect_equal(sort(mods), sort(c(prefabs, "model_controls.yaml")))

})

