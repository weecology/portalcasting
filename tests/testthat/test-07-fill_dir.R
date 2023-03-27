context(desc = "directory filling functions")

# given the directory was created in test-03

main <- "./testing"

test_that(desc = "fill_resources fills the resources folder",
          code = {

  # download is held back on cran

    skip_on_cran() 

  expect_equal(fill_resources(main = main), NULL)

})


test_that(desc = "fill_casts fills the casts folder",
          code = {

  # download is held back on cran

    skip_on_cran() 

  expect_message(xx <- fill_forecasts(main = main))
  expect_equal(xx, NULL)

})


test_that(desc = "fill_fits fills the fits folder",
          code = {

  # download is held back on cran

    skip_on_cran() 

  xx <- fill_fits(main = main)
  expect_equal(xx, NULL)

})




test_that(desc = "fill_data sets up the data sub",
          code = {

  # download is held back on cran

    skip_on_cran() 

  expect_equal(fill_data(main = main), NULL)
  main_dir <- list.files(main)
  main_dir <- sort(main_dir)
  expected_main_dir <- c("data", "directory_configuration.yaml", "fits", "forecasts", "models", "resources")
  expect_equal(main_dir, expected_main_dir)

})




test_that(desc = "write_model_controls writes out the controls file",
          code = {
  expect_is(write_model_controls(main = main), "list")

}) 


test_that(desc = "fill_models adds the models control list and any model scripts to the models folder",
          code = {

  expect_message(fill_models(main = main))
  model_controls_list <- model_controls(main)


  files      <- list.files(file.path(main, "models"))
  model_txts <- unlist(mapply(getElement, mapply(getElement, model_controls_list, "fit"), "model_file"))
  checks     <- c("model_controls.yaml", model_txts)
  names(checks) <- NULL
  expect_equal(sort(files), 
               sort(checks))

})

