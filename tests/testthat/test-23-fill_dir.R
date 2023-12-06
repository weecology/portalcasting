context(desc = "directory filling functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "fill_resources fills the resources folder",
          code = {

  skip_on_cran()

  expect_equal(fill_resources(main = main2), NULL)

})

test_that(desc = "fill_data sets up the data sub",
          code = {

  skip_on_cran()

  expect_equal(fill_data(main = main2), NULL)
  main_dir <- list.files(main2)
  main_dir <- sort(main_dir)
  expected_main_dir <- c("app.R", "data", "directory_configuration.yaml", "fits", "forecasts", "models", "resources", "www")
  expect_equal(main_dir, expected_main_dir)

})



test_that(desc = "fill_fits fills the fits folder",
          code = {


  skip_on_cran()

  xx <- fill_fits(main = main2)
  expect_equal(xx, NULL)

})


test_that(desc = "fill_forecasts fills the forecasts folder",
          code = {


  skip_on_cran()

  xx <- fill_forecasts(main = main2)
  expect_equal(xx, NULL)

})

test_that(desc = "write_models_controls writes out the controls file",
          code = {

  expect_is(write_models_controls(main = main2), "list")

})


test_that(desc = "fill_models adds the models control list and any model scripts to the models folder",
          code = {

  expect_message(fill_models(main = main2))
  model_controls_list <- models_controls(main2)


  files      <- list.files(file.path(main2, "models"))
  model_txts <- unlist(mapply(getElement, mapply(getElement, model_controls_list, "fit"), "model_file"))
  checks     <- c("models_controls.yaml", model_txts)
  names(checks) <- NULL
  expect_equal(sort(files),
               sort(checks))

})