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


zip_name <- c("forecast_id_2019-11-15.zip")
eval_zip <- c("forecasts_evaluations.zip")
resources_forecast <- file.path(main3, "resources/portal-forecasts/forecasts")
expected <- c(
  "forecast_id_1.01_forecast_table.csv",
  "forecast_id_1.01_metadata.yaml",
  "forecast_id_1.01_model_forecast.json",
  "forecasts_evaluations.csv",
  "forecasts_metadata.csv"
)
monk_forecast <- file.path(tempdir(), "forecasts/")
#Create the monk_forecast directory if it doesn't exist
if (!file.exists(monk_forecast)) {
  dir.create(monk_forecast, recursive = TRUE)
}
#Copy files from resources_forecast to monk_forecast
file.copy(file.path(resources_forecast, expected), monk_forecast, overwrite = TRUE)
#Check if files are successfully copied
copied_files <- list.files(monk_forecast)
if (all(copied_files %in% expected)) {
  print("Files copied successfully.")
} else {
  warning("Failed to copy files.")
}


test_that("Zip All Forecast Files", {
  skip_on_cran()
  zip_unzip(type = "zip", forecast_path = monk_forecast)
  expect_true(file.exists(file.path(monk_forecast, zip_name)),
              info = paste("File", zip_name, "does not exist in the directory."))
  expect_true(file.exists(file.path(monk_forecast, eval_zip)),
              info = paste("File", zip_name, "does not exist in the directory."))
})


test_that("Unzip All Forecast Files", {
  skip_on_cran()
  zip_unzip(type = "unzip", forecast_path = monk_forecast)
  file_existence <- sapply(expected, function(file_name) {
    full_path <- file.path(resources_forecast, file_name)
    file.exists(full_path)
  })
  expect_true(all(file_existence), info = "Not all expected files exist in the directory.")
})
