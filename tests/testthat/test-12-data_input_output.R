context(desc = "data input and output functions")

main <- "./testing"


test_that(desc = "read_directory_config works but errors out of directory", {

  dc <- read_directory_configuration(main)
  expect_is(dc, "list")
  expect_error(read_directory_config())

})


test_that(desc = "read_climate_forecasts reads them in right", {

  # downloads take too long for cran checks

    skip_on_cran() 
  
  cc <- read_climate_forecasts(main = main)
  expect_is(cc, "data.frame")
  expect_equal(colnames(cc), c("date", "mintemp", "meantemp", "maxtemp", "precipitation"))

})


test_that(desc = "write_data works if overwriting or not", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_silent(controls <- read_data(main, "rodents_table", "controls"))
  expect_message(write_data(controls, main, save = TRUE, 
                            filename = "rodents_controls.csv",
                            overwrite = FALSE))
  expect_message(write_data(controls, main, save = TRUE, 
                            filename = "rodents_controls.csv",
                            overwrite = TRUE))

})  



test_that(desc = "read_data works when the data are present", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_silent(all <- read_data(main, "rodents_table", "all"))
  expect_is(all, "data.frame")
  expect_silent(controls <- read_data(main, "rodents_table", 
                                      "controls"))
  expect_is(controls, "data.frame")
  expect_silent(controls <- read_data(main, "rodents", "controls"))
  expect_is(controls, "list")
  expect_silent(covariates <- read_data(main, "covariates"))
  expect_is(covariates , "data.frame")
  expect_silent(covariate_casts <- read_data(main, "climate_forecasts"))
  expect_is(covariate_casts , "data.frame")
  expect_silent(moons <- read_data(main, "newmoons"))
  expect_is(moons, "data.frame")
  expect_silent(metadata <- read_data(main, "metadata"))
  expect_is(metadata, "list")



})

