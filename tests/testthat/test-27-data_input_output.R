context(desc = "data input and output functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))




test_that(desc = "read_directory_config works but errors out of directory", {

  skip_on_cran() 

  dc <- read_directory_configuration(main = main2)
  expect_is(dc, "list")
  expect_error(read_directory_config())

})


test_that(desc = "read_climate_forecasts reads them in right", {

  skip_on_cran() 
  
  cc <- read_climate_forecasts(main = main2)
  expect_is(cc, "data.frame")
  expect_equal(colnames(cc), c("date", "mintemp", "meantemp", "maxtemp", "precipitation"))

})


test_that(desc = "write_data works if overwriting or not", {

  skip_on_cran() 

  expect_silent(controls <- read_data(main = main2, "rodents_table", "controls"))
  expect_message(write_data(controls, main = main2, save = TRUE, 
                            filename = "rodents_controls.csv",
                            overwrite = FALSE))
  expect_message(write_data(controls, main = main2, save = TRUE, 
                            filename = "rodents_controls.csv",
                            overwrite = TRUE))

})  



test_that(desc = "read_data works when the data are present", {

  skip_on_cran() 

  expect_silent(all <- read_data(main = main2, "rodents_table", "all"))
  expect_is(all, "data.frame")
  expect_silent(controls <- read_data(main = main2, "rodents_table", 
                                      "controls"))
  expect_is(controls, "data.frame")
  expect_silent(controls <- read_data(main = main2, "rodents", "controls"))
  expect_is(controls, "list")
  expect_silent(covariates <- read_data(main = main2, "covariates"))
  expect_is(covariates , "data.frame")
  expect_silent(covariate_casts <- read_data(main = main2, "climate_forecasts"))
  expect_is(covariate_casts , "data.frame")
  expect_silent(moons <- read_data(main = main2, "newmoons"))
  expect_is(moons, "data.frame")
  expect_silent(metadata <- read_data(main = main2, "metadata"))
  expect_is(metadata, "list")

})

