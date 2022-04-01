context(desc = "data input and output functions")

main <- "./testing"

test_that(desc = "read_directory_config works but errors out of directory", {

  dc <- read_directory_config(main)
  expect_is(dc, "list")
  expect_error(read_directory_config())

})

test_that(desc = "update_directory_config runs through", {

  dc <- update_directory_config(main)
  expect_equal(dc, NULL)

})


test_that(desc = "write_data works if overwriting or not", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_silent(controls <- read_data(main, "rodents", "controls"))
  expect_message(write_data(controls, main, save = TRUE, 
                            filename = "rodents_controls.csv",
                            overwrite = FALSE))
  expect_message(write_data(controls, main, save = TRUE, 
                            filename = "rodents_controls.csv",
                            overwrite = TRUE))

})  



test_that(desc = "read_data works when the data are present or not", {

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
  expect_silent(covariate_casts <- read_data(main, "covariate_casts"))
  expect_is(covariate_casts , "data.frame")
  expect_silent(moons <- read_data(main, "moons"))
  expect_is(moons, "data.frame")
  expect_silent(metadata <- read_data(main, "metadata"))
  expect_is(metadata, "list")


  # wipe the files, then read again to do the prep backup

    unlink(list.files(data_path(main = main), full.names = TRUE))

  expect_silent(moons <- read_moons(main))
  expect_is(moons, "data.frame")
  expect_silent(all <- read_data(main, "rodents_table", "all"))
  expect_is(all, "data.frame")
  expect_silent(controls <- read_data(main, "rodents_table", "controls"))
  expect_is(controls, "data.frame")
  expect_silent(controls <- read_data(main, "rodents", "controls"))
  expect_is(controls, "list")
  expect_silent(covariates <- read_data(main, "covariates"))
  expect_is(covariates , "data.frame")
  expect_silent(covariate_casts <- read_data(main, "covariate_casts"))
  expect_is(covariate_casts , "data.frame")
  expect_silent(metadata <- read_data(main, "metadata"))
  expect_is(metadata, "list")


})

