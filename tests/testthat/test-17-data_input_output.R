context("Test data_input_output functions")


test_that("read_data", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(all <- read_data("./testing", "rodents_table", "all"))
  expect_is(all, "data.frame")
  expect_silent(controls <- read_data("./testing", "rodents_table", 
                                      "controls"))
  expect_is(controls, "data.frame")
  expect_silent(controls <- read_data("./testing", "rodents", "controls"))
  expect_is(controls, "list")
  expect_silent(covariates <- read_data("./testing", "covariates"))
  expect_is(covariates , "data.frame")
  expect_silent(covariate_casts <- read_data("./testing", "covariate_casts"))
  expect_is(covariate_casts , "data.frame")
  expect_silent(moons <- read_data("./testing", "moons"))
  expect_is(moons, "data.frame")
  expect_silent(metadata <- read_data("./testing", "metadata"))
  expect_is(metadata, "list")

# wipe the files, then read again to do the prep backup

unlink(list.files(data_path(main = "./testing"), full.names = TRUE))

  expect_silent(moons <- read_moons("./testing"))
  expect_is(moons, "data.frame")

  expect_silent(all <- read_data("./testing", "rodents_table", "all"))
  expect_is(all, "data.frame")
  expect_silent(controls <- read_data("./testing", "rodents_table", 
                                      "controls"))
  expect_is(controls, "data.frame")
  expect_silent(controls <- read_data("./testing", "rodents", "controls"))
  expect_is(controls, "list")
  expect_silent(covariates <- read_data("./testing", "covariates"))
  expect_is(covariates , "data.frame")
  expect_silent(covariate_casts <- read_data("./testing", 
                                               "covariate_casts"))
  expect_is(covariate_casts , "data.frame")

  expect_silent(metadata <- read_data("./testing", "metadata"))
  expect_is(metadata, "list")


})


