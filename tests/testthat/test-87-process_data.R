context("Test process_data functions")

test_that("lag_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  covs <- read_covariates(main = "./testing")
  expect_is(lag_covariates(covs, 2), "data.frame")
})

test_that("interpolate_abundance", {
  skip_on_cran() # downloads take too long for cran checks
  rods <- read_rodents(main = "./testing", "all")
  expect_silent(rods2 <- interpolate_abundance(rods))
  expect_is(rods2, "data.frame")
})

test_that("read_data", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(all <- read_data("./testing", "rodents", "all"))
  expect_is(all, "data.frame")
  expect_silent(controls <- read_data("./testing", "rodents", "controls"))
  expect_is(controls, "data.frame")
  expect_silent(covariates <- read_data("./testing", "covariates"))
  expect_is(covariates , "data.frame")
  expect_silent(covariate_casts <- read_data("./testing", "covariate_casts"))
  expect_is(covariate_casts , "data.frame")
  expect_silent(moons <- read_data("./testing", "moons"))
  expect_is(moons, "data.frame")
  expect_silent(metadata <- read_data("./testing", "metadata"))
  expect_is(metadata, "list")
})