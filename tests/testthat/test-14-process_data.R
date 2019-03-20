context("Test process_data functions")

rod_opts <- all_options(main = "testing_casting")$options_data$rodents
moon_opts <- all_options(main = "testing_casting")$options_data$moons
moons <- prep_moons(moon_opts)
rodents <- prep_rodents(moons, rod_opts)

test_that("foy", {
  expect_silent(foys <- foy(moons$newmoondate))
  expect_is(foys, "numeric")
  expect_equal(nrow(moons), length(foys))
  expect_error(foy("ok"))
})

test_that("interpolate_abundance", {
  expect_error(interpolate_abundance(1))
  expect_silent(rodents_int <- interpolate_abundance(rodents))
  expect_equal(nrow(rodents_int) > nrow(rodents), TRUE)
  expect_equal(ncol(rodents_int) < ncol(rodents), TRUE)
  expect_is(rodents_int, "data.frame")
})

test_that("lag_covariates", {
  tree <- dirtree(main = "testing_casting")
  covariates <- read_data(tree, "covariates")
  expect_error(lag_covariates(covariates, 1, 1))
  expect_error(lag_covariates(covariates, "ok", FALSE))
  expect_error(lag_covariates(covariates, -1, FALSE))
  expect_error(lag_covariates(covariates, 1.5, FALSE))
  expect_error(lag_covariates(covariates, 1:2, FALSE))
  expect_error(lag_covariates(1, 1, FALSE))
  expect_silent(lagged <- lag_covariates(covariates, 6, FALSE))
  expect_silent(lagged2 <- lag_covariates(covariates, 6, TRUE))
  expect_silent(lagged3 <- lag_covariates(covariates, 0, FALSE))
  expect_silent(lagged4 <- lag_covariates(covariates, 0, TRUE))

  expect_equal(nrow(lagged) < nrow(covariates), TRUE)
  expect_equal(nrow(lagged2) == nrow(covariates), TRUE)
  expect_equal(nrow(lagged3) == nrow(covariates), TRUE)
  expect_equal(nrow(lagged4) == nrow(covariates), TRUE)
})

test_that("read_data", {
  tree <- dirtree(main = "testing_casting")
  expect_error(read_data(1, "all"))
  expect_error(read_data(tree, c("all", "controls")))
  expect_error(read_data(tree, "ok"))
  expect_error(read_data(tree, 1))
  expect_silent(all <- read_data(tree, "all"))
  expect_is(all, "rodents")
  expect_silent(controls <- read_data(tree, "controls"))
  expect_is(controls, "rodents")
  expect_silent(covariates <- read_data(tree, "covariates"))
  expect_is(covariates , "covariates")
  expect_silent(covariate_fcasts <- read_data(tree, "covariate_forecasts"))
  expect_is(covariate_fcasts , "covariates")
  expect_silent(moons <- read_data(tree, "moons"))
  expect_is(moons, "moons")
  expect_silent(metadata <- read_data(tree, "metadata"))
  expect_is(metadata, "metadata")
})


test_that("most_recent_census", {
  expect_silent(cdate <- most_recent_census(tree))
  expect_is(cdate, "Date")
  expect_error(most_recent_census(1))
})