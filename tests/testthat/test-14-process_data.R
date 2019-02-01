context("Test process_data functions")

rod_opts <- all_options(main = "testing_casting")$options_data$rodents
moon_opts <- all_options(main = "testing_casting")$options_data$moons
moons <- prep_moons(moon_opts)
rodents <- rodents_data(moons, rod_opts)

test_that("interpolate_abundance", {
  expect_error(interpolate_abundance(1), "`abundance` is not")
  expect_silent(rodents_int <- interpolate_abundance(rodents))
  expect_equal(nrow(rodents_int) > nrow(rodents), TRUE)
  expect_equal(ncol(rodents_int) < ncol(rodents), TRUE)
  expect_is(rodents_int, "data.frame")
})

test_that("lag_data", {
  tree <- dirtree(main = "testing_casting")
  covariates <- read_data(tree, "covariates")
  expect_error(lag_data(covariates, 1, 1))
  expect_error(lag_data(covariates, "ok", FALSE))
  expect_error(lag_data(covariates, -1, FALSE))
  expect_error(lag_data(covariates, 1.5, FALSE))
  expect_error(lag_data(covariates, 1:2, FALSE))
  expect_error(lag_data(1, 1, FALSE))
  expect_silent(lagged <- lag_data(covariates, 6, FALSE))
  expect_silent(lagged2 <- lag_data(covariates, 6, TRUE))
  expect_silent(lagged3 <- lag_data(covariates, 0, FALSE))
  expect_silent(lagged4 <- lag_data(covariates, 0, TRUE))

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
  expect_is(all , "rodents")
  expect_silent(controls <- read_data(tree, "controls"))
  expect_is(controls , "rodents")
  expect_silent(covariates <- read_data(tree, "covariates"))
  expect_is(covariates , "covariates")
  expect_silent(metadata <- read_data(tree, "metadata"))
  expect_is(metadata , "metadata")
})

