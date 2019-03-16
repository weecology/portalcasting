context("Test prepare_covariates functions")

options_all <- all_options(main = "testing_casting")

test_that("prep_covariates", {
  moons <- prep_moons(options_all$options_data$moons)
  cov_opts <- options_all$options_data$covariates
  expect_error(prep_covariates(1))
  expect_error(prep_covariates(moons, 1))
  expect_message(cov_tab <- prep_covariates(moons, cov_opts))
  expect_is(cov_tab, "covariates")
})

test_that("transfer_hist_covariate_forecasts", {
  d_opts <- options_all$options_data
  expect_silent(transfer_hist_covariate_forecasts(d_opts))
  path_to <- file_paths(d_opts$tree, "data/covariate_forecasts.csv")
  unlink(path_to, force = TRUE)  
  expect_message(transfer_hist_covariate_forecasts(d_opts))
  unlink(path_to, force = TRUE)  
  exists <- read.csv(path_to, stringsAsFactors = FALSE) 
  exists$date_made <- "1970-01-01"
  write.csv(exists, path_to, row.names = FALSE)
  expect_message(transfer_hist_covariate_forecasts(d_opts))
  write.csv(exists, path_to, row.names = FALSE)
  expect_message(transfer_hist_covariate_forecasts(d_opts))
})

test_that("prep_hist_covariates", {
  cov_opts <- options_all$options_data$covariates
  cov_opts2 <- cov_opts
  cov_opts2$end <- 400
  expect_error(prep_hist_covariates(1))
  expect_is(prep_hist_covariates(cov_opts), "covariates")
  expect_is(prep_hist_covariates(cov_opts2), "covariates")
})

test_that("prep_fcast_covariates", {
  cov_opts <- options_all$options_data$covariates
  hcov <- prep_hist_covariates(cov_opts)
  moons <- prep_moons(options_all$options_data$moons)
  expect_error(prep_fcast_covariates(hcov, moons, 1))
  expect_error(prep_fcast_covariates(hcov, 1, cov_opts))
  expect_error(prep_fcast_covariates(1, moons, cov_opts))
  expect_is(prep_fcast_covariates(hcov, moons, cov_opts), "covariates")
})

test_that("prep_weather_data", {
  expect_error(prep_weather_data(1))
  expect_is(prep_weather_data(dirtree(main = "testing_casting")), 
            "data.frame")
})

test_that("update_covfcast_options", {
  cov_opts <- options_all$options_data$covariates
  cov_opts2 <- cov_opts
  cov_opts2$cast_type <- "hindcasts"
  cov_opts2$end <- 400
  hcov <- prep_hist_covariates(cov_opts)
  moons <- prep_moons(options_all$options_data$moons)
  expect_error(update_covfcast_options(1, hcov, moons))
  expect_error(update_covfcast_options(cov_opts, hcov, 1))
  expect_error(update_covfcast_options(cov_opts, 1, moons))
  expect_is(update_covfcast_options(cov_opts, hcov, moons), 
            "covariates_options")
  expect_is(update_covfcast_options(cov_opts2, hcov, moons), 
            "covariates_options")
})