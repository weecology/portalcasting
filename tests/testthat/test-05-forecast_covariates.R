context("Test forecast_covariates functions")

options_all <- all_options(main = "ok")
create_dir(options_all$options_dir)
fill_PortalData(options_all$options_PortalData)
transfer_hist_covariate_forecasts(options_all$options_data)
transfer_trapping_table(options_all$options_data)
moons <- prep_moons(options_all$options_data$moons)
rodents <- prep_rodents(moons, options_all$options_data$rodents)
hist_cov <- prep_hist_covariates(options_all$options_data$covariates)
options_covariates <- options_all$options_data$covariates
up_cov_opts <- update_covfcast_options(options_covariates, hist_cov, moons)

options_covariates2 <- options_covariates
options_covariates2$cast_type <- "hindcasts"
options_covariates2$end <- 499
hist_cov2 <- hist_cov[hist_cov$newmoonnumber <= 499, ]
up_cov_opts2 <- update_covfcast_options(options_covariates2, hist_cov2, moons)
nsteps <- up_cov_opts$lead_time - up_cov_opts$min_lag

test_that("forecast_covariates", {
  expect_error(forecast_covariates(1, moons, up_cov_opts))
  expect_error(forecast_covariates(hist_cov, 1, up_cov_opts))
  expect_error(forecast_covariates(hist_cov, moons, 1))
  fcast_covs <- forecast_covariates(hist_cov, moons, up_cov_opts)
  expect_is(fcast_covs, "data.frame")
  expect_equal(nrow(fcast_covs), nsteps)
  hcast_covs <- forecast_covariates(hist_cov2, moons, up_cov_opts2)
  expect_is(hcast_covs, "data.frame")
  expect_equal(nrow(hcast_covs), nsteps)
})

test_that("forecast_ndvi", {
  expect_error(forecast_ndvi(1, moons, up_cov_opts))
  expect_error(forecast_ndvi(hist_cov, moons, 1))
  expect_error(forecast_ndvi(hist_cov, 1, up_cov_opts))
})

test_that("forecast_weather", {
  expect_error(forecast_weather(moons, 1))
  expect_error(forecast_weather(1, up_cov_opts))
})

test_that("trim_moons_fcast", {
  moons_t <- trim_moons_fcast(moons, options_covariates = up_cov_opts)
  expect_is(moons_t, "moons")
  nfcnms <- length(up_cov_opts$fcast_nms)
  expect_equal(nrow(moons) - nrow(moons_t), nfcnms)
  expect_error(trim_moons_fcast(1, up_cov_opts))
  expect_error(trim_moons_fcast(moons, 1))
})

test_that("get_climate_forecasts", {
  moons_t <- trim_moons_fcast(moons, options_covariates = up_cov_opts)
  expect_error(get_climate_forecasts(1, up_cov_opts))
  expect_error(get_climate_forecasts(moons_t, 1))
  climate_fcast <- get_climate_forecasts(moons_t, up_cov_opts)
  expect_is(climate_fcast, "climate_forecast")
})

unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)