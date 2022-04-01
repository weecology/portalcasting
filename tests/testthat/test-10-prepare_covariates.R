context(desc = "Test prepare_covariates functions")

main <- "./testing"


test_that(desc = "prep_covariates", {

  # downloads take too long for cran checks

    skip_on_cran() 

  covs <- prep_covariates(main = main)
  expect_is(covs, "data.frame")

})

test_that(desc = "lag_covariates lags properly, including the tail or not", {

  # downloads take too long for cran checks

    skip_on_cran() 

  covariate_casts <- read_covariate_casts(main = main)
  covar_casts_lag1 <- lag_covariates(covariate_casts, lag = 2, tail = TRUE)
  expect_is(covar_casts_lag1, "data.frame")
  covar_casts_lag2 <- lag_covariates(covariate_casts, lag = 2, tail = FALSE)
  expect_is(covar_casts_lag2, "data.frame")
  expect_equal(NROW(covar_casts_lag1) > NROW(covar_casts_lag2), TRUE)

})



test_that(desc = "daily weather can be summarized by moon", {

  # downloads take too long for cran checks

    skip_on_cran() 

  raw_path <- raw_path(main)
  moons <- prep_moons(main)
  weather <- portalr::weather("daily", fill = TRUE, path = raw_path)
  weather <- add_date_from_components(weather)
  weather <- add_moons_from_date(weather, moons)
  dw <- summarize_daily_weather_by_moon(weather)
  expect_is(dw, "data.frame")
  expect_equal(NROW(dw) < NROW(weather), TRUE)

})
