context("Test prepare_covariates functions")

main <- "./testing"

test_that("prep_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(prep_covariates(main = main), "data.frame")
})

test_that("process_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  fill_data(main = main)
  covariate_casts <- read_covariate_casts(main = main)
  covar_casts_lag <- lag_covariates(covariate_casts, lag = 2, tail = TRUE)
  expect_is(covar_casts_lag, "data.frame")
  covar_casts_lag <- lag_covariates(covariate_casts, lag = 2, tail = FALSE)
  expect_is(covar_casts_lag, "data.frame")
})



test_that("prep_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  raw_path <- sub_path(main = "./testing", subs = "raw")
  moons <- prep_moons(main = "./testing")
  weather("daily", TRUE, raw_path) %>% 
  add_date_from_components() %>%
  select(-c(year, month, day, battery_low, locally_measured))  %>%
  add_moons_from_date(moons) %>%
  summarize_daily_weather_by_moon() -> x
  expect_is(x, "data.frame")
})
