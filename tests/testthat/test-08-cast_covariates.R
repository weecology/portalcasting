
context(desc = "Functions for casting covariates")

main <- "./testing"




test_that(desc = "cast_covariates properly forecasts", {

  # downloads take too long for cran checks

    skip_on_cran() 


  hist_cov <- prep_historic_covariates(main = main)
  expect_is(hist_cov, "data.frame")
  expect_true(all(hist_cov$source == "historic"))

  cast_cov1 <- prep_forecast_covariates(main = main)
  expect_is(cast_cov1, "data.frame")
  expect_true(all(cast_cov1$source == "forecast"))


  expect_error(prep_forecast_covariates(main = main, origin = 1))

})





test_that(desc = "read_climate_forecasts reads them in right", {

  # downloads take too long for cran checks

    skip_on_cran() 
  
  cc <- read_climate_forecasts(main = main)
  expect_is(cc, "data.frame")
  expect_equal(colnames(cc), c("date", "mintemp", "meantemp", "maxtemp",
                               "precipitation"))

})
