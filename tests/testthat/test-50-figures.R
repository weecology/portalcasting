context(desc = "Figure functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "plot_forecast_ts", {


  skip_on_cran() 

  expect_silent(plot_forecast_ts(main = main3, species = "DM", 
                             model = "AutoArima"))
  expect_silent(plot_forecast_ts(main = main3, species = "DM"))
  expect_silent(plot_forecast_ts(main = main3, species = "total"))
  expect_error(plot_forecast_ts(main = main3, species = "DM", forecast_id = 1e10))

})

test_that(desc = "plot_forecast_point", {


  skip_on_cran() 

  expect_silent(plot_forecast_point(main = main3, dataset = "controls", model = "AutoArima", species = "DM"))
  expect_silent(plot_forecast_point(main = main3, dataset = "controls", model = "AutoArima", species = c("BA", "DM"), highlight_sp = "DM"))
  expect_silent(plot_forecast_point(main = main3, model = "AutoArima"))
  expect_silent(plot_forecast_point(main = main3, model = "AutoArima", with_census = TRUE))
  
  expect_error(plot_forecast_point(main = main3, forecast_id = 1e10))


})


test_that(desc = "plot_forecasts_error_lead", {


  skip_on_cran() 

  expect_silent(plot_forecasts_error_lead(main = main3, species = c("PP"), models = c("AutoArima")))
  expect_silent(plot_forecasts_error_lead(main = main3, species = c("total", "DM"), models = c("AutoArima", "ESSS")))

  expect_error(plot_forecasts_error_lead(main = main3, forecast_id = 1e10))

})



test_that(desc = "plot_forecasts_cov_RMSE", {


  skip_on_cran() 

  expect_silent(plot_forecasts_cov_RMSE(main = main3, species = c("DM"), models = c("AutoArima")))
  expect_silent(plot_forecasts_cov_RMSE(main = main3, species = c("total", "DM"), models = c("AutoArima", "ESSS")))

  expect_error(plot_forecasts_cov_RMSE(main = main3, forecast_id = 1e10))

})



test_that(desc = "plot_covariates", {


  skip_on_cran() 

  expect_silent(plot_covariates(main = main3, to_plot = c("ndvi", "warm_precip")))
  expect_silent(plot_covariates(main = main3, to_plot = c("precipitation", "warm_precip")))
  expect_silent(plot_covariates(main = main3, to_plot = c("mintemp", "meantemp", "maxtemp")))

  expect_error(plot_covariates(main = main3, to_plot = "xyz"))

})

