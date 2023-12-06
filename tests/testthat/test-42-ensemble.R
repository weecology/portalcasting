context(desc = "ensembling functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "ensemble_forecasts ensembles accordingly", {

  skip_on_cran()

  forecast_ids <- select_forecasts(main3)$forecast_id
  forecast_id <- forecast_ids[length(forecast_ids)]

  expect_is(ensemble_forecasts(main     = main3,
                               forecasts_ids = forecast_id), "data.frame")
  expect_error(ensemble_forecasts(main = main3, forecasts_ids = 1e10))

  expect_error(ensemble_forecasts(main = main3, historic_end_newmoonnumber = 1e10))

})