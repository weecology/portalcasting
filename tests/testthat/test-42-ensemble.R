context(desc = "ensembling functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "ensemble_casts ensembles accordingly", {

  skip_on_cran() 

  forecast_ids <- select_forecasts(main3)$forecast_id
  forecast_id <- forecast_ids[length(forecast_ids)]

  expect_is(ensemble_casts(main     = main3, 
                           forecast_ids = forecast_id), "data.frame")
  expect_error(ensemble_casts(main = main3, forecast_id = 1e10))

  expect_error(ensemble_casts(main = main3, historic_end_newmoonnumber = 1e10))

})