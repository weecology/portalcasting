context(desc = "evaluate functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "evaluate_forecasts evaluates casts", {

  forecast_ids <- select_forecasts(main = main3, species = c("DM", "PP", "total"), models = c("AutoArima", "ESSS", "pevGARCH", "nbGARCH"), datasets = c("all", "controls"))$forecast_id
  nids <- length(forecast_ids)
  nsample_ids <- 1000
  forecast_ids <- forecast_ids[round(seq(1, nids, length.out = nsample_ids))]
  expect_message(ec <- evaluate_forecasts(main = main3, forecast_ids = forecast_ids))
  expect_is(ec, "data.frame")
  expect_error(evaluate_forecasts(main = main3, forecast_ids = c(1e100, 2e100)))

})


test_that(desc = "evaluate_cast evaluates cast", {

  forecast_ids <- select_forecasts(main = main3)$forecast_id
  forecast_id <- forecast_ids[length(forecast_ids)]

  expect_is(evaluate_cast(main = main3, forecast_id = forecast_id), "data.frame")
  expect_error(evaluate_cast(main = main3, forecast_id = 1e100))

})
