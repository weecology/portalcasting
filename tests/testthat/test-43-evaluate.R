context(desc = "evaluate functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "evaluate_forecasts evaluates forecasts", {

  forecasts_ids <- select_forecasts(main     = main3, 
                                   species  = c("DM", "PP", "total"),
                                   models   = c("AutoArima", "ESSS", "pevGARCH", "nbGARCH", "jags_RW"),
                                   datasets = c("all", "controls"))$forecast_id
  nids <- length(forecasts_ids)
  nsample_ids <- 1000
  forecasts_ids <- forecasts_ids[round(seq(1, nids, length.out = nsample_ids))]
  expect_message(ec <- evaluate_forecasts(main = main3, forecasts_ids = forecasts_ids))
  expect_is(ec, "data.frame")
  expect_message(ec2 <- evaluate_forecasts(main = main3, forecasts_ids = forecasts_ids))
  expect_is(ec2, "data.frame")
  expect_error(evaluate_forecasts(main = main3, forecasts_ids = c(1e100, 2e100)))

})


test_that(desc = "evaluate_forecast evaluates forecast", {

  forecasts_ids <- select_forecasts(main = main3)$forecast_id
  forecast_id <- forecasts_ids[length(forecasts_ids)]

  expect_is(evaluate_forecast(main = main3, forecast_id = forecast_id), "data.frame")
  expect_error(evaluate_forecast(main = main3, forecast_id = 1e100))

})
