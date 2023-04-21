context(desc = "cast processing functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "read_forecast_table reads in a forecast tab", {


  skip_on_cran() 

  expect_is(read_forecast_table(main = main3, forecast_id = NULL), "data.frame")
  expect_error(read_forecast_table(main = main3, forecast_id = 1e10))
  expect_error(read_forecast_table(main = main3, forecast_id = "1.1"))

})

test_that(desc = "read_forecasts_tables reads in multiple forecast tabs", {


  skip_on_cran() 

  expect_is(read_forecasts_tables(main = main3, forecasts_ids = NULL), "data.frame")
  expect_is(read_forecasts_tables(main = main3, forecasts_ids = c("1.01", "1.02")), "data.frame")

})


test_that(desc = "read_forecast_metadata reads in the metadata", {


  skip_on_cran() 

  expect_is(read_forecast_metadata(main = main3, forecast_id = NULL), "list")
  expect_error(read_forecast_metadata(main = main3, forecast_id = 1e10))

})


test_that(desc = "read_model_fit reads in model fits", {


  skip_on_cran() 

  #expect_is(read_model_fit(main = main3, forecast_id = NULL), "list")
  expect_error(read_model_fit(main = main3, forecast_id = 1e10))
  expect_error(read_model_fit(main = main3))

})


test_that(desc = "add_obs_to_forecast_table functions add properly", {


  skip_on_cran() 

   forecast_table <- read_forecast_table(main = main3, forecast_id = "1.01")
   expect_is(add_observations_to_forecast_table(main = main3, forecast_table = forecast_table), 
             "data.frame")


})


test_that(desc = "read_model_forecast reads in model forecasts", {


  skip_on_cran() 

  ok <- read_model_forecast(main = main3, forecast_id = NULL)
  expect_error(read_model_forecast(main = main3, forecast_id = 1e10))

})