context(desc = "resource downloading functions")

test_that(desc = "download_climate_forecasts retrieves files", {

  skip_on_cran()

  dlcc <- download_climate_forecasts(main = main2)
  expect_equal((dlcc), NULL)
  expect_message(download_climate_forecasts(main2))

  expect_error(download_climate_forecasts(main = main2, source = "xxx"))


})

test_that(desc = "NMME urls can error as needed",
          code = {

  expect_error(NMME_urls(model = "xxx"))
  expect_error(NMME_urls(data = "xxx"))
  expect_error(NMME_urls(freq = "xxx"))

})