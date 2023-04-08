context(desc = "resource downloading functions")


main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "download_archive skips or errors", {

  expect_message(download_archive(main = main3))
  expect_error(download_archive(source = "errrrrr"))
  expect_error(download_archive(version = "000110000"))

})

test_that(desc = "download_climate_forecasts retrieves files", {

  skip_on_cran() 

  dlcc <- download_climate_forecasts(main = main1)
  expect_equal((dlcc), NULL)
  expect_message(download_climate_forecasts(main1))

  expect_error(download_climate_forecasts(main = main1, source = "xxx"))


})

test_that(desc = "NMME urls can error as needed",
          code = {

  expect_error(NMME_urls(model = "xxx"))
  expect_error(NMME_urls(data = "xxx"))
  expect_error(NMME_urls(freq = "xxx"))

})