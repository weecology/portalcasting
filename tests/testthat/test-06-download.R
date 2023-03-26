context(desc = "content downloading functions")

# given the directory was created in test-03

main <- "./testing"


test_that(desc = "download_climate_forecasts retrieves files", {

  # downloads take too long for cran checks

    skip_on_cran() 

  dlcc <- download_climate_forecasts(main = main)
  expect_equal((dlcc), NULL)
  expect_message(download_climate_forecasts(main))

  expect_error(download_climate_forecasts(main = main, source = "xxx"))


})

test_that(desc = "NMME urls can error as needed",
          code = {
  expect_error(NMME_urls(model = "xxx"))
  expect_error(NMME_urls(data = "xxx"))
  expect_error(NMME_urls(freq = "xxx"))

})

test_that(desc = "download_archive downloads the archive",
          code = {

  expect_equal(download_archive(main, timeout = 600), NULL)
  expect_message(download_archive(main))

  expect_error(download_archive(source = "xxx"))

})


