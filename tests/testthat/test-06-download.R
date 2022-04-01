context(desc = "content downloading functions")

# given the directory was created in test-03

main <- "./testing"


test_that(desc = "download_climate_forecasts retrieves files", {

  # downloads take too long for cran checks

    skip_on_cran() 

  dlcc <- download_climate_forecasts(main = main)
  expect_equal((dlcc), NULL)

})

test_that(desc = "download_archive downloads the archive",
          code = {

  expect_equal(download_archive(main), NULL)

})


