
context("Test prepare_covariates functions")

test_that("download_climate_forecasts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(download_climate_casts(main=main), "character")
})


test_that("read_climate_forecasts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(read_climate_casts(main = main), "data.frame")
})

