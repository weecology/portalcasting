
context("Test cast_covariates functions")


test_that("cast_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  hist_cov <- prep_hist_covariates(main = "./testing")
print(head(hist_cov))
  expect_is(prep_cast_covariates(main = "./testing", hist_cov = hist_cov),
            "data.frame")
 # expect_is(prep_cast_covariates(main = "./testing", hist_cov = hist_cov),
 #           "data.frame")
 # expect_is(prep_cast_covariates(main = "./testing", hist_cov = hist_cov,
 #                                end_moon = 450), "data.frame")
  expect_error(prep_cast_covariates(main = "./testing", hist_cov = hist_cov,
                                    end_moon = 400))
})


test_that("download_climate_forecasts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(download_climate_casts(main = "./testing"), "character")
})


test_that("read_climate_forecasts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(read_climate_casts(main = "./testing"), "data.frame")
})

