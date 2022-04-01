
context(desc = "Functions for casting covariates")

main <- "./testing"

test_that(desc = "cast_covariates properly forecasts", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prep_moons(main = main)
  hist_cov <- prep_hist_covariates(main = main, moons = moons)

  cast_cov1 <- prep_cast_covariates(main = main, moons = moons, 
                                    hist_cov = hist_cov)
  expect_is(cast_cov1, "data.frame")
  expect_true(all(cast_cov1$source == "cast"))

  cast_cov2 <- prep_cast_covariates(main = main, moons = moons, 
                                    hist_cov = hist_cov, end_moon = 450)
  expect_is(cast_cov2, "data.frame")
  expect_true(all(cast_cov2$source == "cast"))

  expect_error(prep_cast_covariates(main = main, moons = moons, 
                                    hist_cov = hist_cov, end_moon = 400))

})



test_that(desc = "download_climate_forecasts retrieves files", {

  # downloads take too long for cran checks

    skip_on_cran() 

  dlcc <- download_climate_forecasts(main = main)
  expect_equal((dlcc), NULL)

})


test_that(desc = "read_climate_forecasts reads them in right", {

  # downloads take too long for cran checks

    skip_on_cran() 
  
  cc <- read_climate_casts(main = main)
  expect_is(cc, "data.frame")
  expect_equal(colnames(cc), c("date", "mintemp", "meantemp", "maxtemp",
                               "precipitation"))

})
