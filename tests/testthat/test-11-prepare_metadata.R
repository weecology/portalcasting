context("covariates prepping functions")

main <- "./testing"

test_that("prep_covariates", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prep_moons(main = main)
  rodents <- prep_rodents(main = main, moons = moons)
  covariates <- prep_covariates(main = main, moons = moons)
  md <- prep_metadata(main = main, moons = moons, rodents = rodents, 
                          covariates = covariates)
  expect_is(md, "list")

})