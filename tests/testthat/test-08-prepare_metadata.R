context("Test prepare_metadata functions")

test_that("prep_metadata", {
  skip_on_cran() # downloads take too long for cran checks
  moons <- prep_moons(main = "./testing")
  rodents <- prep_rodents(main = "./testing", moons = moons)
  covariates <- prep_covariates(main = "./testing", moons = moons)
  expect_is(prep_metadata(main = "./testing", moons, rodents, covariates),
            "list")

})

