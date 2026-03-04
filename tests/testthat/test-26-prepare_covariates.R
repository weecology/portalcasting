context(desc = "Test prepare_covariates functions")

test_that(desc = "prepare_covariates", {

  skip_on_cran()

  covs <- prepare_covariates(main = main3)
  expect_is(covs, "data.frame")

})
