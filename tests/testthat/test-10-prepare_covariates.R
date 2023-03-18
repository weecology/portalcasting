context(desc = "Test prepare_covariates functions")

main <- "./testing"






test_that(desc = "prepare_covariates", {

  # downloads take too long for cran checks

    skip_on_cran() 

  covs <- prepare_covariates(main = main)
  expect_is(covs, "data.frame")

})
