context(desc = "Test prepare_covariates functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "prepare_covariates", {

  skip_on_cran() 

  covs <- prepare_covariates(main = main2)
  expect_is(covs, "data.frame")

})
