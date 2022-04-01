context(desc = "covariates prepping functions")

main <- "./testing"

test_that(desc = "prep_covariates", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prep_moons(main = main)
  rodents <- prep_rodents(main = main)
  covariates <- prep_covariates(main = main)

})


context(desc = "metadata prepping functions")

main <- "./testing"

test_that(desc = "prep_metadata", {

  # downloads take too long for cran checks

    skip_on_cran() 


  md <- prep_metadata(main = main)
  expect_is(md, "list")

})

