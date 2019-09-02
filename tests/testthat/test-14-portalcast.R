context("Test portalcast functions")

main <- "./testing"

test_that("portalcast", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(portalcast(main = main, models = "AutoArima"))
})


test_that("prep_data", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(prep_data(main = main, end_moon = 515))
})





