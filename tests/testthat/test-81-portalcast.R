context("Test portalcast functions")

# this script should be put in a specific location, likely last or near
test_that("portalcast", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(portalcast(main = "./testing", models = "AutoArima"))
})



test_that("prep_data", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(prep_data(main = "./testing", end_moon = 520))
  expect_message(prep_data(main = "./testing", end_moon = 520))
})