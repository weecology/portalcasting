context("Test portalcast functions")

test_that("portalcast", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(portalcast(main = "./testing", models = "AutoArima",
                            cleanup = FALSE))
})

test_that("prep_data", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(prep_data(main = "./testing", end_moon = 515,
                            cleanup = FALSE))
})





