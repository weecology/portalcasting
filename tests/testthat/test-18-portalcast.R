context("portalcast functions")

main <- "./testing"

test_that("portalcast works as basic", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(portalcast(main = main, models = "AutoArima"))

})

