context("Test prepare_predictions functions")

test_location <- "travis"

test_that("download_predictions", {

  expect_error(download_predictions(1), "`tree` is not")
  expect_message(download_predictions(dirtree(main = "testing_casting"), FALSE))
  if(test_location == "local"){
    expect_message(download_predictions(dirtree(main = "testing_casting")))
  }
})