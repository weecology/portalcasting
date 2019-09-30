context("Test messaging functions")

test_that("model_done_message", {
  expect_is(model_done_message("xx", NA), "NULL")
})

