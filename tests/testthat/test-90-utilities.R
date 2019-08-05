context("Test utility functions")

test_that("isnull", {
  expect_equal(ifnull(NULL, 123), 123)
  expect_equal(ifnull(TRUE, 123), TRUE)
})

test_that("messageq", {
  expect_silent(messageq("ok", TRUE))
  expect_message(messageq("ok"))
})

