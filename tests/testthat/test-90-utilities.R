context("Test utility functions")

test_that("data_out", {
  expect_message(prep_moons(main = "./testing"))
  expect_message(prep_moons(main = "./testing", overwrite = FALSE))
})

test_that("isnull", {
  expect_equal(ifnull(NULL, 123), 123)
  expect_equal(ifnull(TRUE, 123), TRUE)
})

test_that("messageq", {
  expect_silent(messageq("ok", TRUE))
  expect_message(messageq("ok"))
})

test_that("list_depth", {
  expect_equal(list_depth("a"), 0)
  expect_equal(list_depth(list("a")), 1)
  expect_equal(list_depth(list()), 0)
  expect_equal(list_depth(list(list("a"))), 2)
})

test_that("return_if_null", {
  ff <- function(x = 1, null_return = "hello"){
    return_if_null(x, null_return)
    x
  }
  expect_equal(ff(), 1)
  expect_equal(ff(NULL), "hello")
})
unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)