context("Test ESSS functions")

tree <- dirtree(main = "testing_casting");

test_that("ESSS", {
  expect_message(f_a <- ESSS(tree, level = "All", quiet = FALSE))
  expect_message(f_c <- ESSS(tree, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(ESSS(1, level = "Controls", quiet = FALSE))
  expect_error(ESSS(tree, level = 1, quiet = FALSE))
  expect_error(ESSS(tree, level = c("All", "Controls"), quiet = FALSE))
  expect_error(ESSS(tree, level = "ok", quiet = FALSE))
  expect_error(ESSS(tree, level = "Controls", quiet = 1))
})