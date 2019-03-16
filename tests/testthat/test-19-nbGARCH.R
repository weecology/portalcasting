context("Test nbGARCH functions")

tree <- dirtree(main = "testing_casting")

test_that("nbGARCH", {
  expect_message(f_a <- nbGARCH(tree, level = "All", quiet = FALSE))
  expect_message(f_c <- nbGARCH(tree, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(nbGARCH(1, level = "Controls", quiet = FALSE))
  expect_error(nbGARCH(tree, level = 1, quiet = FALSE))
  expect_error(nbGARCH(tree, level = c("All", "Controls"), quiet = FALSE))
  expect_error(nbGARCH(tree, level = "ok", quiet = FALSE))
  expect_error(nbGARCH(tree, level = "Controls", quiet = 1))
})