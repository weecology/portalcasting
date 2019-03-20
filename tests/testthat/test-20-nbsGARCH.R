context("Test nbsGARCH functions")

tree <- dirtree(main = "testing_casting")

test_that("nbsGARCH", {
  expect_message(f_a <- nbsGARCH(tree, level = "All", quiet = FALSE))
  expect_message(f_c <- nbsGARCH(tree, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(nbsGARCH(1, level = "Controls", quiet = FALSE))
  expect_error(nbsGARCH(tree, level = 1, quiet = FALSE))
  expect_error(nbsGARCH(tree, level = c("All", "Controls"), quiet = FALSE))
  expect_error(nbsGARCH(tree, level = "ok", quiet = FALSE))
  expect_error(nbsGARCH(tree, level = "Controls", quiet = 1))
})