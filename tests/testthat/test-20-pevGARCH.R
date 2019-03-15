context("Test pevGARCH functions")

tree <- dirtree(main = "testing_casting")

test_that("pevGARCH", {
  expect_message(f_a <- pevGARCH(tree, level = "All", lag = 6, quiet = FALSE))
  expect_message(f_c <- pevGARCH(tree, level = "Controls", lag = 6, 
                                 quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(pevGARCH(1, level = "Controls", lag = 6,quiet = FALSE))
  expect_error(pevGARCH(tree, level = 1, lag = 6, quiet = FALSE))
  expect_error(pevGARCH(tree, level = c("All", "Controls"), lag = 6, 
                        quiet = FALSE))
  expect_error(pevGARCH(tree, level = "ok", lag = 6, quiet = FALSE))
  expect_error(pevGARCH(tree, level = "Controls", lag = "ok", quiet = FALSE))
  expect_error(pevGARCH(tree, level = "Controls", lag = -1, quiet = FALSE))
  expect_error(pevGARCH(tree, level = "Controls", lag = 6:10, quiet = FALSE))
  expect_error(pevGARCH(tree, level = "Controls", lag = 6, quiet = 1))
})