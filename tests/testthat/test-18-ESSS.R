context("Test ESSS functions")

tree <- dirtree(main = "testing_casting");
all <- read_data(tree, "all");
controls <-read_data(tree, "controls");
metadata <- read_data(tree, "metadata");

test_that("ESSS", {
  expect_message(f_a <- ESSS(all, metadata, quiet = FALSE))
  expect_message(
   f_c <- ESSS(controls, metadata, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(ESSS(1, metadata, level = "Controls", quiet = FALSE))
  expect_error(ESSS(controls, 1, level = "Controls", quiet = FALSE))
  expect_error(ESSS(1, metadata, level = 1, quiet = FALSE))
  expect_error(ESSS(1, metadata, level = c("All", "Controls"), 
               quiet = FALSE))
  expect_error(ESSS(1, metadata, level = "ok", quiet = FALSE))
  expect_error(ESSS(1, metadata, level = "Controls", quiet = 1))
})