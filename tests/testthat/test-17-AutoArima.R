context("Test AutoArima functions")

tree <- dirtree(main = "testing_casting");
all <- read_data(tree, "all");
controls <-read_data(tree, "controls");
metadata <- read_data(tree, "metadata");

test_that("AutoArima", {
  expect_message(f_a <- AutoArima(all, metadata, quiet = FALSE))
  expect_message(
   f_c <- AutoArima(controls, metadata, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(AutoArima(1, metadata, level = "Controls", quiet = FALSE))
  expect_error(AutoArima(controls, 1, level = "Controls", quiet = FALSE))
  expect_error(AutoArima(controls, metadata, level = 1, quiet = FALSE))
  expect_error(AutoArima(controls, metadata, level = c("All", "Controls"), 
               quiet = FALSE))
  expect_error(AutoArima(controls, metadata, level = "ok", quiet = FALSE))
  expect_error(AutoArima(controls, metadata, level = "Controls", quiet = 1))
})

