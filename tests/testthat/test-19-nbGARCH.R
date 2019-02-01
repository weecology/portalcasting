context("Test nbGARCH functions")

tree <- dirtree(main = "testing_casting")
all <- read_data(tree, "all")
rest_cols <- which(colnames(all) %in% c("newmoonnumber", "BA", "DM"))
all <- all[, rest_cols]
controls <-read_data(tree, "controls")
controls <- controls[ , rest_cols]
metadata <- read_data(tree, "metadata")

test_that("nbGARCH", {
  expect_message(f_a <- nbGARCH(all, metadata, quiet = FALSE))
  expect_message(
   f_c <- nbGARCH(controls, metadata, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(nbGARCH(1, metadata, level = "Controls", quiet = FALSE))
  expect_error(nbGARCH(controls, 1, level = "Controls", quiet = FALSE))
  expect_error(nbGARCH(controls, metadata, level = 1, quiet = FALSE))
  expect_error(nbGARCH(controls, metadata, level = c("All", "Controls"), 
               quiet = FALSE))
  expect_error(nbGARCH(controls, metadata, level = "ok", quiet = FALSE))
  expect_error(nbGARCH(controls, metadata, level = "Controls", quiet = 1))
})