context("Test pevGARCH functions")

tree <- dirtree(main = "testing_casting")
all <- read_data(tree, "all")
rest_cols <- which(colnames(all) %in% c("newmoonnumber", "BA"))
all <- all[, rest_cols]
controls <-read_data(tree, "controls")
controls <- controls[ , rest_cols]
metadata <- read_data(tree, "metadata")
covariates <- read_data(tree, "covariates")

test_that("pevGARCH", {
  expect_message(f_a <- pevGARCH(all, covariates, metadata, lag = 6, 
                 quiet = FALSE))
  expect_is(f_a, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_error(pevGARCH(1, covariates, metadata, level = "Controls", lag = 6,
                        quiet = FALSE))
  expect_error(pevGARCH(controls, 1, metadata, level = "Controls", lag = 6,
                        quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, 1, level = "Controls", lag = 6,
                        quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, metadata, level = 1, lag = 6, 
                        quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, metadata, 
                        level = c("All", "Controls"), lag = 6, quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, metadata, level = "ok", lag = 6,
                        quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, metadata, level = "Controls", 
                        lag = 6, quiet = 1))
  expect_error(pevGARCH(controls, covariates, metadata, level = "Controls", 
                        lag = -1, quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, metadata, level = "Controls", 
                        lag = 6:7, quiet = FALSE))
  expect_error(pevGARCH(controls, covariates, metadata, level = "Controls", 
                        lag = "a", quiet = FALSE))
})