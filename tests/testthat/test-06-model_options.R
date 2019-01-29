context("Test model_options functions")

test_that("model_options", {
  expect_error(model_options(tree = 1, name = "AutoArima", 
                             covariates = FALSE, lag = NULL, quiet = FALSE),
               "`tree`")
  expect_error(model_options(tree = dirtree(), name = 1, 
                             covariates = FALSE, lag = NULL, quiet = FALSE),
               "`name` is not")
  expect_error(model_options(tree = dirtree(), name = c("AutoArima", "ESSS"), 
                             covariates = FALSE, lag = NULL, quiet = FALSE),
               "`name` can only be")
  expect_error(model_options(tree = dirtree(), name = "AutoArima", 
                             covariates = 1, lag = NULL, quiet = FALSE),
               "`covariates`")
  expect_error(model_options(tree = dirtree(), name = "AutoArima", 
                             covariates = FALSE, lag = -1, quiet = FALSE),
               "`lag` is not")
  expect_error(model_options(tree = dirtree(), name = "AutoArima", 
                             covariates = FALSE, lag = 2.2, quiet = FALSE),
               "`lag` is not")
  expect_error(model_options(tree = dirtree(), name = "AutoArima", 
                             covariates = FALSE, lag = "a", quiet = FALSE),
               "`lag` is not")
  expect_error(model_options(tree = dirtree(), name = "AutoArima", 
                             covariates = FALSE, lag = 1:2, quiet = FALSE),
               "`lag` can only be")
  expect_error(model_options(tree = dirtree(), name = "AutoArima", 
                             covariates = FALSE, lag = NULL, quiet = 1),
               "`quiet` is not")

  opts <- model_options(tree = dirtree(), name = "AutoArima", 
                        covariates = FALSE, lag = NULL, quiet = FALSE)
  expect_is(opts, "model_options")
})

test_that("AutoArima_options", {
  expect_is(AutoArima_options(), "model_options")
})

test_that("ESSS_options", {
  expect_is(ESSS_options(), "model_options")
})

test_that("nbGARCH_options", {
  expect_is(nbGARCH_options(), "model_options")
})

test_that("pevGARCH_options", {
  expect_is(pevGARCH_options(), "model_options")
})