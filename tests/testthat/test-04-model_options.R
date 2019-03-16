context("Test model_options functions")

test_that("model_options", {
  expect_error(model_options(tree = 1, model = "AutoArima", 
                             mod_covariates = FALSE, lag = NULL, 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = 1, 
                             mod_covariates = FALSE, lag = NULL, 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = c("AutoArima", "ESSS"), 
                             mod_covariates = FALSE, lag = NULL, 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = "AutoArima", 
                             mod_covariates = 1, lag = NULL, 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = "AutoArima", 
                             mod_covariates = FALSE, lag = -1,
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = "AutoArima", 
                             mod_covariates = FALSE, lag = 2.2, 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = "AutoArima", 
                             mod_covariates = FALSE, lag = "a", 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = "AutoArima", 
                             mod_covariates = FALSE, lag = 1:2, 
                             quiet = FALSE))
  expect_error(model_options(tree = dirtree(), model = "AutoArima", 
                             mod_covariates = FALSE, lag = NULL, quiet = 1))

  opts <- model_options(tree = dirtree(), model = "AutoArima", 
                        mod_covariates = FALSE, lag = NULL, quiet = FALSE)
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