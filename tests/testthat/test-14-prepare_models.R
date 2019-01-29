context("Test prepare_models functions")

test_that("models", {
  expect_is(models(), "character")
  expect_is(models(), "models")
  expect_error(models(1), "`set` is not")
  expect_error(models(add = 1), "`add` is not")
  expect_error(models(set = c("prefab", "prefab")), "`set` can only be")
  expect_error(models(set = "ok"), "not defined for that `set`")
})

test_that("write_model", {
  expect_error(write_model(1), "`options_model` is not")
})

test_that("model_template", {
  expect_error(write_model(1), "`options_model` is not")
})