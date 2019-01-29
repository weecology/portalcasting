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
  expect_output(
    write_model(AutoArima_options(dirtree(main = "testing_casting"))))
  expect_silent(
    write_model(
      model_options(dirtree(main = "testing_casting"), name = NULL)))
})

test_that("model_template", {
  expect_error(model_template(1), "`options_model` is not")

  temp1 <- model_template(
              AutoArima_options(dirtree(main = "testing_casting")))
  expect_is(temp1, "character")
  expect_equal(length(temp1), 1)

  temp2 <- model_template(pevGARCH_options(dirtree(main = "testing_casting")))
  expect_is(temp2, "character")
  expect_equal(length(temp2), 1)

  temp3 <- model_template(AutoArima_options(dirtree(main = "testing_casting",
                                             subs = subdirs(subs = "test"))))
  expect_is(temp3, "character")
  expect_equal(length(temp3), 1)
})