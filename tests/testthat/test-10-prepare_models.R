context("Test prepare_models functions")

test_that("model_names", {
  expect_is(model_names(model_set = "prefab"), "character")
  expect_error(model_names(model_set = 1), "`model_set` is not")
  expect_error(model_names(add = 1), "`add` is not")
  expect_error(model_names(model_set = c("prefab", "prefab")), 
               "`model_set` can only be")
  expect_error(model_names(model_set = "ok"), "`model_set` must be")
})

test_that("write_model", {
  expect_error(write_model(1), "`options_model` is not")
  expect_output(
    write_model(AutoArima_options(dirtree(main = "testing_casting"))))
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
                                        subs = subdirs(subs_names = "test"))))
  expect_is(temp3, "character")
  expect_equal(length(temp3), 1)
})