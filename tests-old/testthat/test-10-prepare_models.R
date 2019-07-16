context("Test prepare_models functions")

tree <- dirtree(main = "portalcasting")

test_that("model_scripts", {
  expect_silent(scripts <- model_scripts(tree))
  expect_is(scripts, "character")
  expect_error(model_scripts(1))
})

test_that("model_names", {
  expect_is(model_names(model_set = "prefab"), "character")
  expect_is(model_names(model_set = "wEnsemble"), "character")
  expect_error(model_names(model_set = 1))
  expect_error(model_names(add = 1))
  expect_error(model_names(model_set = c("prefab", "prefab")))
  expect_error(model_names(model_set = "ok"))
})

test_that("write_model", {
  expect_error(write_model(1))
  expect_message(
    write_model(AutoArima_options(dirtree(main = "testing_casting"))))
})

test_that("model_template", {
  expect_error(model_template(1))

  temp1 <- model_template(
              AutoArima_options(dirtree(main = "testing_casting")))
  expect_is(temp1, "character")
  expect_equal(length(temp1), 1)

  temp2 <- model_template(pevGARCH_options(dirtree(main = "testing_casting")))
  expect_is(temp2, "character")
  expect_equal(length(temp2), 1)

  temp3 <- model_template(AutoArima_options(dirtree(main = "testing_casting",
                                        subs = "test")))
  expect_is(temp3, "character")
  expect_equal(length(temp3), 1)
})