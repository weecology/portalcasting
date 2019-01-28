context("Test portalcast functions")

options_all1 <- all_options(main = "ok")
options_all2 <- all_options(main = "ok", quiet = TRUE)
options_all3 <- all_options(main = "ok", model = models(NULL, "AutoArima"))
options_all4 <- all_options(main = "ok", model = models(NULL, "ok"))
options_all5 <- all_options(main = "ok", model = models(NULL, "all"))
options_all6 <- all_options(main = "ok", model = models(NULL, "AutoArima"),
                            cast_type = "hindcasts", end = 490)

setup_dir(options_all1)

test_that("portalcast", {
  expect_error(portalcast(1))
  expect_message(portalcast(options_all3))
})

test_that("verify_models", {
  expect_error(verify_models(1))
  expect_error(verify_models())
  expect_message(verify_models(options_all1$options_cast))
  expect_message(verify_models(options_all2$options_cast))
  expect_error(verify_models(options_all4$options_cast))
})

test_that("cast_models", {
  expect_error(cast_models(1))
  expect_message(cast_models(options_all3$options_cast))
  expect_message(cast_models(options_all6$options_cast))
})

test_that("models_to_cast", {
  expect_error(models_to_cast(1))
  mtc3 <- models_to_cast(options_all3$options_cast)
  expect_is(mtc3, "character")
  expect_equal(length(mtc3), 1)
  mtc5 <- models_to_cast(options_all5$options_cast)
  expect_is(mtc5, "character")
  expect_equal(length(mtc5), length(models()))
})

test_that("create_tmp", {
  expect_error(create_tmp(1))
  expect_silent(create_tmp(dirtree(main = "ok")))
})

test_that("clear_tmp", {
  expect_error(clear_tmp(1))
  expect_silent(clear_tmp(dirtree(main = "ok")))
unlink(sub_path(dirtree(main = "ok"), "tmp"), recursive = TRUE, force = TRUE)
unlink(sub_path(dirtree(main = "ok"), "tmp"), recursive = TRUE, force = TRUE)
  expect_message(clear_tmp(dirtree(main = "ok")))
})

unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
