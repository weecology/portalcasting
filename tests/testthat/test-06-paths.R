context("Test paths functions")

test_that("dirtree", {
  expect_is(dirtree(), "dirtree")
  expect_is(dirtree(base = "~"), "dirtree")
  expect_is(dirtree(main = "forecast_folder"), "dirtree")
  expect_equal(length(dirtree()), 3)
  expect_equal(length(dirtree(base = "~")), 3)
  expect_equal(length(dirtree(main = "forecast_folder")), 3)
  expect_error(dirtree(base = c("ok", "ok")))
  expect_error(dirtree(main = c("ok", "ok")))
  expect_error(dirtree(base = 1))
  expect_error(dirtree(main = 1))
  expect_error(dirtree(subs = 1))
})

test_that("subdirs", {
  expect_is(subdirs(subs_type = "portalcasting"), "character")
  expect_is(subdirs(subs_type = "portalcasting", subs_names = "ok"), 
                    "character")
  expect_is(subdirs(subs_type = NULL, subs_names = "ok"), "character")
  expect_equal(length(subdirs(subs_type = "portalcasting")), 5)
  expect_equal(length(subdirs(subs_type = "portalcasting", 
                              subs_names = "ok")), 6)
  expect_equal(length(subdirs(subs_type = NULL, subs_names = "ok")), 1)
  expect_equal(subdirs(subs_type = NULL), NULL)
  expect_error(subdirs(subs_type = "ok"))
  expect_error(subdirs(subs_type = "portalcasting", subs_names = 1))
})

test_that("base_path", {
  expect_is(base_path(), "character")
  expect_equal(length(base_path()), 1)
  expect_error(base_path(1))
})

test_that("main_path", {
  expect_is(main_path(), "character")
  expect_equal(length(main_path()), 1)
  expect_error(main_path(1))
})

test_that("sub_paths", {
  expect_is(sub_paths(), "character")
  expect_equal(length(sub_paths()), 5)
  expect_equal(length(sub_paths(specific_subs = NULL)), 5)
  expect_error(sub_paths(1))
  expect_is(sub_paths(specific_subs = "predictions"), "character")
  expect_is(sub_paths(specific_subs = "models"), "character")
  expect_is(sub_paths(specific_subs = "PortalData"), "character")
  expect_is(sub_paths(specific_subs = "data"), "character")
  expect_is(sub_paths(specific_subs = "tmp"), "character")
  expect_equal(length(sub_paths(specific_subs = "predictions")), 1)
  expect_equal(length(sub_paths(specific_subs = "models")), 1)
  expect_equal(length(sub_paths(specific_subs = "PortalData")), 1)
  expect_equal(length(sub_paths(specific_subs = "data")), 1)
  expect_equal(length(sub_paths(specific_subs = "tmp")), 1)
  expect_error(sub_paths(1, 1))
  expect_error(sub_paths(dirtree(), "ok"))
})

test_that("model_paths", {
  expect_is(model_paths(), "character")
  expect_is(model_paths(models = "AutoArima"), "character")
  expect_is(model_paths(models = c("AutoArima", "ESSS")), "character")
  expect_equal(length(model_paths(models = "AutoArima")), 1)
  expect_equal(length(model_paths(models = c("AutoArima", "ESSS"))), 2)
  expect_error(model_paths(tree = 1, models = "AutoArima"))
  expect_error(model_paths(models = 1))
  expect_error(model_paths(models = "AutoArima", extension = 1))
  expect_error(model_paths(models = "AutoArima", extension = "ok"))
  expect_error(model_paths(models = "AutoArima", extension = c("ok", "ok")))
  expect_error(model_paths(models = "AutoArima", extension = ".R."))
})


test_that("file_paths", {
  expect_error(file_paths(tree = 1, local_paths = "data/covariates.csv"))
  expect_error(file_paths())
  expect_error(file_paths(local_paths = 1))
  expect_is(file_paths(local_paths = "data/covariates.csv"), "character")
  expect_is(file_paths(local_paths = c("ok", "data/covar.csv")), "character")
  expect_equal(length(file_paths(local_paths = "data/covariates.csv")), 1)
  expect_equal(length(file_paths(local_paths = c("ok", "data/covar.csv"))), 2)
})