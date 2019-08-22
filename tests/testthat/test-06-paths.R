context("Test paths functions")


test_that("file_ext", {
  expect_equal(file_ext("home/folders.with.dots/stuff/ok.csv"), "csv")
  expect_equal(path_no_ext("home/folders.with.dots/stuff/ok.csv"),
                           "home/folders.with.dots/stuff/ok")
  expect_equal(file_ext(NMME_urls()[[1]]), "")
  expect_equal(file_ext(NMME_urls()[[1]], "="), "csv")
})

test_that("subdirs", {
  expect_is(subdirs(), "character")
  expect_equal(length(subdirs()), 5)
  expect_error(subdirs(NULL, "ok"))
  expect_null(subdirs(NULL, NULL))
})


test_that("main_path", {
  expect_is(main_path(), "character")
  expect_equal(length(main_path()), 1)
})

test_that("sub_paths", {
  expect_is(sub_paths(), "character")
  expect_equal(length(sub_paths()), 5)
  expect_equal(length(sub_paths(specific_subs = "tmp")), 1)
  expect_error(sub_paths(specific_subs = "ok"))
})

test_that("model_paths", {
  expect_null(model_paths())
  expect_is(model_paths(models = "AutoArima"), "character")
  expect_is(model_paths(models = c("AutoArima", "ESSS")), "character")
  expect_equal(length(model_paths(models = "AutoArima")), 1)
  expect_equal(length(model_paths(models = c("AutoArima", "ESSS"))), 2)
})


test_that("file_paths", {
  expect_is(file_paths(local_paths = "xxx.csv"), "character")
  expect_equal(length(file_paths(local_paths = "xxx.csv")), 1)
  expect_null(file_paths())
})