context("path generating and manipulating functions")


test_that("file_ext extracts the file extension", {

  expect_equal(file_ext("home/folders.with.dots/stuff/ok.csv"), "csv")
  expect_equal(file_ext(NMME_urls()[[1]]), "")
  expect_equal(file_ext(NMME_urls()[[1]], "="), "csv")

})

test_that("path_no_ext extracts the path without the file extension", {

  expect_equal(path_no_ext("home/folders.with.dots/stuff/ok.csv"),
                           "home/folders.with.dots/stuff/ok")

})

test_that("main_path produces expected value", {

  expect_is(main_path(), "character")
  expect_equal(length(main_path()), 1)

})

test_that("sub_path gives a proper path", {

  expect_is(sub_path(subs = "tmp"), "character")
  expect_equal(length(sub_path(subs = "tmp")), 1)
  expect_equal(length(sub_path()), 0)
  expect_error(sub_path(subs = "ok"))

})

test_that("files_control creates a control list for files", {

  fc <- files_control()
  expect_is(fc, "list")

})

test_that("file_path gives a proper path", {

  expect_is(file_path(files = "xxx.csv"), "character")
  expect_equal(length(file_path(files = "xxx.csv")), 1)
  expect_null(file_path())

})