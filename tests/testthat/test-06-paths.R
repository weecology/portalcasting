context("Test paths functions")


test_that("file_ext", {
  expect_equal(file_ext("home/folders.with.dots/stuff/ok.csv"), "csv")
  expect_equal(path_no_ext("home/folders.with.dots/stuff/ok.csv"),
                           "home/folders.with.dots/stuff/ok")
  expect_equal(file_ext(NMME_urls()[[1]]), "")
  expect_equal(file_ext(NMME_urls()[[1]], "="), "csv")
})


test_that("main_path", {
  expect_is(main_path(), "character")
  expect_equal(length(main_path()), 1)
})

test_that("sub_paths", {
  expect_is(sub_path(subs = "tmp"), "character")
  expect_equal(length(sub_path(subs = "tmp")), 1)
  expect_error(sub_path(subs = "ok"))
})

test_that("file_path", {
  expect_is(file_path(files = "xxx.csv"), "character")
  expect_equal(length(file_path(files = "xxx.csv")), 1)
  expect_null(file_path())
})