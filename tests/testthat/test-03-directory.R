context("Test directory functions")

test_that("setup_dir", {
  expect_error(setup_dir(1))
  expect_silent(setup_dir(all_options(main = "ok", quiet = TRUE)))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})

test_that("create_dir", {
  expect_error(create_dir(1))
})

test_that("create_main_dir", {
  expect_error(create_main_dir(1))
  expect_message(create_main_dir(dir_options(main = "ok")))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})

test_that("create_sub_dirs", {
  expect_error(create_sub_dirs(1))
})

test_that("create_sub_dir", {
  expect_error(create_sub_dir(1))
  expect_equal(create_sub_dir(), NULL)
  create_main_dir(dir_options(main = "ok", quiet = TRUE))
  expect_message(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  create_main_dir(dir_options(main = "ok", quiet = TRUE))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1],
                               quiet = TRUE))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})



