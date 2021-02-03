context("directory creating functions")

main <- "./testing"


test_that("create_dir fully creates the folder structure", {

  expect_message(create_dir(main = main))

  expect_true(file.exists(casts_path(main)))
  expect_true(file.exists(data_path(main)))
  expect_true(file.exists(fits_path(main)))
  expect_true(file.exists(models_path(main)))
  expect_true(file.exists(raw_path(main)))
  expect_true(file.exists(tmp_path(main)))

  expect_is(read_directory_config(main), "list")

})

test_that("verify throws needed errors", {
  expect_error(verify(main_path("abcdefghijklmop")))
  expect_error(verify(c(main_path("abcdef"), main_path("ghijklmop"))))
})

