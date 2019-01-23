context("Test directory functions")

test_that("setup_dir", {
  expect_error(setup_dir(1))
  expect_silent(setup_dir(all_options(main = "ok", quiet = TRUE)))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})

