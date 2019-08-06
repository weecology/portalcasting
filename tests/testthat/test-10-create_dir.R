context("Test directory creating functions")

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)
test_that("create_dir", {
  expect_message(create_dir(main = "./testing"))
})

test_that("verify", {
  expect_error(verify(main_path(base ="abcdefghijklmop")))
})

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)
