context("Clear test casting directories")

test_that("placehold", {
  expect_equal(1, 1)
})

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)
unlink(main_path(main = "./sand"), recursive = TRUE, force = TRUE)
unlink(main_path(main = "./prod"), recursive = TRUE, force = TRUE)
unlink(main_path(main = "./test"), recursive = TRUE, force = TRUE)