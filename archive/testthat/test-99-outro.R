context("cleaning house")

main <- "./testing"

test_that("placeholder", {

  expect_equal(1, 1)

})

  unlink(main_path(main = main), recursive = TRUE, force = TRUE)
  #unlink(main_path(main = "./sand"), recursive = TRUE, force = TRUE)
  #unlink(main_path(main = "./prod"), recursive = TRUE, force = TRUE)
