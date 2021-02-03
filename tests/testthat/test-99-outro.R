context("cleaning house")

main <- "./testing"

test_that("we can clean", {

  unlink(main_path(main = main), recursive = TRUE, force = TRUE)
  unlink(main_path(main = main), recursive = TRUE, force = TRUE)
  unlink(main_path(main = main), recursive = TRUE, force = TRUE)
  unlink(main_path(main = main), recursive = TRUE, force = TRUE)
  unlink(main_path(main = main), recursive = TRUE, force = TRUE)
  unlink(main_path(main = main), recursive = TRUE, force = TRUE)


  expect_equal(1, 1)

})
