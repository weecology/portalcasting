context("directory creating functions")

main <- "./testing"
# remove the testing directory if it already existed for some reason
unlink(main_path(main = main), recursive = TRUE, force = TRUE)

test_that("create_dir fully creates the folder structure", {
  expect_message(create_dir(main = main))
})

test_that("verify throws needed errors", {
  expect_error(verify(main_path("abcdefghijklmop")))
  expect_error(verify(c(main_path("abcdef"), main_path("ghijklmop"))))
})
