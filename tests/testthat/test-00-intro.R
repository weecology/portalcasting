context(desc = "setting up house")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))


test_that(desc = "removal main1",
          code = {

  expect_equal((unlink(file.path(main1), recursive = TRUE, force = TRUE)), 0)

})

test_that(desc = "removal main2",
          code = {

  expect_equal((unlink(file.path(main2), recursive = TRUE, force = TRUE)), 0)

})

test_that(desc = "removal main3",
          code = {

  expect_equal((unlink(file.path(main3), recursive = TRUE, force = TRUE)), 0)

})
