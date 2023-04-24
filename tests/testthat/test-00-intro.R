context(desc = "setting up house")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "main1",
          code = {

  expect_equal(file.exists(main1), FALSE)

})

test_that(desc = "main2",
          code = {

  expect_equal(file.exists(main2), FALSE)


})

test_that(desc = "main3",
          code = {

  expect_equal(file.exists(main3), FALSE)

})
