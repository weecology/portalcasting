context(desc = "setting up house")

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
