context(desc = "setting up house")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

#example(stan_model, package = "rstan", run.dontrun = TRUE)

#test_that(desc = "stan",
 #         code = {

  #(x <- example(stan_model, package = "rstan", run.dontrun = TRUE))
  #expect_is(x, "list")
#})



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
