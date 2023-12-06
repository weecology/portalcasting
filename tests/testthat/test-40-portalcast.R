context(desc = "portalcast functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "portalcast works as basic", {


  skip_on_cran()

  expect_message(portalcast(main = main2, models = "AutoArima"))

})