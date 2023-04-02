context(desc = "portalcast functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))

test_that(desc = "portalcast works as basic", {


  skip_on_cran() 

  expect_message(portalcast(main = main2, models = "AutoArima"))

})