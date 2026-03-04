context(desc = "portalcast functions")

test_that(desc = "portalcast works as basic", {


  skip_on_cran()

  expect_message(portalcast(main = main2, models = "AutoArima"))

})