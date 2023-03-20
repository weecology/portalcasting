context(desc = "portalcast functions")

main <- "./testing"

test_that(desc = "portalcast works as basic", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(portalcast(main = main, models = "AutoArima"))

})

