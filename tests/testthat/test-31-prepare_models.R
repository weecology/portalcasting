context(desc = "model preparation functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))


test_that(desc = "read_model_controls does", {

  skip_on_cran() 

  mc <- read_model_controls(main = main2)
  expect_is(mc, "list")

})



test_that(desc = "model_controls reads in a list", {

  skip_on_cran() 

  mc <- model_controls(main = main2)
  expect_is(mc, "list")

})



test_that(desc = "write_model_controls returns a list", {

  mc <- write_model_controls(main = main2)
  expect_is(mc, "list")

})



test_that(desc = "add_new_model makes a list", {

  skip_on_cran() 

  mc <- add_new_model(main = main2, new_model_controls = new_model_controls(metadata = new_model_metadata(name = "newone")))
  expect_is(mc, "list")

  mc <- add_new_model(main = main2, new_model_controls = new_model_controls(fit = new_model_fit(model_file = "jags_RW.txt"), metadata = new_model_metadata(name = "newonejags")))
  expect_is(mc, "list")

})



test_that(desc = "model_controls_template reads in the list", {

  mc <- model_controls_template( )
  expect_is(mc, "list")
  expect_equal(length(mc), 7)

})



test_that(desc = "new_model_<> functions update the list", {

  mc <- new_model_controls(extra_field = 1234)
  expect_is(mc, "list")
  expect_equal(names(mc)[length(mc)], "extra_field")
  expect_equal(mc[[length(mc)]], 1234)

  mc <- new_model_metadata(name = "xyz")
  expect_is(mc, "list")
  expect_equal(mc$name, "xyz")

  mc <- new_model_fit(fun = "lm", args = list(y = "abundance"))
  expect_is(mc, "list")
  expect_equal(mc$fun, "lm")
  expect_equal(mc$args, list(y = "abundance"))
  
  mc <- new_model_cast(fun = "predict")
  expect_is(mc, "list")
  expect_equal(mc$fun, "predict")

  mc <- new_model_interpolate(needed = TRUE, fun = "round_na.interp")
  expect_is(mc, "list")
  expect_equal(length(mc), 2)

  mc <- new_model_datasets()
  expect_is(mc, "list")
  expect_equal(length(mc), 3)

  mc <- new_model_response(link = "negative_binomial", type = "distribution")
  expect_is(mc, "list")

})



