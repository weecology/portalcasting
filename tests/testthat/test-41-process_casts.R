context(desc = "cast processing functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))

test_that(desc = "read_cast_tab reads in a cast tab", {


  skip_on_cran() 

  expect_is(read_cast_tab(main = main2, cast_id = NULL), "data.frame")
  expect_error(read_cast_tab(main = main2, cast_id = 1e10))

})

test_that(desc = "read_cast_tabs reads in multiple cast tabs", {


  skip_on_cran() 

  expect_is(read_cast_tabs(main = main2, cast_ids = NULL), "data.frame")
  expect_is(read_cast_tabs(main = main2, cast_ids = c("1.01", "1.02")), "data.frame")

})


test_that(desc = "read_cast_metadata reads in the metadata", {


  skip_on_cran() 

  expect_is(read_cast_metadata(main = main2, cast_id = NULL), "list")
  expect_error(read_cast_metadata(main = main2, cast_id = 1e10))

})


test_that(desc = "read_model_fit reads in model fits", {


  skip_on_cran() 

  #expect_is(read_model_fit(main = main2, cast_id = NULL), "list")
  expect_error(read_model_fit(main = main2, cast_id = 1e10))

})


test_that(desc = "add_obs_to_cast_tab functions add properly", {


  skip_on_cran() 

   cast_tab <- read_cast_tab(main = main2, cast_id = "1.01")
   expect_is(add_obs_to_cast_tab(main = main2, cast_tab = cast_tab), 
             "data.frame")


})


test_that(desc = "read_model_cast reads in model casts", {


  skip_on_cran() 

  expect_is(read_model_cast(main = main2, cast_id = NULL), "forecast")
  expect_error(read_model_cast(main = main2, cast_id = 1e10))

})