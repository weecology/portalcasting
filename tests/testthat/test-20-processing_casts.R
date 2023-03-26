context(desc = "cast processing functions")

main <- "./testing"

test_that(desc = "read_cast_tab reads in a cast tab", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_cast_tab(main = main, cast_id = NULL), "data.frame")
  expect_error(read_cast_tab(main = main, cast_id = 1e10))

})

test_that(desc = "read_cast_tabs reads in multiple cast tabs", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_cast_tabs(main = main, cast_ids = NULL), "data.frame")
  expect_is(read_cast_tabs(main = main, cast_ids = c("1-1", "1-2")), "data.frame")

})


test_that(desc = "read_cast_metadata reads in the metadata", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_cast_metadata(main = main, cast_id = NULL), "list")
  expect_error(read_cast_metadata(main = main, cast_id = 1e10))

})


test_that(desc = "read_model_fit reads in model fits", {

  # download is held back on cran

    skip_on_cran() 

  #expect_is(read_model_fit(main = main, cast_id = NULL), "list")
  expect_error(read_model_fit(main = main, cast_id = 1e10))

})


test_that(desc = "add_obs_to_cast_tab functions add properly", {

  # download is held back on cran

    skip_on_cran() 

   cast_tab <- read_cast_tab(main = main, cast_id = "1-1")
   expect_is(add_obs_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")


})


test_that(desc = "read_model_cast reads in model casts", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_model_cast(main = main, cast_id = NULL), "forecast")
  expect_error(read_model_cast(main = main, cast_id = 1e10))

})