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
  expect_is(read_cast_tabs(main = main, cast_ids = 1:2), "data.frame")

})

test_that(desc = "read_cast_metadata reads in the metadat", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_cast_metadata(main = main, cast_id = NULL), "list")
  expect_error(read_cast_metadata(main = main, cast_id = 1e10))

})

test_that(desc = "read_model_fit reads in model fits", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_model_fit(main = main, cast_id = NULL), "list")
  expect_error(read_model_fit(main = main, cast_id = 1e10))

})

test_that(desc = "read_model_cast reads in model casts", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(read_model_cast(main = main, cast_id = NULL), "list")
  expect_error(read_model_cast(main = main, cast_id = 1e10))

})

test_that(desc = "add_XXX_to_cast_tab functions add properly", {

  # download is held back on cran

    skip_on_cran() 

   cast_tab <- read_cast_tab(main = main, cast_id = 1)
   expect_is(add_lead_to_cast_tab(main = main, cast_tab = cast_tab),
             "data.frame")
   expect_is(add_obs_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")
   expect_is(add_err_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")
   expect_is(add_covered_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")

})

test_that(desc = "measure_cast_level_error", {

  # download is held back on cran

    skip_on_cran() 

  cast_choices <- select_casts(main = main)
  cast_tab <- read_cast_tabs(main = main, cast_ids = 1)
  cast_tab <- add_obs_to_cast_tab(main = main, cast_tab = cast_tab)
  cast_tab <- add_err_to_cast_tab(main = main, cast_tab = cast_tab)
  cast_tab <- add_lead_to_cast_tab(main = main, cast_tab = cast_tab)
  cast_tab <- add_covered_to_cast_tab(main = main, cast_tab = cast_tab)
  expect_is(measure_cast_level_error(cast_tab), "data.frame")

})
