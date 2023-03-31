context(desc = "ensembling functions")

main <- "./testing"

test_that(desc = "ensemble_casts ensembles accordingly", {

  # download is held back on cran

    skip_on_cran() 
  cast_ids <- select_casts(main)$cast_id
  cast_id <- cast_ids[length(cast_ids)]

  expect_is(ensemble_casts(main     = main, 
                           cast_ids = cast_id), "data.frame")
  expect_error(ensemble_casts(main = main, cast_id = 1e10))

  expect_error(ensemble_casts(main = main, historic_end_newmoonnumber = 1e10))

})