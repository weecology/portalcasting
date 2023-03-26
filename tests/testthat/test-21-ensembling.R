context(desc = "ensembling functions")

main <- "./testing"

test_that(desc = "ensemble_casts ensembles accordingly", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(ensemble_casts(main                       = main, 
                           species                    = "DM", 
                           dataset                    = "controls"), "data.frame")
  expect_error(ensemble_casts(main = main, cast_id = 1e10))

  expect_error(ensemble_casts(main = main, end_moon = 1e10))

})