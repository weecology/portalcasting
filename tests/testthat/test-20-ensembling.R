context(desc = "ensembling functions")

main <- "./testing"

test_that(desc = "ensemble_casts ensembles accordingly", {

  # download is held back on cran

    skip_on_cran() 

  casts_metadata <- read_casts_metadata(main     = main,
                                        quiet    = quiet)

end_moon <- max(casts_metadata$end_moon)

  expect_is(ensemble_casts(main     = main, 
                           cast_tab = NULL,
                           end_moon = end_moon,
                           models   = NULL, 
                           rodent_dataset = NULL,
                           species  = NULL), "data.frame")


  expect_error(ensemble_casts(main = main, cast_id = 1e10))

  expect_error(ensemble_casts(main = main, end_moon = 1e10))

})