context("ensembling functions")

main <- "./testing"

test_that("ensemble_casts ensembles accordingly", {

  # download is held back on cran

    skip_on_cran() 

  expect_is(ensemble_casts(main = main, end_moon = 515), "data.frame")

  expect_error(ensemble_casts(main = main, end_moon = 515, cast_id = 1e10))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(ensemble_casts(main = main, end_moon = 515, cast_tab = cast_tab,
                                   cast_id = 1e10))

})