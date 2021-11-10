context("ensembling functions")

main <- "./testing"

test_that("ensemble_casts ensembles accordingly", {

  # download is held back on cran

    skip_on_cran() 

  cast_tab <- read_cast_tabs(main = main)
  em <- cast_tab$end_moon[1]
  expect_is(ensemble_casts(main = main, end_moon = em), "data.frame")

  expect_error(ensemble_casts(main = main, end_moon = em, cast_id = 1e10))

  expect_error(ensemble_casts(main = main, end_moon = em, cast_tab = cast_tab,
                                   cast_id = 1e10))

})