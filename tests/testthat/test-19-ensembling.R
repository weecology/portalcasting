context("Test ensembling functions")

main <- "./testing"

test_that("ensemble_casts", {
  skip_on_cran() # downloads take too long for cran checks
  portalcast(main = main, models = c("AutoArima", "ESSS"), end_moons = 515:516)
  expect_is(ensemble_casts(main = main), "data.frame")

  expect_error(ensemble_casts(main = main, cast_id = 1e10))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(ensemble_casts(main = main, cast_tab = cast_tab,
                                   cast_id = 1e10))
})

