context("Figure functions")

main <- "./testing"

test_that("plot_cast_point", {

  # download is held back on cran

    skip_on_cran() 

  portalcast(main = main, 
             models = c("AutoArima", "NaiveArima"), end_moons = 515:516)
  expect_silent(plot_cast_point(main = main, moon = 520))
  expect_silent(plot_cast_point(main = main, moon = 520, model = "AutoArima"))
  expect_silent(plot_cast_point(main = main, moon = 520, with_census = TRUE))
  expect_error(plot_cast_point(main = main, cast_id = 1e10))

})



test_that("plot_cast_ts", {

  # download is held back on cran

    skip_on_cran() 

  portalcast(main = main, models = c("AutoArima", "NaiveArima"))
  expect_silent(plot_cast_ts(main = main, species = "DM", 
                             model = "AutoArima"))
  expect_silent(plot_cast_ts(main = main, species = "DM"))
  expect_error(plot_cast_ts(main = main, species = "DM", cast_id = 1e10))

})


test_that("plot_casts_err_lead", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_casts_err_lead(main = main))
  expect_silent(plot_casts_err_lead(main = main, models = "AutoArima", 
                                    ensemble = FALSE, species = "total", 
                                    data_set = "all"))
  expect_silent(plot_casts_err_lead(main = main, models = "AutoArima", 
                                    ensemble = FALSE,
                                   species = "BA", data_set = "all"))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(plot_casts_err_lead(main = main, cast_tab = cast_tab,
                                   cast_id = 1e10))
  expect_error(plot_casts_err_lead(main = main, cast_id = 1e10))

})


test_that("plot_casts_cov_RMSE", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_casts_cov_RMSE(main = main))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(plot_casts_cov_RMSE(main = main, cast_tab = cast_tab,
                                   cast_id = 1e10))
  expect_error(plot_casts_cov_RMSE(main = main, cast_id = 1e10))

})