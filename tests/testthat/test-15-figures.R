context("Figures")

main <- "./testing"

test_that("plot_cast_point", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_silent(plot_cast_point(main = main))
  expect_silent(plot_cast_point(main = main, with_census = TRUE))
  expect_error(plot_cast_point(main = main, cast_id = 1e10))

})

test_that("plot_cast_ts", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_silent(plot_cast_ts(main = main))
  expect_silent(plot_cast_ts(main = main, species = "DM"))
  expect_error(plot_cast_ts(main = main, cast_id = 1e10))

})


test_that("plot_casts_err_lead", {
  skip_on_cran() # downloads and casting take too long to run on cran
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
})


test_that("plot_casts_cov_RMSE", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_silent(plot_casts_cov_RMSE(main = main))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(plot_casts_cov_RMSE(main = main, cast_tab = cast_tab,
                                   cast_id = 1e10))
})