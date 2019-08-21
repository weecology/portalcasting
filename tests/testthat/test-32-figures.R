context("Test figure functions")

test_that("plot_cast_point", {
  skip_on_cran() # downloads take too long for cran checks
  fill_data(main = "./testing")
  expect_silent(plot_cast_point(main = "./testing"))
  expect_silent(plot_cast_point(main = "./testing", with_census = TRUE))
})

test_that("plot_cast_ts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_cast_ts(main = "./testing"))
  expect_silent(plot_cast_ts(main = "./testing", species = "NA"))
})

test_that("plot_cov_RMSE_mod_spp(", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_cov_RMSE_mod_spp(main = "./testing"))
})

test_that("plot_err_lead_spp_mods", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_err_lead_spp_mods(main = "./checks"))
})

