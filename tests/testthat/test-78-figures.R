context("Test figure functions")

test_that("plot_cast_point", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_cast_point(main = "./sand"))
  expect_silent(plot_cast_point(main = "./sand", with_census = TRUE))
  expect_error(plot_cast_point(main = "./sand", species = "PI"))

})

test_that("plot_cast_ts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_cast_ts(main = "./sand"))
  expect_silent(plot_cast_ts(main = "./sand", species = "NA"))
  expect_error(plot_cast_ts(main = "./sand", species_id = "PI"))
})

test_that("plot_cov_RMSE_mod_spp(", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_cov_RMSE_mod_spp(main = "./sand"))
})

test_that("plot_err_lead_spp_mods", {
  skip_on_cran() # downloads take too long for cran checks
  expect_silent(plot_err_lead_spp_mods(main = "./sand"))
  expect_silent(plot_err_lead_spp_mods(main = "./sand", ndates = NULL))
  expect_silent(plot_err_lead_spp_mods(main = "./sand", 
               species = rodent_species(set = "evalplot", total = TRUE)))
  expect_error(plot_err_lead_spp_mods(main = "./sand", species = "PI", 
                                      models = "nbsGARCH"))

})

