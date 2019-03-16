context("Test figure functions")

tree <- dirtree(main = "testing_casting");
data_opts <- all_options(main = "testing_casting")$options_data
moons <- prep_moons(data_opts$moons)
rodents_opts <- data_opts$rodents
rodents_opts <- enforce_rodents_options(rodents_opts, "all")
prep_rodents(moons, rodents_opts)
rodents_opts <- enforce_rodents_options(rodents_opts, "controls")
prep_rodents(moons, rodents_opts)
metadata <- read_data(tree, "metadata")
cast_date <- as.Date(metadata$forecast_date)
cleanup_dir(all_options(main = "testing_casting", to_cleanup = "predictions"))
spath <- sub_paths(dirtree(main = "testing_casting", subs = "predictions"))
create_sub_dir(spath)
options_all3 <- all_options(main = "testing_casting", model = "AutoArima")
portalcast(options_all3)

test_that("plot_cast_point", {
  expect_silent(plot_cast_point(tree, cast_date = cast_date))
  expect_silent(plot_cast_point(tree))
  expect_error(plot_cast_point(1))
  expect_error(plot_cast_point(tree, species = 1))
  expect_error(plot_cast_point(tree, species = "ok"))
  expect_error(plot_cast_point(tree, level = 1))
  expect_error(plot_cast_point(tree, level = "ok" ))
  expect_error(plot_cast_point(tree, level = rep("All", 2)))
  expect_error(plot_cast_point(tree, cast_type = 1))
  expect_error(plot_cast_point(tree, cast_type = "ok"))
  expect_error(plot_cast_point(tree, cast_type = rep("forecasts", 2)))
  expect_error(plot_cast_point(tree, cast_date = 1))
  expect_error(plot_cast_point(tree, cast_date = rep(cast_date, 2)))
  expect_error(plot_cast_point(tree, lead = 0))
  expect_error(plot_cast_point(tree, lead = 1:2))
  expect_error(plot_cast_point(tree, lead = 1.5))
  expect_error(plot_cast_point(tree, from_date = 1))
  expect_error(plot_cast_point(tree, from_date = rep(cast_date, 2)))
})

test_that("plot_cast_point_yaxis", {
  species = NULL
  level = "Controls"
  cast_type = "forecasts"
  model = "Ensemble"
  lead = 1

  metadata <- read_data(tree, "metadata")
  obs <- read_data(tree, tolower(level))
  newmoonnumber <- metadata$rodent_forecast_newmoons[lead]
  pred <- read_cast(tree, cast_type = cast_type, cast_date = cast_date) %>%
          select_casts(species = species, level = level, model = model,
                       newmoonnumber = newmoonnumber)  
  expect_silent(plot_cast_point_yaxis(tree = tree, species = pred$species))
  expect_error(plot_cast_point_yaxis(1))
  expect_error(plot_cast_point_yaxis(tree, "ok"))
  expect_error(plot_cast_point_yaxis(tree, 1))
})

test_that("plot_cast_ts", {
  expect_silent(plot_cast_ts(tree, species = "total", cast_date = cast_date))
  expect_silent(plot_cast_ts(tree, species = "NA", cast_date = cast_date))

  expect_error(plot_cast_ts(1))
  expect_error(plot_cast_ts(tree, species = 1))
  expect_error(plot_cast_ts(tree, species = "ok"))
  expect_error(plot_cast_ts(tree, species = rep("total", 2)))
  expect_error(plot_cast_ts(tree, level = 1))
  expect_error(plot_cast_ts(tree, level = "ok" ))
  expect_error(plot_cast_ts(tree, level = rep("All", 2)))
  expect_error(plot_cast_ts(tree, cast_type = 1))
  expect_error(plot_cast_ts(tree, cast_type = "ok"))
  expect_error(plot_cast_ts(tree, cast_type = rep("forecasts", 2)))
  expect_error(plot_cast_ts(tree, cast_date = 1))
  expect_error(plot_cast_ts(tree, cast_date = rep(cast_date, 2)))
  expect_error(plot_cast_ts(tree, add_obs = 1))
  expect_error(plot_cast_ts(tree, add_obs = rep(TRUE, 2)))
  expect_error(plot_cast_ts(tree, start_newmoon = 0))
  expect_error(plot_cast_ts(tree, start_newmoon = 1:2))
  expect_error(plot_cast_ts(tree, start_newmoon = 1.5))
})

test_that("plot_cast_ts_xaxis", {
  expect_silent(plot_cast_ts_xaxis(tree, c(300, 410)))
  expect_error(plot_cast_ts_xaxis("ok", 1:2))
  expect_error(plot_cast_ts_xaxis(tree, rep("ok", 2)))
  expect_error(plot_cast_ts_xaxis(tree, 1:3))
  expect_error(plot_cast_ts_xaxis(tree, c(300.1, 410)))
})

test_that("plot_cast_ts_ylab", {
  expect_silent(ylab <- plot_cast_ts_ylab(tree, species = "total"))
  expect_is(ylab, "list")
  expect_equal(length(ylab), 2)
  expect_equal(names(ylab), c("text", "font"))
  expect_silent(ylab <- plot_cast_ts_ylab(tree, species = "BA"))
  expect_is(ylab, "list")
  expect_equal(length(ylab), 2)
  expect_equal(names(ylab), c("text", "font"))

  expect_error(plot_cast_ts_ylab(1))
  expect_error(plot_cast_ts_ylab(tree, species = 1))
  expect_error(plot_cast_ts_ylab(tree, species = "ok"))
  expect_error(plot_cast_ts_ylab(tree, species = rep("total", 2)))
})

test_that("plot_err_lead_spp_mods", {
  expect_error(plot_err_lead_spp_mods(1))
  expect_error(plot_err_lead_spp_mods(tree, cast_type = 1))
  expect_error(plot_err_lead_spp_mods(tree, cast_type = "ok"))
  expect_error(plot_err_lead_spp_mods(tree, cast_type = rep("forecasts", 2)))
  expect_error(plot_err_lead_spp_mods(tree, species = 1))
  expect_error(plot_err_lead_spp_mods(tree, species = "ok"))
  expect_error(plot_err_lead_spp_mods(tree, level = 1))
  expect_error(plot_err_lead_spp_mods(tree, level = "ok"))
  expect_error(plot_err_lead_spp_mods(tree, level = rep("All", 2)))
  expect_error(plot_err_lead_spp_mods(tree, ndates = 1.5))
  expect_error(plot_err_lead_spp_mods(tree, ndates = "ok"))
  expect_error(plot_err_lead_spp_mods(tree, ndates = 1:5))
})

test_that("plot_cov_RMSE_mod_spp", {
  expect_error(plot_cov_RMSE_mod_spp(1))
  expect_error(plot_cov_RMSE_mod_spp(tree, cast_type = 1))
  expect_error(plot_cov_RMSE_mod_spp(tree, cast_type = "ok"))
  expect_error(plot_cov_RMSE_mod_spp(tree, cast_type = rep("forecasts", 2)))
  expect_error(plot_cov_RMSE_mod_spp(tree, species = 1))
  expect_error(plot_cov_RMSE_mod_spp(tree, species = "ok"))
  expect_error(plot_cov_RMSE_mod_spp(tree, level = 1))
  expect_error(plot_cov_RMSE_mod_spp(tree, level = "ok"))
  expect_error(plot_cov_RMSE_mod_spp(tree, level = rep("All", 2)))
  expect_error(plot_cov_RMSE_mod_spp(tree, min_observed = 1.5))
  expect_error(plot_cov_RMSE_mod_spp(tree, min_observed = "ok"))
  expect_error(plot_cov_RMSE_mod_spp(tree, min_observed = 1:5))
  expect_error(plot_cov_RMSE_mod_spp(tree, cast_dates = 1))
})