context("Test figure functions")

tree <- dirtree(main = "testing_casting");
metadata <- read_data(tree, "metadata")
cast_date <- as.Date(metadata$forecast_date)
cleanup_dir(all_options(main = "testing_casting", to_cleanup = "predictions"))
spath <- sub_paths(dirtree(main = "testing_casting", 
                           subs = subdirs("predictions")))
create_sub_dir(spath)
options_all3 <- all_options(main = "testing_casting", 
                            model = models("AutoArima"))
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

test_that("plotcastpoint_yaxis", {
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
  expect_silent(plotcastpoint_yaxis(tree = tree, species = pred$species))
  expect_error(plotcastpoint_yaxis(1))
  expect_error(plotcastpoint_yaxis(tree, "ok"))
  expect_error(plotcastpoint_yaxis(tree, 1))
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
  expect_error(plot_cast_ts(tree, start_newmoon = 0))
  expect_error(plot_cast_ts(tree, start_newmoon = 1:2))
  expect_error(plot_cast_ts(tree, start_newmoon = 1.5))
})

test_that("plotcastts_xaxis", {
  expect_silent(plotcastts_xaxis(tree, c(300, 410)))
  expect_error(plotcastts_xaxis("ok", 1:2))
  expect_error(plotcastts_xaxis(tree, rep("ok", 2)))
  expect_error(plotcastts_xaxis(tree, 1:3))
  expect_error(plotcastts_xaxis(tree, c(300.1, 410)))
})

test_that("plotcastts_ylab", {
  expect_silent(ylab <- plotcastts_ylab(tree, "total"))
  expect_is(ylab, "list")
  expect_equal(length(ylab), 2)
  expect_equal(names(ylab), c("text", "font"))
  expect_silent(ylab <- plotcastts_ylab(tree, "BA"))
  expect_is(ylab, "list")
  expect_equal(length(ylab), 2)
  expect_equal(names(ylab), c("text", "font"))

  expect_error(plotcastts_ylab(1))
  expect_error(plotcastts_ylab(tree, species = 1))
  expect_error(plotcastts_ylab(tree, species = "ok"))
  expect_error(plotcastts_ylab(tree, species = rep("total", 2)))
})

test_that("select_most_ab_spp", {
  expect_silent(spp <- select_most_ab_spp(tree = tree, cast_date = cast_date))
  expect_is(spp, "character")
  expect_equal(length(spp), 3)
  expect_silent(spp2 <- select_most_ab_spp(tree = tree, cast_date = NULL))
  expect_is(spp2, "character")
  expect_equal(length(spp2), 3)
  expect_equal(spp, spp2)
  expect_error(select_most_ab_spp(topx = "ok", 
      tree = tree, cast_date = cast_date))
  expect_error(select_most_ab_spp(topx = 1:2, 
      tree = tree, cast_date = cast_date))
  expect_error(select_most_ab_spp(topx = 1.2, 
      tree = tree, cast_date = cast_date))
  expect_error(select_most_ab_spp(tree = 1))
  expect_error(select_most_ab_spp(tree = tree, species = 1))
  expect_error(select_most_ab_spp(tree = tree, species = "ok"))
  expect_error(select_most_ab_spp(tree = tree, level = 1))
  expect_error(select_most_ab_spp(tree = tree, level = "ok" ))
  expect_error(select_most_ab_spp(tree = tree, level = rep("All", 2)))
  expect_error(select_most_ab_spp(tree = tree, cast_type = 1))
  expect_error(select_most_ab_spp(tree = tree, cast_type = "ok"))
  expect_error(
     select_most_ab_spp(tree = tree, cast_type = rep("forecasts", 2)))
  expect_error(select_most_ab_spp(tree = tree, cast_date = 1))
  expect_error(select_most_ab_spp(tree = tree, cast_date = rep(cast_date, 2)))
  expect_error(select_most_ab_spp(tree = tree, lead = 0))
  expect_error(select_most_ab_spp(tree = tree, lead = 1:2))
  expect_error(select_most_ab_spp(tree = tree, lead = 1.5))
  expect_error(select_most_ab_spp(tree = tree, from_date = 1))
  expect_error(select_most_ab_spp(tree = tree, from_date = rep(cast_date, 2)))

})

test_that("plot_err_lead_spp_mods", {
  expect_silent(plot_err_lead_spp_mods(tree))
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

