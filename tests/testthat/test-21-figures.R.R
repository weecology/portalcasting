context("Test figure functions")

tree <- dirtree(main = "testing_casting");
metadata <- read_data(tree, "metadata")
cast_date <- metadata$forecast_date
cleanup_dir(all_options(main = "testing_casting", to_cleanup = "predictions"))
spath <- sub_paths(dirtree(main = "testing_casting", 
                           subs = subdirs("predictions")))
create_sub_dir(spath)
options_all3 <- all_options(main = "testing_casting", 
                            model = models("AutoArima"))
portalcast(options_all3)

test_that("plot_species_casts", {
  expect_silent(plot_species_casts(tree, cast_date = cast_date))
})

test_that("sppcastsplot_yaxis", {
  species = NULL
  level = "Controls"
  cast_type = "forecasts"
  model = "Ensemble"
  lead = 1

  metadata <- read_data(tree, "metadata")
  obs <- read_data(tree, tolower(level))
  newmoonnumber <- metadata$rodent_forecast_newmoons[lead]
  pred <- read_casts(tree, cast_type = cast_type, cast_date = cast_date) %>%
          select_casts(species = species, level = level, model = model,
                       newmoonnumber = newmoonnumber)  
  expect_silent(sppcastsplot_yaxis(tree = tree, species = pred$species))

})

test_that("plot_cast", {
  expect_silent(plot_cast(tree, species = "total", cast_date = cast_date))
  expect_silent(plot_cast(tree, species = "NA", cast_date = cast_date))
})

test_that("castplot_xaxis", {
  expect_silent(castplot_xaxis(tree, c(300, 410)))
})

test_that("castplot_ylab", {
  expect_silent(ylab <- castplot_ylab(tree, "total"))
  expect_is(ylab, "list")
  expect_equal(length(ylab), 2)
  expect_equal(names(ylab), c("text", "font"))
  expect_silent(ylab <- castplot_ylab(tree, "BA"))
  expect_is(ylab, "list")
  expect_equal(length(ylab), 2)
  expect_equal(names(ylab), c("text", "font"))
})
