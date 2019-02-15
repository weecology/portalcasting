context("Test figure functions")

tree <- dirtree(main = "testing_casting");
metadata <- read_data(tree, "metadata")
castdate <- metadata$forecast_date
cleanup_dir(all_options(main = "testing_casting", to_cleanup = "predictions"))
spath <- sub_paths(dirtree(main = "testing_casting", 
                           subs = subdirs(NULL, "predictions")))
create_sub_dir(spath)
options_all3 <- all_options(main = "testing_casting", 
                            model = models(NULL, "AutoArima"))
portalcast(options_all3)

test_that("plot_species_casts", {

})

test_that("sppcastsplot_yaxis", {

})

test_that("plot_cast", {
  expect_silent(
plot_cast(tree, species = "total", model = "AutoArima", castdate = castdate)
)
  expect_silent(plot_cast(tree, species = "NA", model = "AutoArima", castdate = castdate))
})

test_that("castplot_xaxis", {
  #expect_silent(castplot_xaxis(tree, c(300, 410)))
})

test_that("castplot_ylab", {
  #expect_silent(ylab <- castplot_ylab(tree, "total"))
  #expect_is(ylab, "list")
  #expect_equal(length(ylab), 2)
  #expect_equal(names(ylab), c("text", "font"))
  #expect_silent(ylab <- castplot_ylab(tree, "BA"))
  #expect_is(ylab, "list")
  #expect_equal(length(ylab), 2)
  #expect_equal(names(ylab), c("text", "font"))
})
