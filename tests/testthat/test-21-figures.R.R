context("Test figure functions")

tree <- dirtree(main = "testing_casting");
metadata <- read_data(tree, "metadata")
castdate <- metadata$forecast_date

test_that("plot_species_casts", {

})

test_that("sppcastsplot_yaxis", {

})

test_that("plot_cast", {
  #expect_silent(plot_cast(tree, species = "total", castdate = castdate))
  #expect_silent(plot_cast(tree, species = "NA", castdate = castdate))
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
