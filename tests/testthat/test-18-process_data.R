context("Test process_data functions")

rod_opts <- all_options(main = "testing_casting")$options_data$rodents
moon_opts <- all_options(main = "testing_casting")$options_data$moons
moons <- prep_moons(moon_opts)
rodents <- rodents_data(moons, rod_opts)

test_that("interpolate_abundance", {
  expect_error(interpolate_abundance(1), "`abundance` is not")
  expect_silent(rodents_int <- interpolate_abundance(rodents))
  expect_equal(nrow(rodents_int) > nrow(rodents), TRUE)
  expect_equal(ncol(rodents_int) < ncol(rodents), TRUE)
  expect_is(rodents_int, "data.frame")
})

