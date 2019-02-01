context("Test prepare_metadata functions")

options_all <- all_options(main = "testing_casting")

moons <- prep_moons(options_all$options_data$moons)
rodents <- prep_rodents(moons, options_all$options_data$rodents)
covariates <- prep_covariates(moons, options_all$options_data$covariates)
options_metadata <- options_all$options_data$metadata
options_metadata2 <- options_metadata
options_metadata2$quiet <- TRUE
options_metadata3 <- options_metadata
options_metadata3$cast_type <- "hindcasts"

test_that("prep_metadata", {
  expect_error(prep_metadata(1, rodents, covariates, options_metadata))
  expect_error(prep_metadata(moons, 1, covariates, options_metadata))
  expect_error(prep_metadata(moons, rodents, 1, options_metadata))
  expect_error(prep_metadata(moons, rodents, covariates, 1))
  expect_message(
     md1 <- prep_metadata(moons, rodents, covariates, options_metadata))
  expect_silent(
     md2 <- prep_metadata(moons, rodents, covariates, options_metadata2))
  expect_is(md1, "metadata")
  expect_is(md2, "metadata")
  expect_equal(md1, md2)
  expect_message(
     md3 <- prep_metadata(moons, rodents, covariates, options_metadata3))
  expect_is(md3, "metadata")
})