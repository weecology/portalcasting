context("Test prepare_metadata functions")

options_all <- all_options(main = "testing_casting")

moons <- prep_moons(options_all$options_data$moons)
rodents_list <- prep_rodents_list(moons, options_all$options_data$rodents)
covariates <- prep_covariates(moons, options_all$options_data$covariates)
options_metadata <- options_all$options_data$metadata
options_metadata3 <- options_metadata
options_metadata3$cast_type <- "hindcasts"

test_that("prep_metadata", {
  expect_error(prep_metadata(1, rodents_list, covariates, options_metadata))
  expect_error(prep_metadata(moons, 1, covariates, options_metadata))
  expect_error(prep_metadata(moons, rodents_list, 1, options_metadata))
  expect_error(prep_metadata(moons, rodents_list, covariates, 1))
  expect_message(
     md1 <- prep_metadata(moons, rodents_list, covariates, options_metadata))
  expect_is(md1, "metadata")
  expect_message(
     md3 <- prep_metadata(moons, rodents_list, covariates, options_metadata3))
  expect_is(md3, "metadata")
})