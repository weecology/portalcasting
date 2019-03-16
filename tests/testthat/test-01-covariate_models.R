context("Test covariate_models functions")

test_that("covariate_models", {
  ref <- list(c("maxtemp", "meantemp", "precipitation", "ndvi"),
              c("maxtemp", "mintemp", "precipitation", "ndvi"),
              c("mintemp", "maxtemp", "meantemp", "precipitation"),
              c("precipitation", "ndvi"),
              c("mintemp", "ndvi"),
              c("mintemp"),
              c("maxtemp"),
              c("meantemp"),
              c("precipitation"),
              c("ndvi"),
              c(NULL))
  mods1 <- covariate_models()
  expect_error(covariate_models("ok"))
  expect_equal(length(mods1), 11)
  expect_equal(mods1, ref)
})