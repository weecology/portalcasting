context(desc = "model preparation functions")

main <- "./testing"

test_that(desc = "covariate_models constructs the submodels", {

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
  expect_equal(length(mods1), 11)
  expect_equal(mods1, ref)

})


test_that(desc = "prefab_models creates a vector of model names", {

  pfm <- prefab_models()
  expect_equal(length(pfm), 8)
  expect_is(pfm, "character")

})


test_that(desc = "write_model constructs model file", {

  expect_message(write_model("AutoArima", main = main))

})


test_that(desc = "model_template produces lines for a model script", {

  temp1 <- model_template("AutoArima", main = main)
  expect_is(temp1, "character")
  expect_equal(length(temp1), 8)

  temp2 <- model_template("pevGARCH", main = main)
  expect_is(temp2, "character")
  expect_equal(length(temp2), 8)

})

