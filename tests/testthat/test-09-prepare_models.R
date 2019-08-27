context("Test prepare_models functions")

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
  expect_equal(length(mods1), 11)
  expect_equal(mods1, ref)
})

test_that("verify_models", {
  create_dir(main = "./testing")
  fill_models(main = "./testing")
  expect_message(verify_models(main = "./testing"))
  expect_error(verify_models(main = "./testing", models = "1234")) 
  expect_error(verify_models(main = "./123"))
})

test_that("model_script_controls", {
  expect_is(model_script_controls(prefab_models()), "list")
  expect_equal(length(model_script_controls(prefab_models())), 5)
  expect_is(model_script_controls(prefab_models(), 
                                     list(name = "xx", 
                                          covariates = FALSE, lag = NA)), 
            "list")

  expect_error(model_script_controls(prefab_models(), 
                                     list(name = "AutoArima", 
                                          covariates = FALSE, lag = NA)))
  expect_error(model_script_controls("xx"))
})

test_that("prefab_models", {
  expect_equal(length(prefab_models()), 5)
  expect_equal(length(prefab_models("xx")), 6)
})


test_that("write_model", {
  expect_message(write_model("AutoArima", main = "./testing"))
  expect_message(write_model("AutoArima", main = "./testing", 
                             covariatesTF = NULL, lag = NULL))
  expect_message(write_model("AutoArima", main = "./testing", 
                             covariatesTF = TRUE, lag = NULL))
  expect_message(write_model("AutoArima", main = "./testing", 
                             covariatesTF = NULL, lag = NA))
  expect_message(write_model("AutoArimaX", main = "./testing", 
                             covariatesTF = NULL, lag = 1))

})


test_that("model_template", {

  temp1 <- model_template("AutoArima", main = "./testing")
  expect_is(temp1, "character")
  expect_equal(length(temp1), 4)

  temp2 <- model_template("pevGARCH", main = "./testing")
  expect_is(temp2, "character")
  expect_equal(length(temp2), 4)

})
