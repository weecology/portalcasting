context("Test prepare_models functions")

main <- "./testing"

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

test_that("model_controls", {
  expect_is(model_controls(prefab_models()), "list")
  expect_equal(length(model_controls(prefab_models())), 8)
  expect_is(model_controls(prefab_models(), 
                                     list(name = "xx", 
                                          covariates = FALSE, lag = NA)), 
            "list")

  expect_error(model_controls(prefab_models(), 
                                     list(name = "AutoArima", 
                                          covariates = FALSE, lag = NA)))
  mm <- list(ESSS = model_control("ESSS"), xx = model_control("xx"))
  expect_error(model_controls(c("xx", "ESSS"), controls_model = mm))
  expect_is(model_controls(c("xx", "ESSS"), controls_model = mm,
                           arg_checks = FALSE),
           "list")

  expect_message(model_controls("xx"))
})

test_that("prefab_models", {
  expect_equal(length(prefab_models()), 8)
})


test_that("write_model", {
  expect_message(write_model("AutoArima", main = main))
  expect_message(write_model("AutoArima", main = main, 
                             covariatesTF = NULL, lag = NULL))
  expect_message(write_model("AutoArima", main = main, 
                             covariatesTF = TRUE, lag = NULL))
  expect_message(write_model("AutoArima", main = main, 
                             covariatesTF = NULL, lag = NA))
  expect_message(write_model("AutoArimaX", main = main,
                             covariatesTF = NULL, lag = 1))

})

test_that("update_models", {
   cm <- model_control(name = "AutoArima", data_sets = c("all", "controls"))
   update_models(main = main, controls_model = cm)
})

test_that("model_template", {

  temp1 <- model_template("AutoArima", main = "./testing")
  expect_is(temp1, "character")
  expect_equal(length(temp1), 4)

  temp2 <- model_template("pevGARCH", main = "./testing")
  expect_is(temp2, "character")
  expect_equal(length(temp2), 4)

})

test_that("control_list_arg", {
  expect_is(control_list_arg(runjags_control(nchains = 3), "runjags_control"),
            "character")
  expect_is(control_list_arg(runjags_control(nchains = NULL), 
                             "runjags_control"),
            "character")
})
