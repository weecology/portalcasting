context("Test prepare_models functions")


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

test_that("model_names", {
  expect_equal(length(model_names(c("model1", "model2"))),2)
  expect_equal(length(model_names(c("model1", "model2"), "prefab")),7)
  expect_equal(length(prefab_models()), 5)
  expect_equal(length(wEnsemble_models()), 6)
  expect_equal(model_names(), NULL)
})


test_that("write_model", {
  expect_message(write_model("AutoArima", main = "./testing"))
  expect_message(write_model("AutoArima", main = "./testing", 
                             covariates = NULL, lag = NULL))
  expect_message(write_model("AutoArima", main = "./testing", 
                             covariates = TRUE, lag = NULL))
  expect_message(write_model("AutoArima", main = "./testing", 
                             covariates = NULL, lag = NA))
  expect_message(write_model("AutoArimaX", main = "./testing", 
                             covariates = NULL, lag = 1))

})


test_that("model_template", {

  temp1 <- model_template("AutoArima", main = "./testing")
  expect_is(temp1, "character")
  expect_equal(length(temp1), 1)

  temp2 <- model_template("pevGARCH", main = "./testing")
  expect_is(temp2, "character")
  expect_equal(length(temp2), 1)

})
