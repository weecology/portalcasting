context(desc = "jags functions")

main <- "./testing"

test_that(desc = "runjags_controls makes a list", {

  expect_is(runjags_controls(), "list")

})

  abundance      <- prepare_abundance(main     = main,
                                      dataset  = "controls",
                                      species  = "DM",
                                      model    = "jags_RW")
  model_controls <- model_controls(main        = main,
                                   model       = "jags_RW")[["jags_RW"]]
  metadata       <- read_metadata(main         = main)
  newmoons       <- read_newmoons(main         = main)
  covariates     <- read_covariates(main       = main)


test_that(desc = "runjags_model makes a model character", {

  expect_is(mod <- runjags_model(model_controls = model_controls), "character")
  expect_equal(substr(mod, 1, 5), "model")

})

test_that(desc = "runjags_monitors makes a character vector", {

  expect_is(mon <- runjags_monitors(model_controls = model_controls, metadata = metadata), "character")
  expect_equal(length(mon), 15)

})


test_that(desc = "runjags_data makes a data list", {

  expect_is(dat <- runjags_data(model_controls = model_controls, metadata = metadata, abundance = abundance, covariates = covariates), "list")
  expect_equal(length(dat), 3)
  expect_equal(names(dat), c("count", "N", "log_mean_count"))

})

test_that(desc = "runjags_inits makes a function that operates on data to make a function that draws values", {

  expect_is(inits <- runjags_inits(model_controls = model_controls), "function")
  dat <- runjags_data(model_controls = model_controls, metadata = metadata, abundance = abundance, covariates = covariates)
  expect_is(inits2 <- inits(dat), "function")
  expect_is(inits2(), "list")

 
})


test_that(desc = "fit_runjags wraps up all the functions and can be forecast from", {

  rjc <- runjags_controls(nchains = 2, adapt = 100, burnin = 100, sample = 100, thin = 1)
  expect_silent(run <- fit_runjags(model_controls = model_controls, metadata = metadata, abundance = abundance, covariates = covariates, control_runjags = rjc))
  expect_is(run, "runjags")
 
  expect_silent(fc <- forecast(object = run, h = 13, level = 0.95))
  expect_is(fc, "forecast")  

})
