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

test_that(desc = "runjags_inits makes a function", {

  expect_is(inits <- runjags_inits(model_controls), "function")

})

test_that(desc = "runjags_model makes a model character", {

  expect_is(mod <- runjags_model(model_controls), "character")
  expect_equal(substr(mod, 1, 5), "model")

})




