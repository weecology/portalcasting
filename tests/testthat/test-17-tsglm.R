context(desc = "tsglm functions")

main <- "./testing"



  abundance      <- prepare_abundance(main     = main,
                                      dataset  = "controls",
                                      species  = "DM",
                                      model    = "pevGARCH")
  model_controls <- model_controls(main        = main,
                                   model       = "pevGARCH")[["pevGARCH"]]
  metadata       <- read_metadata(main         = main)
  newmoons       <- read_newmoons(main         = main)
  covariates     <- read_covariates(main       = main)
  model          <- list(past_obs = 1, past_mean = 13)
  distr          <- "poisson"
  link           <- "log"
  lag            <- 6
  submodels      <- list(c("mintemp", "ndvi"),
                         c("maxtemp"),
                         c("meantemp"),
                         c("precipitation"),
                         c(NULL))

test_that(desc = "meta_tsglm runs multiple models, picks best, and forecasts", {

  expect_message(mod <- meta_tsglm(ts = abundance, model = model, distr = distr, link = link, lag = lag, submodels = submodels, covariates = covariates, metadata = metadata, quiet = FALSE))
  expect_is(mod, "tsglm")

  expect_silent(fc <- forecast(object = mod, h = 13, level = 0.95))
  expect_is(fc, "forecast")

})