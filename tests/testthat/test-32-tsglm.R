context(desc = "tsglm functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")



  abundance      <- prepare_abundance(main     = main2,
                                      dataset  = "controls",
                                      species  = "DM",
                                      model    = "pevGARCH")
  model_controls <- model_controls(main        = main2,
                                   model       = "pevGARCH")[["pevGARCH"]]
  metadata       <- read_metadata(main         = main2)
  newmoons       <- read_newmoons(main         = main2)
  covariates     <- read_covariates(main       = main2)
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


test_that(desc = "forecast.tsglm allows for new xregs or not", {

  xreg <- covariates[covariates$newmoonnumber %in% (metadata$time$historic_newmoonnumbers - lag), "mintemp"]

  expect_silent(mod <- tsglm(ts = abundance, model = model, distr = distr, link = link, xreg = xreg))
  expect_is(mod, "tsglm")
 
  newxreg <- covariates[covariates$newmoonnumber %in% (metadata$time$forecast_newmoonnumbers - lag), "mintemp"]
  expect_silent(fc <- forecast(object = mod, h = 13, level = 0.95, newxreg = newxreg))
  expect_is(fc, "forecast")


  expect_silent(mod <- tsglm(ts = abundance, model = model, distr = distr, link = link))
  expect_is(mod, "tsglm")
 
  expect_silent(fc <- forecast(object = mod, h = 13, level = 0.95))
  expect_is(fc, "forecast")

})