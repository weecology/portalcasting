context(desc = "tsglm functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "meta_tsglm components work properly individually and together", {

  skip_on_cran()

  abundance      <- prepare_abundance(main     = main2,
                                      dataset  = "controls",
                                      species  = "DM",
                                      model    = "pevGARCH")
  model_controls <- models_controls(main        = main2,
                                    models       = "pevGARCH")[["pevGARCH"]]
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



  expect_message(mod <- meta_tsglm(ts = abundance, model = model, distr = distr, link = link, lag = lag, submodels = submodels, covariates = covariates, metadata = metadata, quiet = FALSE))
  expect_is(mod, "tsglm")

  expect_silent(fc <- forecast(object = mod, h = 13, level = 0.95))
  expect_is(fc, "forecast")

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