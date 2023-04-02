context(desc = "jags functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))

test_that(desc = "runjags_controls makes a list", {

  expect_is(runjags_controls(), "list")

})


test_that(desc = "runjags_model components work properly individually and together", {

  skip_on_cran() 

  abundance      <- prepare_abundance(main     = main2,
                                      dataset  = "controls",
                                      species  = "DM",
                                      model    = "jags_RW")
  model_controls <- model_controls(main        = main2,
                                   model       = "jags_RW")[["jags_RW"]]
  metadata       <- read_metadata(main         = main2)
  newmoons       <- read_newmoons(main         = main2)
  covariates     <- read_covariates(main       = main2)

  settings       <- directory_settings()



  expect_is(mod <- runjags_model(model = file.path(main2, settings$subdirectories$models, model_controls$fit$model_file)), "character")
  expect_equal(substr(mod, 1, 5), "model")

  expect_is(mon <- runjags_monitors(monitors = model_controls$fit$args$monitors, metadata = metadata), "character")
  expect_equal(length(mon), 28)


  expect_is(dat <- runjags_data(data_names = model_controls$fit$args$data_names, metadata = metadata, abundance = abundance, covariates = covariates), "list")
  expect_equal(length(dat), 3)
  expect_equal(names(dat), c("count", "N", "log_mean_count"))

  expect_is(inits <- runjags_inits(inits = model_controls$fit$args$inits), "function")
  dat <- runjags_data(data_names = model_controls$fit$args$data_names, metadata = metadata, abundance = abundance, covariates = covariates)
  expect_is(inits2 <- inits(dat), "function")
  expect_is(inits2(), "list")

  rjc <- runjags_controls(nchains = 2, adapt = 100, burnin = 100, sample = 100, thin = 1)
  run <- fit_runjags(inits = model_controls$fit$args$inits, 
                     data_names = model_controls$fit$args$data_names,
                     model = file.path(main2, settings$subdirectories$models, model_controls$fit$model_file),
                     monitors = model_controls$fit$args$monitors,
                     metadata = metadata, abundance = abundance, covariates = covariates, control_runjags = rjc)
  expect_is(run, "runjags")
 
  expect_silent(fc <- forecast(object = run, h = 13, level = 0.95, nsamples = 1))
  expect_is(fc, "forecast")  

})
