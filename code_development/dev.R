rm(list=ls())
devtools::load_all()

main <- "~/ptc"
setup_production(main)

main <- "~/sandbox2"
setup_sandbox(main)


main <- "~/covs"
create_dir(main)
fill_resources(main)


# currently working here on the model script files for the jags models


runjags_inits


rm(list=ls())
devtools::load_all()

main <- "~/ptc"
fill_models(main,verbose=F)

dataset  = "controls"
species <- "DM"

                  model    = "jags_logistic_covariates"
                  settings = directory_settings( ) 
                  quiet    = FALSE 
                  verbose  = TRUE



  abundance      <- prepare_abundance(main     = main,
                                      dataset  = dataset,
                                      species  = species,
                                      model    = model,
                                      settings = settings,
                                      quiet    = quiet,
                                      verbose  = verbose)
  model_controls <- model_controls(main        = main,
                                   model       = model,
                                   settings    = settings)[[model]]
  metadata       <- read_metadata(main         = main,
                                  settings     = settings)
  newmoons       <- read_newmoons(main         = main,
                                  settings     = settings)                                        
  covariates     <- read_covariates(main       = main,
                                    settings   = settings)
control_runjags <- runjags_control(thin = 1, adapt = 1000, burnin = 1000, sample = 1000)

  data       <- runjags_data(model_controls = model_controls,
                             abundance      = abundance,
                             metadata       = metadata,
                             covariates     = covariates)  

  monitors       <- runjags_monitors(model_controls = model_controls,
                             metadata       = metadata)
monitors
  


  model_fit  <- do.call(what = model_controls$fit$fun,
                        args = lapply(model_controls$fit$args, eval_parse_text))

  model_cast <- do.call(what = model_controls$cast$fun,
                        args = lapply(model_controls$cast$args, eval_parse_text))

  pmo <- process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 



