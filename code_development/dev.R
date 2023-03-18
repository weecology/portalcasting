rm(list=ls())
devtools::load_all()
main <- "~/sandbox2"
setup_sandbox(main)

dataset  = "controls"
species <- "DM"

                  model    = "AutoArima"
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



