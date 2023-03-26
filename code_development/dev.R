rm(list=ls())
devtools::load_all()

main <- "~/pc"


settings             = directory_settings( )
quiet                = FALSE
verbose              = TRUE
model <- "jags_RW"

dataset  = "controls"
species <- "DM"


  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

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

  fit_args  <- named_null_list(element_names = names(model_controls$fit$args))
  for (i in 1:length(fit_args)) {
    fit_args[[i]] <- eval(parse(text = model_controls$fit$args[i]))
  }

  model_fit  <- do.call(what = model_controls$fit$fun,
                        args = fit_args)

  cast_args  <- named_null_list(element_names = names(model_controls$cast$args))
  for (i in 1:length(cast_args)) {
    cast_args[[i]] <- eval(parse(text = model_controls$cast$args[i]))
  }

  model_cast <- do.call(what = model_controls$cast$fun,
                        args = cast_args)

out <-  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 


#  model_cast          <- read_model_cast(main         = main,
#                                         cast_id      = cast_id,
#                                         settings     = settings)
#  casts_metadata      <- read_casts_metadata(main     = main,
#                                             settings = settings)
#  cast_model          <- casts_metadata$model[casts_metadata$cast_id == cast_id]
#  cast_tab            <- read_cast_tab(main           = main, 
#                                       settings       = settings,
#                                       cast_id        = cast_id)
cast_model <- model
cast_tab   <- out$cast_tab
#
  cast_model_controls <- model_controls(main          = main,
                                        models        = cast_model,
                                        settings      = settings)[[cast_model]]
  cast_model_response <- cast_model_controls$response

#  cast_tab <- add_obs_to_cast_tab(main     = main,  
#                                  settings = settings,
#                                  cast_tab = cast_tab)
set.seed(1312)
cast_tab$obs <- rpois(nrow(cast_tab), as.numeric(cast_tab$estimate))
#

  cast_tab <- add_err_to_cast_tab(main     = main,  
                                  settings = settings,
                                  cast_tab = cast_tab)
  cast_tab <- add_covered_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
  measure_cast_level_error(cast_tab = cast_tab)


# working here to expand on the evaluations of forecasts!!!
#  AutoArima, with a normal response



logs(y      = cast_tab$obs,
     family = ifelse(model_controls$response$type == "empirical", "sample", model_controls$response$link),
     mean   = as.numeric(cast_tab$estimate),
     sd     = as.numeric((cast_tab$estimate - cast_tab$lower) / 1.96))

crps(y      = cast_tab$obs,
     family = ifelse(model_controls$response$type == "empirical", "sample", model_controls$response$link),
     mean   = as.numeric(cast_tab$estimate),
     sd     = as.numeric((cast_tab$estimate - cast_tab$lower) / 1.96))






control_runjags <- runjags_controls(nchains = 2, thin = 1, sample = 100, adapt = 100, burnin = 100) 



