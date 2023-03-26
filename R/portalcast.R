#' @title Forecast Portal Rodents Models
#'
#' @description Forecast Portal rodent populations using the data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of model - dataset - species combinations. \cr \cr
#'  \code{cast} runs a single cast of a single model on one species of one dataset. \cr \cr
#'  \code{make_model_combinations} translates model controls into a \code{data.frame} of model, dataset, and species columns, with a row for each combination. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param models,model \code{character} vector of name(s) of model(s) to include in the forecast. In \code{cast}, \code{model} can only be length-one.
#'
#' @param datasets,dataset \code{character} vector of datasets to be forecast. In \code{cast}, \code{dataset} can only be length-one. 
#'
#' @param species \code{character} vector of species to be forecast. In \code{cast}, \code{species} can only be length-one. See \code{\link[portalr]{rodent_species}}.
#'
#' @return \code{portalcast}, \code{cast}: Results are saved to files, \code{NULL} is returned \code{\link[base]{invisible}}-ly. \cr \cr
#'   \code{make_model_combinations}: \code{data.frame} of the model combinations.
#'
#' @export
#'
portalcast <- function (main     = ".", 
                        models   = prefab_models( ), 
                        datasets = prefab_datasets( ),
                        species  = forecasting_species(total = TRUE),
                        settings = directory_settings( ),
                        quiet    = FALSE,
                        verbose  = FALSE) {

  messageq(break_line( ), "Forecasting models...\n", 
           break_line( ), "This is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", 
           break_line( ), quiet = quiet)

  model_combinations <- make_model_combinations(main     = main,
                                                models   = models,
                                                datasets = datasets,
                                                species  = species,
                                                settings = settings)

  nmodel_combinations <- nrow(model_combinations)
  out <- named_null_list(element_names = apply(model_combinations, 1, paste0, collapse = "_"))

  for (i in 1:nmodel_combinations) {

    out[[i]] <- tryCatch(expr = cast(main     = main,
                                     settings = settings,
                                     model    = model_combinations$model[i],
                                     dataset  = model_combinations$dataset[i],
                                     species  = model_combinations$species[i],
                                     quiet    = quiet,
                                     verbose  = verbose),
                         error = function(x) {NA})
  
    if (all(is.na(out[[i]]))) { 

      messageq("    |------| failed |------|", quiet = quiet) 

    } else {

      messageq("    |++++| successful |++++|", quiet = quiet )    

    }
  }

  messageq(break_line( ), "...forecasting complete.\n", break_line( ), quiet = quiet)
  invisible( ) 

} 

#' @rdname portalcast
#'
#' @export
#'
cast <- function (main     = ".", 
                  dataset  = NULL,
                  species  = NULL,
                  model    = NULL,
                  settings = directory_settings( ), 
                  quiet    = FALSE, 
                  verbose  = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  return_if_null(x = model)

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

  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 

}


#' @rdname portalcast
#'
#' @export
#'
make_model_combinations <- function (main     = ".", 
                                     models   = prefab_models( ), 
                                     datasets = prefab_datasets( ),
                                     species  = forecasting_species(total = TRUE),
                                     settings = directory_settings( )) {

  available_controls  <- read_model_controls(main     = main,
                                             settings = settings)
  available_models    <- names(available_controls)
  navailable_models   <- length(available_models)

  out <- data.frame(model   = character(0),
                    dataset = character(0),
                    species = character(0))

  for (i in 1:navailable_models) {

    control <- available_controls[[available_models[i]]]

    model_datasets   <- control$datasets
    dataset_species  <- lapply(model_datasets, getElement, "species")    
    ndataset_species <- lapply(dataset_species, length) 

    out <- rbind(out, data.frame(model   = available_models[[i]], 
                                 dataset = rep(names(model_datasets), ndataset_species),
                                 species = unlist(dataset_species)))

  }

  row.names(out) <- NULL
  all_in         <- out$model %in% models &
                    out$dataset %in% datasets &
                    out$species %in% species
  out[all_in, ]

}
