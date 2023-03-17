#' @title Forecast Portal Rodents Models
#'
#' @description Forecast the Portal rodent population data using the data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of multiple models. \cr \cr
#'  \code{cast} runs a single cast of a single model on one species of one data set.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param models \code{character} vector of name(s) of model(s) to include in the forecast.
#'
#' @param datasets \code{character} vector of datasets to be forecast. 
#'
#' @param species \code{character} vector of species to be forecast. 
#'
#' @return Results are saved to files, \code{NULL} is returned \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
portalcast <- function (main     = ".", 
                        models   = prefab_models( ), 
                        datasets = prefab_datasets( ),
                        species  = base_species( ),
                        settings = directory_settings( ),
                        quiet    = FALSE,
                        verbose  = FALSE) {

  messageq(message_break( ), "\nPreparing directory for casting\n", 
           message_break( ), "\nThis is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", 
           message_break( ), quiet = quiet)

  model_combinations <- make_model_combinations(main     = main,
                                                models   = models,
                                                datasets = datasets,
                                                species  = species,
                                                settings = settings)

  nmodel_combinations <- nrow(model_combinations)

  for (i in 1:nmodel_combinations) {

    cast(main     = main,
         settings = settings,
         model    = model_combinations$model[i],
         dataset  = model_combinations$dataset[i],
         species  = model_combinations$species[i],
         quiet    = quiet,
         verbose  = verbose)

  }

  messageq(message_break( ), "\nCasting complete\n", message_break( ), quiet = quiet)
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

  model_fit  <- do.call(what = model_controls$fit$fun,
                        args = lapply(model_controls$fit$args, eval_parse_text))

  model_cast <- do.call(what = model_controls$cast$fun,
                        args = lapply(model_controls$cast$args, eval_parse_text))

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

#' @title Determine All Requested Combinations of Model, Dataset, and Species to Cast 
#'
#' @description Translate model controls into a \code{data.frame} of model, dataset, and species columns, with a row for each combination. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include in the forecast.
#'
#' @param datasets \code{character} vector of datasets to be forecast. 
#'
#' @param species \code{character} vector of species to be forecast. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{data.frame} of the model combinations.
#'
#' @export
#'
make_model_combinations <- function (main     = ".", 
                                     models   = prefab_models( ), 
                                     datasets = prefab_datasets( ),
                                     species  = base_species( ),
                                     settings = directory_settings( )) {
  
  metadata <- read_metadata(main     = main,
                            settings = settings)


  available_models  <- metadata$models
  navailable_models <- length(available_models)

  controls <- model_controls(main = main)

  out <- data.frame(model   = character(0),
                    dataset = character(0),
                    species = character(0))

  for (i in 1:navailable_models) {

    control <- controls[[available_models[i]]]

    model_datasets   <- control$datasets
    dataset_species  <- lapply(model_datasets, getElement, "species")    
    ndataset_species <- lapply(dataset_species, length) 

    out <- rbind(out, data.frame(model   = available_models[[i]], 
                                 dataset = rep(names(model_datasets), ndataset_species),
                                 species = unlist(dataset_species)))

  }

  row.names(out) <- NULL

  all_in      <- out$model %in% models &
                 out$dataset %in% datasets &
                 out$species %in% species
  out[all_in, ]

}
