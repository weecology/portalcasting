#' @title Forecast Portal Rodents Models
#'
#' @description Forecast Portal rodent populations using the data and models in a portalcasting directory. \cr \cr
#'  `portalcast` wraps around `cast` to allow multiple runs of model - dataset - species combinations. It returns and saves out the model combinations table with fit success added as a column.\cr \cr
#'  `cast` runs a single cast of a single model on one species of one dataset. \cr \cr
#'  `make_model_combinations` translates model controls into a `data.frame` of model, dataset, and species columns, with a row for each combination. 
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param models,model `character` vector of name(s) of model(s) to include in the forecast. In `cast`, `model` can only be length-one.
#'
#' @param datasets,dataset `character` vector of datasets to be forecast. In `cast`, `dataset` can only be length-one. 
#'
#' @param species `character` vector of species to be forecast. In `cast`, `species` can only be length-one. See [`rodent_species`][portalr::rodent_species].
#'
#' @return 
#'   `portalcast`: `data.frame` of model combinations with a `logic` column added for fit success, [`invisible`][base::invisible]-ly. \cr  \cr
#'   `cast`: `list` of model outputs from [`process_model_output`]. \cr \cr
#'   `make_model_combinations`: `data.frame` of the model combinations.
#'
#' @export
#'
portalcast <- function (main     = ".", 
                        models   = prefab_models( ), 
                        datasets = prefab_datasets( ),
                        species  = forecasting_species(total = TRUE)) {

  settings <- read_directory_settings(main = main)

  messageq(break_line( ), "Forecasting models...\n", 
           break_line( ), "This is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", 
           break_line( ), quiet = settings$quiet)

  model_combinations <- make_model_combinations(main     = main,
                                                models   = models,
                                                datasets = datasets,
                                                species  = species)

  nmodel_combinations <- nrow(model_combinations)
  out <- named_null_list(element_names = apply(model_combinations, 1, paste0, collapse = "_"))

  for (i in 1:nmodel_combinations) {

    out[[i]] <- tryCatch(expr = cast(main    = main,
                                     model   = model_combinations$model[i],
                                     dataset = model_combinations$dataset[i],
                                     species = model_combinations$species[i]),
                         error = function(x) {NA})
 
    if (all(is.na(out[[i]]))) { 

      messageq("    |------| failed |------|", quiet = settings$quiet) 

    } else {

      messageq("    |++++| successful |++++|", quiet = settings$quiet)    

    }
  }

  model_combinations$fit_successful <- NA
  for (i in 1:nmodel_combinations) {
    model_combinations$fit_successful[i] <- !(all(is.na(out[[i]])))
  }
  write.csv(x         = model_combinations, 
            file      = file.path(main, settings$subdirectories$forecasts, settings$files$forecast_results),
            row.names = FALSE)

  messageq(break_line( ), "...forecasting complete.\n", break_line( ), quiet = settings$quiet)
  invisible(model_combinations) 

} 

#' @rdname portalcast
#'
#' @export
#'
cast <- function (main     = ".", 
                  dataset  = NULL,
                  species  = NULL,
                  model    = NULL) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  return_if_null(x = model)

  settings <- read_directory_settings(main = main)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = settings$quiet)

  abundance      <- prepare_abundance(main    = main,
                                      dataset = dataset,
                                      species = species,
                                      model   = model)
  model_controls <- model_controls(main       = main,
                                   models     = model)[[model]]
  metadata       <- read_metadata(main        = main)
  newmoons       <- read_newmoons(main        = main)                                        
  covariates     <- read_covariates(main      = main)

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
                       species    = species) 

}


#' @rdname portalcast
#'
#' @export
#'
make_model_combinations <- function (main     = ".", 
                                     models   = prefab_models( ), 
                                     datasets = prefab_datasets( ),
                                     species  = forecasting_species(total = TRUE)) {

  available_controls  <- read_model_controls(main = main)
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
