#' @title Forecast Portal Rodents Models
#'
#' @description Forecast the Portal rodent population data using the (updated if needed) data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of multiple models, with data preparation as needed between runs occurring via \code{prepare_data}. \cr \cr
#'  \code{cast} runs a single cast of multiple models across data sets.
#'
#' @details Multiple models can be run together on the multiple, varying data sets for a single new moon using \code{cast}. \code{portalcast} wraps around \code{cast}, providing updating, verification, and resetting utilities as well as facilitating multiple runs of \code{cast} across new moons (e.g., when using a rolling origin).
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @return Results are saved to files, \code{NULL} is returned \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
portalcast <- function (main       = ".", 
                        settings   = directory_settings(),
                        quiet      = FALSE,
                        verbose    = FALSE){


  messageq(message_break(), "\nPreparing directory for casting\n", 
           message_break(), "\nThis is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", 
           message_break(), quiet = quiet)

  # we're pulling everything from the metadata file, rather than as arguments to the functions

  metadata <- read_metadata(main     = main,
                            settings = settings)

  model_combinations <- make_model_combinations(metadata = metadata)

# does this replace (/  should it be renamed) models_to_cast

  nmodel_combinations <- nrow(model_combinations)


  # use do.call along the table

model <- model_combinations$model[1]
dataset <- model_combinations$dataset[1]
species <- model_combinations$species[1]

  do.call(what = model,
          args = list(main = main,
                      settings = settings,
                      dataset = dataset,
                      species = species,
                      quiet = quiet,
                      verbose = verbose))


  messageq(message_break(), "\nCasting complete\n", message_break(), quiet = quiet)
  invisible() 

# things to consider / incorporate from cast
# tryCatch and error messaging
# what are we using scripts for now

# the cast function should be what's looped over along the do.call
# model is an argument then?
# it pulls all the details needed from metadata


} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function (main       = ".", 
                  settings   = directory_settings(), 
                  quiet      = FALSE, 
                  verbose    = FALSE) {

  moons <- read_newmoons(main     = main,
                      settings = settings)

  which_last_moon <- max(which(moons$newmoondate < cast_date))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moon        <- ifnull(end_moon, last_moon)

  messageq(message_break(), "\nReadying data for forecast origin newmoon ", end_moon, "\n", message_break(), quiet = quiet)

  if (end_moon != last_moon) {

    fill_data(main     = main, 
              datasets = datasets,
              models   = models,
              settings = settings,
              quiet    = quiet, 
              verbose  = verbose)

  }

  messageq(message_break(), "\nRunning models for forecast origin newmoon ", end_moon, "\n", message_break(), quiet = quiet)

  models_scripts <- models_to_cast(main     = main, 
                                   models   = models,
                                   settings = settings)

  nmodels <- length(models)

  for (i in 1:nmodels) {

    model <- models_scripts[i]

    messageq(message_break(), "\n -Running ", path_no_ext(basename(model)), "\n", message_break(), quiet = quiet)

    run_status <- tryCatch(expr  = source(model),
                           error = function(x){NA})

    if (all(is.na(run_status))) {

      messageq("  |----| ", path_no_ext(basename(model)), " failed |----|", quiet = quiet)

    } else {

      messageq("  |++++| ", path_no_ext(basename(model)), " successful |++++|", quiet = quiet)

    }

  }

  invisible()

}

make_model_combinations <- function (metadata = NULL) {

  return_if_null(metadata)

  models <- metadata$models
  nmodels <- length(models)

  controls <- model_controls(main = main)

  out <- data.frame(model   = character(0),
                    dataset = character(0),
                    species = character(0))

  for (i in 1:nmodels) {

    control <- controls[[models[i]]]

    datasets <- control$datasets
    species  <- lapply(datasets, getElement, "species")    
    nspecies <- lapply(species, length) 

    out <- rbind(out, data.frame(model   = models[[i]], 
                                 dataset = rep(names(datasets), nspecies),
                                 species = unlist(species)))

  }

  row.names(out) <- NULL
  out
}


#' @title Determine the Paths to the Scripts for Models to Cast with
#'
#' @description Translate a \code{character} vector of model name(s) into a \code{character} vector of file path(s) corresponding to the model scripts. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{character} vector of the path(s) of the R script file(s) to be run.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   models_to_cast()
#'  }
#'
#' @export
#'
models_to_cast <- function (main     = ".", 
                            models   = prefab_models(),
                            settings = directory_settings()) {
  
  models_path <- file.path(main, settings$subdirectories$models)
  file_names  <- paste0(models, ".R")
  torun       <- list.files(models_path) %in% file_names
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 

  normalizePath(torun_paths)

}