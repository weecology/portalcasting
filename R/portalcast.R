#' @title Cast Portal Rodents Models
#'
#' @description Cast the Portal rodent population data using the (updated if needed) data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of multiple models, with data preparation as needed between runs occurring via \code{prep_data}. \cr \cr
#'  \code{cast} runs a single cast of multiple models across data sets.
#'
#' @details Multiple models can be run together on the multiple, varying data sets for a single new moon using \code{cast}. \code{portalcast} wraps around \code{cast}, providing updating, verification, and resetting utilities as well as facilitating multiple runs of \code{cast} across new moons (e.g., when using a rolling origin).
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param end_moons,end_moon \code{integer} (or integer \code{numeric}) newmoon number(s) of the last sample(s) to be included. Default value is \code{NULL}, which equates to the most recently included sample. \cr
#'  \strong{\code{end_moons} allows for multiple moons, \code{end_moon} can only be one value}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of the cast). In the recurring forecasting, is set to today's date using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number of the first sample to be included. Default value is \code{217}, corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param controls_model Additional controls for models not in the prefab set or for overriding controls of a prefab model (for example when applying a model to a new data set). \cr 
#'  A \code{list} of a single model's script-writing controls or a \code{list} of \code{list}s, each of which is a single model's script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{data_sets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  }
#'  If only a single model's controls are included, the name of the model from the element \code{name} will be used to name the model's \code{list} in the larger \code{list}. If multiple models are added, each element \code{list} must be named according to the model and the\code{name} element. 
#'
#' @param datasets \code{character} vector of dataset names to be created. 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @return Results are saved to files, \code{NULL} is returned \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast()
#'   cast()
#'  }
#'
#' @export
#'
portalcast <- function (main             = ".", 
                        models           = prefab_models(), 
                        datasets         = prefab_rodent_datasets(),
                        end_moons        = NULL, 
                        start_moon       = 217, 
                        lead_time        = 12, 
                        confidence_level = 0.95, 
                        cast_date        = Sys.Date(),
                        controls_model   = NULL, 
                        settings         = directory_settings(),
                        quiet            = FALSE,
                        verbose          = FALSE){

  return_if_null(models)

  messageq(message_break(), "\nPreparing directory for casting\n", message_break(), "\nThis is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", message_break(), quiet = quiet)

  moons <- read_moons(main     = main,
                      settings = settings)

  which_last_moon <- max(which(moons$newmoondate < cast_date))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moons       <- ifnull(end_moons, last_moon)
  nend_moons      <- length(end_moons)

  for (i in 1:nend_moons) {

    cast(main             = main, 
         datasets         = datasets,
         models           = models, 
         end_moon         = end_moons[i], 
         start_moon       = start_moon, 
         lead_time        = lead_time, 
         confidence_level = confidence_level, 
         cast_date        = cast_date, 
         controls_model   = controls_model, 
         settings         = settings,
         quiet            = quiet, 
         verbose          = verbose)

  }

  if (end_moons[nend_moons] != last_moon) {

    messageq(message_break(), "\nResetting data to most up-to-date versions\n", message_break(), quiet = quiet)

    fill_data(main     = main, 
              datasets = datasets,
              models   = models,
              settings = settings,
              quiet    = quiet, 
              verbose  = verbose)

  }

  messageq(message_break(), "\nCasting complete\n", message_break(), quiet = quiet)

} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function (main             = ".", 
                  models           = prefab_models(), 
                  datasets         = prefab_rodent_datasets(),
                  end_moon         = NULL, 
                  start_moon       = 217, 
                  lead_time        = 12, 
                  confidence_level = 0.95, 
                  cast_date        = Sys.Date(), 
                  controls_model   = NULL, 
                  settings         = directory_settings(), 
                  quiet            = FALSE, 
                  verbose          = FALSE) {

  moons <- read_moons(main     = main,
                      settings = settings)

  which_last_moon <- max(which(moons$newmoondate < cast_date))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moon        <- ifnull(end_moon, last_moon)

  messageq(message_break(), "\nReadying data for forecast origin newmoon ", end_moon, "\n", message_break(), quiet = quiet)

  fill_data(main     = main, 
            datasets = datasets,
            models   = models,
            settings = settings,
            quiet    = quiet, 
            verbose  = verbose)

  clear_tmp(main     = main, 
            settings = settings, 
            quiet    = quiet, 
            verbose  = verbose)

  messageq(message_break(), "\nRunning models for forecast origin newmoon ", end_moon, "\n", message_break(), quiet = quiet)

  models_scripts <- models_to_cast(main     = main, 
                                   models   = models,
                                   settings = settings)

  nmodels <- length(models)

  for (i in 1:nmodels) {

    model <- models_scripts[i]

    messageq(message_break(), "\n -Running ", path_no_ext(basename(model)), "\n", message_break(), quiet = quiet)

    run_status <- tryCatch(source(model),
                           error = function(x){NA})
    if (all(is.na(run_status))) {

      messageq("  |----| ", path_no_ext(basename(model)), " failed |----|", quiet = quiet)

    } else {

      messageq("  |++++| ", path_no_ext(basename(model)), " successful |++++|", quiet = quiet)

    }

  }

  clear_tmp(main     = main, 
            settings = settings, 
            quiet    = quiet, 
            verbose  = verbose)

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
  
  models_path <- file.path(main, settings$subs$`model scripts`)
  file_names  <- paste0(models, ".R")
  torun       <- list.files(models_path) %in% file_names
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 

  normalizePath(torun_paths)

}