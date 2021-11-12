#' @title Cast Portal rodents models
#'
#' @description Cast the Portal rodent population data using the (updated if 
#'  needed) data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of 
#'  multiple models, with data preparation as needed between runs occurring
#'  via \code{prep_data}. \cr \cr
#'  \code{cast} runs a single cast of multiple models across data sets.
#'
#' @details Multiple models can be run together on the multiple, varying 
#'  data sets for a single new moon using \code{cast}. \code{portalcast}
#'  wraps around \code{cast}, providing updating, verification, and resetting
#'  utilities as well as facilitating multiple runs of \code{cast} across 
#'  new moons (e.g., when using a rolling origin).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param end_moons,end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number(s) of the last sample(s) to be included. Default value is 
#'  \code{NULL}, which equates to the most recently included sample. \cr
#'  \strong{\code{end_moons} allows for multiple moons, \code{end_moon}
#'  can only be one value}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'  summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_rodents Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details.  
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  made or not. For toggling separately from the more general \code{quiet}
#'  argument. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param update_prefab_models \code{logical} indicator if all of the models'
#'  scripts should be updated, even if they do not have an explicit change
#'  to their model options via \code{controls_model}. Default is
#'  \code{FALSE}, which leads to only the models in \code{controls_model}
#'  having their scripts re-written. Switching to \code{TRUE} results in the
#'  models listed in \code{models} having their scripts re-written. \cr \cr
#'  This is particularly helpful when one is changing the global (with respect
#'  to the models) options \code{main}, \code{quiet}, \code{verbose}, or
#'  \code{control_files}.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages). 
#'
#'
#' @param PortalData_source,PortalData_version \code{character} values for the
#'        source and version of the Portal Data to download. 
#'        \cr \cr 
#'        Default values retrieve the latest data from github.
#'        \cr \cr 
#'        See \code{\link[portalr]{download_observations}}.
#'
#' @param portalPredictions_source,portalPredictions_version \code{character} 
#'        values for the source and version of the archive to download.
#'        \cr \cr 
#'        Default values point to github, but \code{version = NULL} indicates
#'        no download.
#'        \cr \cr 
#'        See \code{\link{download_archive}}.
#'
#' @param climate_forecast_source,climate_forecast_version  \code{character} 
#'        values for the source and version of the climate forecasts to
#'        download.
#'        \cr \cr 
#'        Default values retrieve the current day's forecast from the
#'        Northwest Knowledge Network's North American Multi-Model Ensemble 
#'        (NMME) climate forecasts.
#'        \cr \cr 
#'        See \code{\link{download_climate_forecasts}}.
#' 
#' @return Results are saved to files, \code{NULL} is returned.
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
portalcast <- function(main = ".", models = prefab_models(), end_moons = NULL, 
                       start_moon = 217, lead_time = 12, 
                       confidence_level = 0.95, cast_date = Sys.Date(),

                       control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),                       
                       update_prefab_models = FALSE, bline = TRUE, 
                       quiet = FALSE, verbose = FALSE){
  
  return_if_null(models)
  mainp <- main_path(main = main)
  verify(paths = mainp)
  portalcast_welcome(quiet = quiet)
  update_models(main = main, models = models,  
                update_prefab_models = update_prefab_models, quiet = quiet,
                verbose = verbose, control_files = control_files)
  verify_models(main = main, models = models, quiet = quiet)


  last <- last_moon(main = main, date = cast_date)
  end_moons <- ifnull(end_moons, last)
  nend_moons <- length(end_moons)
  for(i in 1:nend_moons){
    cast(main = main, models = models, end_moon = end_moons[i], 
         start_moon = start_moon, lead_time = lead_time, 
         confidence_level = confidence_level, cast_date = cast_date, 
         controls_model = controls_model, controls_rodents = controls_rodents, 
         control_files = control_files, 
                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_version = climate_forecast_version,
                 climate_forecast_source = climate_forecast_source,
 
         quiet = quiet, verbose = verbose)
  }
  if(end_moons[nend_moons] != last){
    data_resetting_message(quiet = quiet)
    fill_data(main = main, models = models, 
              end_moon = NULL, lead_time = lead_time, 
              cast_date = cast_date, start_moon = start_moon, 
              confidence_level = confidence_level, 
              controls_rodents = controls_rodents,
              controls_model = controls_model, 
              control_files = control_files,
              quiet = !verbose, verbose = verbose)
  }
  portalcast_goodbye(bline = bline, quiet = quiet)
} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function(main = ".", models = prefab_models(), end_moon = NULL, 
                 start_moon = 217, lead_time = 12, confidence_level = 0.95, 
                 cast_date = Sys.Date(), controls_model = NULL, 
                 controls_rodents = rodents_controls(),

                 control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),

                 bline = TRUE, quiet = FALSE, verbose = FALSE){

  
  data_readying_message(end_moon = end_moon, quiet = quiet)
  fill_data(main = main, models = models, end_moon = end_moon, 
            lead_time = lead_time, cast_date = cast_date, 
            start_moon = start_moon, confidence_level = confidence_level, 
            control_files = control_files,
            quiet = !verbose, verbose = verbose)

  clear_tmp(main = main, quiet = quiet, cleanup = control_files$cleanup)

  last <- last_moon(main = main, date = cast_date)
  end_moon <- ifnull(end_moon, last)

  models_running_message(end_moon = end_moon, quiet = quiet)
  models_scripts <- models_to_cast(main = main, models = models)
  nmodels <- length(models)
  for(i in 1:nmodels){
    model <- models_scripts[i]
    model_running_message(model = model, quiet = quiet)
    run_status <- tryCatch(
                     source(model),
                     error = function(x){NA}                     
                  )
    model_done_message(model = model, run_status = run_status, quiet = quiet)
  }
  clear_tmp(main = main, quiet = quiet, verbose = verbose, 
            cleanup = control_files$cleanup)
}




#' @title Determine the paths to the scripts for models to cast with
#'
#' @description Translate a \code{character} vector of model name(s) into a 
#'  \code{character} vector of file path(s) corresponding to the model 
#'  scripts. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @return \code{character} vector of the path(s) of the R script file(s) to 
#'   be run.
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
models_to_cast <- function(main = ".", models = prefab_models()){
  
  models_path <- models_path(main = main)
  file_names <- paste0(models, ".R")
  torun <- (list.files(models_path) %in% file_names)
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 
  normalizePath(torun_paths)
}