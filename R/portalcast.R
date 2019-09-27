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
#' @param controls_model Additional controls for models not in the prefab 
#'  set or for overriding controls of a prefab model (for example when 
#'  applying a model to a new data set). \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{data_sets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  }
#'  If only a single model's controls are included, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param controls_rodents Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details.  
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  made or not. For toggling separately from the more general \code{quiet}
#'  argument. 
#'
#' @param control_climate_dl \code{list} of specifications for the download, 
#'  which are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param downloads \code{list} of arguments to pass to \code{\link{download}}
#'  for raw file downloading. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages). 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
                       controls_model = NULL, 
                       controls_rodents = rodents_controls(),
                       control_climate_dl = climate_dl_control(),
                       control_files = files_control(),
                       downloads = zenodo_downloads(c("1215988", "833438")), 
                       bline = TRUE, quiet = FALSE, verbose = FALSE, 
                       arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  portalcast_welcome(quiet = quiet, arg_checks = arg_checks)
  update_models(main = main, models = NULL, controls_model = controls_model, 
                quiet = quiet, verbose = verbose, 
                control_files = control_files, arg_checks = arg_checks)
  verify_models(main = main, models = models, quiet = quiet, 
                arg_checks = arg_checks)
  fill_raw(main = main, downloads = downloads, only_if_missing = TRUE, 
           quiet = quiet, control_files = control_files, 
           arg_checks = arg_checks)
  last <- last_moon(main = main, date = cast_date, arg_checks = arg_checks)
  end_moons <- ifnull(end_moons, last)
  nend_moons <- length(end_moons)
  for(i in 1:nend_moons){
    cast(main = main, models = models, end_moon = end_moons[i], 
         start_moon = start_moon, lead_time = lead_time, 
         confidence_level = confidence_level, cast_date = cast_date, 
         controls_model = controls_model, controls_rodents = controls_rodents, 
         control_climate_dl = control_climate_dl, 
         control_files = control_files, downloads = downloads, 
         quiet = quiet, verbose = verbose, arg_checks = TRUE)
  }
  if(end_moons[nend_moons] != last){
    data_resetting_message(bline = bline, quiet = quiet, 
                           arg_checks = arg_checks)
    fill_data(main = main, models = models, 
              end_moon = NULL, lead_time = lead_time, 
              cast_date = cast_date, start_moon = start_moon, 
              confidence_level = confidence_level, 
              controls_rodents = controls_rodents,
              controls_model = controls_model, 
              control_climate_dl = control_climate_dl, 
              downloads = downloads, control_files = control_files,
              quiet = !verbose, verbose = verbose, arg_checks = arg_checks)
  }
  portalcast_goodbye(bline = bline, quiet = quiet, arg_checks = arg_checks)
} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function(main = ".", models = prefab_models(), end_moon = NULL, 
                 start_moon = 217, lead_time = 12, confidence_level = 0.95, 
                 cast_date = Sys.Date(), controls_model = NULL, 
                 controls_rodents = rodents_controls(),
                 control_climate_dl = climate_dl_control(),
                 control_files = files_control(),
                 downloads = zenodo_downloads(c("1215988", "833438")), 
                 bline = TRUE, quiet = FALSE, verbose = FALSE,
                 arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  data_readying_message(end_moon = end_moon, bline = bline, 
                        quiet = quiet, arg_checks = arg_checks)
  fill_data(main = main, models = models, end_moon = end_moon, 
            lead_time = lead_time, cast_date = cast_date, 
            start_moon = start_moon, confidence_level = confidence_level, 
            controls_rodents = controls_rodents,
            controls_model = controls_model, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, control_files = control_files,
            quiet = !verbose, verbose = verbose, arg_checks = arg_checks)

  clear_tmp(main = main, quiet = quiet, cleanup = control_files$cleanup,
            arg_checks = arg_checks)

  last <- last_moon(main = main, date = cast_date, arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last)

  models_running_message(end_moon = end_moon, bline = bline, quiet = quiet, 
                         arg_checks = arg_checks)
  models_scripts <- models_to_cast(main = main, models = models,
                                   arg_checks = arg_checks)
  nmodels <- length(models)
  for(i in 1:nmodels){
    model <- models_scripts[i]
    model_running_message(model = model, quiet = quiet,
                          arg_checks = arg_checks)
    run_status <- tryCatch(
                     source(model, local = TRUE),
                     error = function(x){NA}                     
                  )
    model_done_message(model = model, run_status = run_status, quiet = quiet,
                       arg_checks = arg_checks)
  }
  clear_tmp(main = main, quiet = quiet, verbose = verbose, 
            cleanup = control_files$cleanup, arg_checks = arg_checks)
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
models_to_cast <- function(main = ".", models = prefab_models(), 
                           arg_checks = TRUE){
  check_args(arg_checks)
  models_path <- models_path(main = main, arg_checks = arg_checks)
  file_names <- paste0(models, ".R")
  torun <- (list.files(models_path) %in% file_names)
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 
  normalizePath(torun_paths)
}