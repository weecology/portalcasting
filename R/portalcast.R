#' @title Cast Portal rodents models
#'
#' @description Cast the Portal rodent population data using the (updated if 
#'  needed) data and models in a portalcasting directory. \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of 
#'  multiple models, with data preparation as needed between runs occurring
#'  via \code{prep_data}. \cr \cr
#'  \code{cast} runs a single cast of multiple models, using the data as
#'  prepared by \code{prep_data}.
#'
#' @details Multiple models can be run together on the same data and 
#'  multiple runs of a model (e.g., with a rolling origin) can be initiated
#'  together using \code{portalcast} and \code{cast} runs the individual
#'  instances.
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
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
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
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_models Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param controls_rodents Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details.  
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
#
#' @param data_sets \code{character} values of the rodent data sets that
#'  are needed, used to enforce certain arguments in data creation (see 
#'  \code{\link{prep_rodents_table}}). The prefab sets currently include 
#'  \code{"all"}, \code{"controls"}, \code{"all_interp"},
#'  and \code{"controls_interp"} (\code{\link{prefab_data_sets}}).
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
portalcast <- function(main = ".", models = prefab_models(),
                       data_sets = prefab_data_sets(), end_moons = NULL, 
                       start_moon = 217, lead_time = 12, 
                       confidence_level = 0.95, cast_date = Sys.Date(),
                       controls_models = NULL, 
                       controls_rodents = rodents_controls(),
                       control_climate_dl = climate_dl_control(),
                       control_files = files_control(),
                       downloads = zenodo_downloads(c("1215988", "833438")), 
                       quiet = FALSE, verbose = FALSE, 
                       arg_checks = TRUE){
  check_args(arg_checks)
  portalcast_welcome(quiet = quiet)
  messageq("Preparing directory for casting", quiet)
  messageq("---------------------------------------------------------", quiet)
  verify_models(main = main, models = models, quiet = quiet, 
                arg_checks = arg_checks)
  raw_data_present <- verify_raw_data(main = main, 
                                      raw_data = control_files$raw_data, 
                                      arg_checks = arg_checks)
  if(!raw_data_present){
    fill_raw(main = main, downloads = downloads, quiet = quiet, 
             control_files = control_files, arg_checks = arg_checks)
  }
  moons <- read_moons(main = main, control_files = control_files,
                      arg_checks = arg_checks)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                            arg_checks = arg_checks)
  end_moons <- ifnull(end_moons, last_moon)
  nend_moons <- length(end_moons)
  for(i in 1:nend_moons){
    msg <- paste0("Readying data for forecast origin newmoon ", end_moons[i])
    messageq(msg, quiet)
    fill_data(main = main, models = models, 
              end_moon = end_moons[i], lead_time = lead_time, 
              cast_date = cast_date, start_moon = start_moon, 
              confidence_level = confidence_level, 
              controls_rodents = controls_rodents,
              controls_models = controls_models, 
              control_climate_dl = control_climate_dl, 
              downloads = downloads, control_files = control_files,
              quiet = !verbose, verbose = verbose, arg_checks = arg_checks)
    cast(main = main, models = models,
         cast_date = cast_date, moons = moons, 
         end_moon = end_moons[i], 
         controls_rodents = controls_rodents, control_files = control_files,
         confidence_level = confidence_level, quiet = quiet, 
         verbose = verbose, arg_checks = TRUE)
  }
  if(end_moons[nend_moons] != last_moon){
    messageq("---------------------------------------------------------", 
             quiet)
    messageq("Resetting data to the most up-to-date versions", quiet)
    fill_data(main = main, models = models, 
              end_moon = NULL, lead_time = lead_time, 
              cast_date = cast_date, start_moon = start_moon, 
              confidence_level = confidence_level, 
              controls_rodents = controls_rodents,
              controls_models = controls_models, 
              control_climate_dl = control_climate_dl, 
              downloads = downloads, control_files = control_files,
              quiet = !verbose, verbose = verbose, arg_checks = arg_checks)

  }
  portalcast_goodbye(quiet = quiet)
} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function(main = ".", models = prefab_models(), moons = NULL, 
                 end_moon = NULL, confidence_level = 0.95, 
                 cast_date = Sys.Date(), 
                 controls_rodents = rodents_controls(), 
                 control_files = files_control(),
                 quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  check_args(arg_checks = arg_checks)
  messageq("---------------------------------------------------------", quiet)
  clear_tmp(main = main, quiet = quiet, cleanup = control_files$cleanup,
            arg_checks = arg_checks)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)

  msg1 <- "---------------------------------------------------------"
  msg2 <- paste0("Running models for forecast origin newmoon ", end_moon)
  messageq(c(msg1, msg2), quiet)

  models_scripts <- models_to_cast(main = main, models = models,
                                   arg_checks = arg_checks)

  nmodels <- length(models)
  for(i in 1:nmodels){
    modelname <- path_no_ext(basename(models_scripts)[i])
    messageq(paste0(" -Running ", modelname), quiet)

    run_status <- tryCatch(
                     source(models_scripts[i], local = TRUE),
                     error = function(x){NA}
                  )
    if(all(is.na(run_status))){
      msg <- paste0("  |----| ", modelname, " failed |----|")
    } else{
      msg <- paste0("  |++++| ", modelname, " successful |++++|")
    }
    messageq(msg, quiet)
  }
  messageq("---------------------------------------------------------", quiet)
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