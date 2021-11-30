#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including (optionally) 
#'              the raw data (\code{\link[portalr]{download_observations}}), 
#'              portalPredictions archive (\code{\link{download_archive}}), 
#'              climate forecasts (\code{\link{download_climate_forecasts}}), 
#'              models (\code{\link{write_model}}).
#'             
#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
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
#' @param models \code{vector} of models to be written via 
#'        \code{\link{write_model}}.
#'
#' @param data_sets \code{list} of data_sets to be created using 
#'        \code{\link{do.call}} on the defined functions. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
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
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
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
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param lead \code{integer} (or integer \code{numeric}) value for the number of days forward a cast will cover.
#'
#' @param origin \code{Date} forecast origin, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @param t1 \code{Date} for the beginning of the rodent time series to include within the model, typically \code{1995-01-01}, corresponding to \code{newmoonnumber = 217}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param controls_rodents Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details. 
#'
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'
#'   create_dir("./portalcasting")
#'   fill_dir("./portalcasting")
#'
#'   create_dir("./pc")
#'   fill_raw("./pc")
#'   fill_casts("./pc")
#'   fill_fits("./pc")
#'   fill_models("./pc")
#'  }
#'
#' @name fill_directory
#'
NULL

#' @rdname fill_directory
#'
#' @export
#'
fill_dir <- function (main  = ".",
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      models = prefab_models(),
                      data_sets = prefab_data_sets(),
                      quiet = FALSE,
                      verbose = TRUE,
                        controls_model = NULL, 
                        control_files = files_control(), 
                        origin = NULL, t1 = as.Date("1995-01-01"), lead = 365,
                        confidence_level = 0.95, 
                        controls_rodents = rodents_controls(), 
                        arg_checks = TRUE) {

  messageq("Filling directory with requested content",
           quiet = quiet)

  fill_raw(main = main, 
                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_version = climate_forecast_version,
                 climate_forecast_source = climate_forecast_source,
                 quiet = quiet, verbose = verbose)

  fill_casts(main = main, 
             quiet = quiet, 
             verbose = verbose)

  fill_fits(main = main, 
            quiet = quiet, 
            verbose = verbose)

  fill_models(main = main, 
              models = models, 
              controls_model = controls_model, 
              control_files = control_files, 
              quiet = quiet, 
              verbose = verbose, 
              arg_checks = arg_checks)


  fill_data(main = main, 
            models = models,
            origin = origin,
            t1 = t1,
            lead = lead,
            confidence_level = confidence_level,
                      controls_model = controls_model,
                      controls_rodents = controls_rodents, 
                      control_files = control_files, 
                      arg_checks = arg_checks,
            data_sets = data_sets,
            quiet = quiet, 
            verbose = verbose)

  invisible()

}


#' @rdname fill_directory
#'
#' @export
#'
fill_data <- function(main = ".", models = prefab_models(),
                      origin = NULL, t1 = as.Date("1995-01-01"), lead = 365,
                      confidence_level = 0.95,
                      controls_model = NULL,
                      controls_rodents = rodents_controls(), 

                      control_files = files_control(), 
                      data_sets = prefab_data_sets(),
                      quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)

  min_lag <- extract_min_lag(models = models, 
                             controls_model = controls_model, 
                             quiet = quiet, arg_checks = arg_checks)

  messageq(" -Adding data files to data subdirectory", quiet)
  data_m <- prep_moons(main = main, lead = lead, origin = origin, 
                       quiet = quiet, verbose = verbose,
                       control_files = control_files, 
                       arg_checks = arg_checks)
  data_r <- prep_rodents(main = main, moons = data_m, 
                         data_sets = data_sets, origin = origin, 
                         controls_rodents = controls_rodents,
                         quiet = quiet, verbose = verbose, 
                         control_files = control_files, 
                         arg_checks = arg_checks)

#working here

  data_c <- prep_covariates(main = main, moons = data_m, origin = origin, 
                            t1 = t1, lead = lead, 
                            min_lag = min_lag, cast_date = cast_date, 
                            quiet = quiet, control_files = control_files,
                            arg_checks = arg_checks)
  prep_metadata(main = main, models = models,
                data_sets = data_sets, moons = data_m, 
                rodents = data_r, covariates = data_c, origin = origin, 
                lead = lead, min_lag = min_lag, 
t1 = t1, 
                confidence_level = confidence_level, 
                controls_model = controls_model,
                controls_rodents = controls_rodents, quiet = quiet, 
                control_files = control_files, arg_checks = arg_checks)

  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_raw <- function (main  = ".",
                            PortalData_version = "latest",
                            PortalData_source = "gitub",
                            portalPredictions_version = NULL,
                            portalPredictions_source = "github",
                            climate_forecast_version = Sys.Date(),
                            climate_forecast_source = "NMME",
                            quiet = FALSE,
                            verbose = FALSE) {

  messageq("Downloading resources...",
           quiet = quiet)

  download_observations(path = raw_path(main = main), 
                        version = PortalData_version,
                        from_zenodo = PortalData_source == "zenodo",
                        quiet = quiet)

  download_archive(main = main, 
                   version = portalPredictions_version,
                   source = portalPredictions_source,
                   quiet = quiet,
                   verbose = verbose)

  download_climate_forecasts(main = main, 
                             source = climate_forecast_source,  
                             version = climate_forecast_version, 
                             quiet = quiet,
                             verbose = verbose)

  messageq("... downloads complete.",
           quiet = quiet)
  
  invisible()

}

#' @rdname fill_directory
#'
#' @export
#'
fill_casts <- function (main = ".", quiet = FALSE, verbose = FALSE) {

  archive <- normalized_file_path(main, "raw", "portalPredictions", 
                                  mustWork = FALSE)

  casts <- normalized_file_path(main, "raw", "portalPredictions", 
                                "casts",  mustWork = FALSE)
  files <- list.files(casts, full.names = TRUE)

  if (length(files) == 0) {

    casts <- normalized_file_path(main, "raw", "portalPredictions", 
                                  "predictions",  mustWork = FALSE)
    files <- list.files(casts, full.names = TRUE)    

  }  

  if (length(files) == 0) {

    return(invisible())

  }

  dest <- normalized_file_path(main, "casts",  mustWork = FALSE)
 
  fc <- file.copy(from = files, to = dest, recursive = TRUE)

  messageq(paste0(sum(fc), " of ", length(fc), " cast files moved"),
           quiet = quiet)

  messageq(paste(ifelse(sum(fc) > 0, 
                   paste("moved:", basename(files[fc]), collapse = "\n"),
                   ""),
                 ifelse(sum(!fc) > 0, 
                   paste("not moved:", basename(files[!fc]), collapse = "\n"),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)

  invisible()

}


#' @rdname fill_directory
#'
#' @export
#'
fill_fits <- function (main = ".", quiet = FALSE, verbose = FALSE) {

  archive <- normalized_file_path(main, "raw", "portalPredictions", 
                                  mustWork = FALSE)

  fits <- normalized_file_path(main, "raw", "portalPredictions", 
                                "fits",  mustWork = FALSE)
  files <- list.files(fits, full.names = TRUE)
  files[grepl("README", files)] <- NA
  files <- na.omit(files)

  if (length(files) == 0) {

    return(invisible())

  }

  dest <- normalized_file_path(main, "fits",  mustWork = FALSE)
 
  fc <- file.copy(from = files, to = dest, recursive = TRUE)

  messageq(paste0(sum(fc), " of ", length(fc), " fit files moved"),
           quiet = quiet)

  messageq(paste(ifelse(sum(fc) > 0, 
                   paste("moved:", basename(files[fc]), collapse = "\n"),
                   ""),
                 ifelse(sum(!fc) > 0, 
                   paste("not moved:", basename(files[!fc]), collapse = "\n"),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)

  invisible()

}



#' @rdname fill_directory
#'
#' @export
#'
fill_models <- function (main = ".", 
                         models = prefab_models(), 
                        controls_model = NULL, 
                        control_files = files_control(), quiet = FALSE, 
                        verbose = FALSE, arg_checks = TRUE) {


  check_args(arg_checks = arg_checks)
  return_if_null(models)
  controls_model <- model_controls(models = models, 
                                    controls_model = controls_model,
                                    quiet = quiet, arg_checks = arg_checks)
  messageq(" -Writing model scripts", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, verbose = verbose, 
                control_files = control_files, 
                control_model = controls_model[[models[i]]], 
                arg_checks = arg_checks)
  } 
  invisible(NULL)

}


