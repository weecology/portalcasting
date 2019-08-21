#' @title Create and fill a forecasting directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}}
#'  to create a ready-to-run (via \code{\link{portalcast}}) when
#'  indicated. \cr \cr
#'  \code{setup_production} creates a standard production directory. \cr \cr
#'  \code{setup_sandbox} creates a sandboxing directory. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
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
#' @param hist_covariates \code{logical} indicator of whether or not 
#'  historical covariates are to be included.
#'
#' @param cast_covariates \code{logical} indicator whether or not -casted 
#'  covariates are to be included.
#'
#' @param raw_path_archive \code{character} value of the path to the archive
#'  within the raw sub folder.
#' 
#' @param raw_path_data \code{character} value indicating the folder path
#'  to the data within the \code{raw} subdirectory but above the files. A 
#'  standard portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so \code{raw_path_data = "PortalData"} (as
#'  \code{"raw/"} is implied). 
#' 
#' @param raw_path_predictions \code{character} value of the path to the
#'  predictions folder within the raw sub folder (via the archive).  
#' 
#' @param raw_cov_cast_file \code{character} value of the path to the
#'  covariate cast file within \code{raw_path_archive}.
#'
#' @param raw_path_cov_cast \code{character} value of the path to the folder
#'  that holds any downloaded covariate cast files within the raw sub folder.
#'
#' @param raw_moons_file \code{character} value indicating the path
#'  to the moons data file within \code{raw_path_data}. A standard 
#'  portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so 
#'  \code{raw_moons_file = "Rodents/moon_dates.csv"}.
#'
#' @param source_name \code{character} value for the name to give the 
#'   covariate forecast. Currently is \code{"current_archive"}. Previous to
#'   \code{"current_archive"}, the data were retroactively filled in and are 
#'   given the source name \code{"retroactive"}.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'   cast covariates should be appended to the historical casts for the
#'   purposes of hindcasting later.
#'
#' @param controls_m Additional controls for models not in the prefab set. 
#'  \cr 
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
#' @param controls_r Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details. 
#'
#' @param control_cdl \code{list} of specifications for the download, which
#'  are sent to \code{\link{NMME_urls}} to create the specific URLs. See
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
#' @param subs \code{character} vector of the names of the sub components of
#'  the directory tree. Generally shouldn't be changed.
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param filename_moons \code{character} name of the file for saving the 
#'  data output.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
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
#' @return \code{NULL}
#'
#' @examples
#' \donttest{
#'  setup_dir()
#'  setup_sandbox()
#'  setup_production()
#' }
#'
#' @export
#'
setup_dir <- function(main = ".", models = prefab_models(), end_moon = NULL, 
                     lead_time = 12, cast_date = Sys.Date(),
                     start_moon = 217, 
                     confidence_level = 0.9, 
                     hist_covariates = TRUE, cast_covariates = TRUE,
                     raw_path_archive = "portalPredictions",
                     raw_path_data = "PortalData",
                     raw_path_predictions = "portalPredictions/predictions",
                     raw_cov_cast_file = "data/covariate_casts.csv",
                     raw_path_cov_cast = "cov_casts", 
                     raw_moons_file = "Rodents/moon_dates.csv",
                     source_name = "current_archive",
                     append_cast_csv = TRUE, controls_m = NULL,
                     controls_r = rodents_controls(),
                     control_cdl = climate_dl_control(),
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     quiet = FALSE, verbose = FALSE, subs = subdirs(),
                     save = TRUE, overwrite = TRUE, 
                     filename_moons = "moon_dates.csv",
                     filename_cov = "covariates.csv", 
                     filename_meta = "metadata.yaml", cleanup = TRUE,
                     arg_checks = TRUE){
  check_args(arg_checks)
  pass_and_call(portalcast_welcome)
  pass_and_call(create_dir)
  pass_and_call(fill_dir)
}



#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(main = ".", models = prefab_models(), 
                             end_moon = NULL, lead_time = 12, 
                             cast_date = Sys.Date(), 
                             start_moon = 217, confidence_level = 0.9, 
                             hist_covariates = TRUE, cast_covariates = TRUE,
                             raw_path_archive = "portalPredictions",
                             raw_path_data = "PortalData",
                             raw_path_predictions =
                               "portalPredictions/predictions",
                             raw_cov_cast_file = "data/covariate_casts.csv",
                             raw_path_cov_cast = "cov_casts", 
                             raw_moons_file = "Rodents/moon_dates.csv",
                             source_name = "current_archive",
                             append_cast_csv = TRUE, controls_m = NULL,
                             controls_r = rodents_controls(),
                             control_cdl = climate_dl_control(),
                             downloads = 
                               zenodo_downloads(c("1215988", "833438")), 
                             quiet = FALSE, verbose = FALSE, subs = subdirs(),
                             save = TRUE, overwrite = TRUE, 
                             filename_moons = "moon_dates.csv",
                             filename_cov = "covariates.csv", 
                             filename_meta = "metadata.yaml", cleanup = TRUE,
                             arg_checks = TRUE){
  check_args(arg_checks)
  pass_and_call(setup_dir)
}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(main = ".", models = prefab_models(), 
                          end_moon = NULL, lead_time = 12, 
                          cast_date = Sys.Date(), 
                          start_moon = 217, confidence_level = 0.9, 
                          hist_covariates = TRUE, cast_covariates = TRUE,
                          raw_path_archive = "portalPredictions",
                          raw_path_data = "PortalData",
                          raw_path_predictions =
                            "portalPredictions/predictions",
                          raw_cov_cast_file = "data/covariate_casts.csv",
                          raw_path_cov_cast = "cov_casts", 
                          raw_moons_file = "Rodents/moon_dates.csv",
                          source_name = "current_archive",
                          append_cast_csv = TRUE, controls_m = NULL,
                          controls_r = rodents_controls(),
                          control_cdl = climate_dl_control(),
                          downloads = 
                            zenodo_downloads(c("1215988", "833438")), 
                          quiet = FALSE, verbose = FALSE, subs = subdirs(),
                          save = TRUE, overwrite = TRUE, 
                          filename_moons = "moon_dates.csv",
                          filename_cov = "covariates.csv", 
                          filename_meta = "metadata.yaml", cleanup = TRUE,
                          arg_checks = TRUE){
  check_args(arg_checks)
  pass_and_call(setup_dir)
  pass_and_call(sandbox_welcome)
}

#' @title Directory welcome and goodbye messages
#'
#' @description Create a welcome message for the directory based on the 
#'  package version or a goodbye message for when models are done.
#'
#' @param quiet \code{logical} indicator if message should be quieted.
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
#' @return \code{NULL}, as message is printed.
#'
#' @examples
#'  portalcast_welcome()
#'  portalcast_goodbye()
#'
#' @export
#' 
portalcast_welcome <- function(quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  msg1 <- "##########################################################"
  version_number <- packageDescription("portalcasting", fields = "Version")
  msg2 <- paste0("This is portalcasting v", version_number)
  messageq(c(msg1, msg2), quiet)
}

#' @rdname portalcast_welcome
#'
#' @export
#'
portalcast_goodbye <- function(quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  msg1 <- "########################################################"
  messageq(c(msg1, "Models done", msg1), quiet)
}


#' @title Sandbox welcome
#'
#' @description Create a welcome message for the sandbox
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if progress message should be
#'   quieted.
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
#' @return \code{NULL}, as message is printed.
#'
#' @examples
#'  sandbox_welcome()
#'
#' @export
#' 
sandbox_welcome <-function(main = ".", quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)

main <- main_path(main)
castle <- "
                                         ____
             /\\                         / -- )   
            /  \\                       (____/
           /|  |\\                       / /  
          /_|__|_\\                     / / 
          |      |                    / /
 __    __ |      | __    __          / / 
[  ]__[  ].      .[  ]__[  ]        / /  
|__         ____         __|  ____ / /__ 
   |      .|    |.      |    / .------  )
   |      |      |      |   / /      / / 
   |      |      |      |  / /      / /  
~~~~~~~~~~~~~~~~~~~~~~~~~~------------~~~~~~~~~~~~~~
"
succ <- "sanbox directory successfully set up at \n"
happy <- "\nhappy portalcasting!"
msg <- paste0(castle, succ, main, happy)

messageq(msg, quiet)
}