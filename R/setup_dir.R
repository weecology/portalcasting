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
                      start_moon = 217, lead_time = 12, 
                      confidence_level = 0.95, cast_date = Sys.Date(),
                      controls_models = NULL, 
                      controls_rodents = rodents_controls(),
                      control_climate_dl = climate_dl_control(),
                      control_files = files_control(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  portalcast_welcome(quiet = quiet)
  create_dir(main = main, filename_config = control_files$filename_config,
             quiet = quiet, verbose = verbose, arg_checks = arg_checks)
  messageq("---------------------------------------------------------", quiet)
  fill_dir(main = main, models = models, 
           end_moon = end_moon, lead_time = lead_time, 
           cast_date = cast_date, start_moon = start_moon, 
           confidence_level = confidence_level, 
           controls_models = controls_models, 
           controls_rodents = controls_rodents, 
           control_climate_dl = control_climate_dl, 
           downloads = downloads, quiet = quiet, verbose = verbose, 
           control_files = control_files, arg_checks = arg_checks)
  messageq("---------------------------------------------------------", quiet)
  messageq("Directory successfully instantiated", quiet)
  messageq("---------------------------------------------------------", quiet)
}



#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(main = ".", models = prefab_models(), 
                             end_moon = NULL, start_moon = 217, 
                             lead_time = 12, cast_date = Sys.Date(),
                             confidence_level = 0.95, controls_models = NULL,
                             controls_rodents = rodents_controls(),
                             control_climate_dl = climate_dl_control(),
                             control_files = files_control(), 
                             downloads = 
                               zenodo_downloads(c("1215988", "833438")), 
                             quiet = FALSE, verbose = FALSE, 
                             arg_checks = TRUE){
  check_args(arg_checks)
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_models = controls_models, 
            controls_rodents = controls_rodents, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, quiet = quiet, verbose = verbose, 
            control_files = control_files, arg_checks = arg_checks)
}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_models = NULL,
                          controls_rodents = rodents_controls(),
                          control_climate_dl = climate_dl_control(),
                          control_files = files_control(),
                          downloads = 
                            zenodo_downloads(c("1215988", "833438")), 
                          quiet = FALSE, verbose = FALSE, 
                          arg_checks = FALSE){
  check_args(arg_checks)
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_models = controls_models, 
            controls_rodents = controls_rodents, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, quiet = quiet, verbose = verbose, 
            control_files = control_files, arg_checks = arg_checks)
  sandbox_welcome(main = main, quiet = quiet, arg_checks = arg_checks)
}

