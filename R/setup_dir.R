#' @title Create and fill a forecasting directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}}
#'              to create a ready-to-run (via \code{\link{portalcast}})
#'              directory where indicated. 
#'              \cr \cr
#'              \code{setup_production} creates a standard production 
#'              directory for use in the automated pipeline. 
#'              \cr \cr
#'              \code{setup_sandbox} creates a sandboxing directory for 
#'              exploration, tinkering, etc. with the code outside of 
#'              productions. 
#'              \cr \cr
#'
#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'
#'   setup_dir("./portalcasting")
#'   setup_sandbox("./sandbox")
#'   setup_production("./production")
#'
#'  }
#'
#' @export
#'
setup_dir <- function (main = ".",
                       PortalData_version = "latest",
                       PortalData_source = "github",
                       portalPredictions_version = NULL,
                       portalPredictions_source = "github",
                       quiet = FALSE) {


  create_dir(main  = main, 
             quiet = quiet)

  fill_dir(main  = main,
           PortalData_version = PortalData_version,
           PortalData_source = PortalData_source,
           portalPredictions_version = portalPredictions_version,
           portalPredictions_source = portalPredictions_source,
           quiet = quiet)


}


#setup_dir <- function (main = ".",
  #                     models = prefab_models(), 
  #                     end_moon = NULL, 
  #                     start_moon = 217,
  #                     lead_time = 12, 
  #                     confidence_level = 0.95, 
  #                     cast_date = Sys.Date(),
  #                     controls_model = NULL, 
  #                     controls_rodents = rodents_controls(),
 #                      control_climate_dl = climate_dl_control(),
 #                      control_files = files_control(),
 #                      downloads = zenodo_downloads(c("1215988", "833438")), 
 #                      quiet = FALSE, 
 #                      verbose = FALSE) {


#  create_dir(main  = main, 
#             quiet = quiet)
#
#  fill_dir(main = main, models = models, 
#           end_moon = end_moon, lead_time = lead_time, 
#           cast_date = cast_date, start_moon = start_moon, 
#           confidence_level = confidence_level, 
#           controls_model = controls_model, 
#           controls_rodents = controls_rodents, 
#           control_climate_dl = control_climate_dl, 
#           downloads = downloads, quiet = quiet, verbose = verbose, 
#           control_files = control_files, arg_checks = arg_checks)


#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(main = ".", models = prefab_models(), 
                             end_moon = NULL, start_moon = 217, 
                             lead_time = 12, confidence_level = 0.95, 
                             cast_date = Sys.Date(), controls_model = NULL,
                             controls_rodents = rodents_controls(),
                             control_climate_dl = climate_dl_control(),
                             control_files = files_control(), 
                             downloads = 
                               zenodo_downloads(c("1215988", "833438")), 
                             quiet = FALSE, verbose = TRUE, 
                             arg_checks = TRUE){
  check_args(arg_checks)
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
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
                          controls_model = NULL,
                          controls_rodents = rodents_controls(),
                          control_climate_dl = climate_dl_control(),
                          control_files = files_control(),
                          downloads = 
                            zenodo_downloads(c("1215988", "833438")), 
                          quiet = FALSE, verbose = TRUE, 
                          arg_checks = FALSE){
  check_args(arg_checks)
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
            controls_rodents = controls_rodents, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, quiet = quiet, verbose = verbose, 
            control_files = control_files, arg_checks = arg_checks)
  sandbox_welcome(main = main, quiet = quiet, arg_checks = arg_checks)
}

