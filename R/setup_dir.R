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
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
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
setup_dir <- function (main  = ".",
                       PortalData_version = "latest",
                       PortalData_source = "gitub",
                       portalPredictions_version = NULL,
                       portalPredictions_source = "github",
                       climate_forecast_version = Sys.Date(),
                       climate_forecast_source = "NMME",
                       models = prefab_models(),
                       quiet = FALSE,
                       verbose = TRUE) {

  create_dir(main  = main, 
             quiet = quiet)

  fill_dir(main  = main,
           PortalData_version = PortalData_version,
           PortalData_source = PortalData_source,
           portalPredictions_version = portalPredictions_version,
           portalPredictions_source = portalPredictions_source,
           climate_forecast_version = climate_forecast_version,
           climate_forecast_source = climate_forecast_source,
           models = models,
           quiet = quiet,
           verbose = verbose)


}




#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(...){

  setup_dir(...)

}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(...){

  setup_dir(...)
  sandbox_welcome(main = main, quiet = quiet)

}




#' @title Sandbox Welcome
#'
#' @description Generates a special welcome message for sandboxes,
#'              displayed upon completion of \code{\link{setup_sandbox}}.
#'
#' @param main \code{character} value of the name of the main component of
#'              the directory tree. 
#'
#' @param quiet \code{logical} indicator if message should be quieted.
#'
#' @return Message is put to the console; \code{NULL} is returned.
#'
#' @examples
#'  sandbox_welcome()
#'
#' @export
#'
sandbox_welcome <-function(main = ".", quiet = FALSE){

  main <- main_path(main = main)
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
  succ <- "sanbox directory successfully set up at \n  "
  happy <- "\nHappy portalcasting!"

  messageq(msg = paste0(castle, succ, main, happy), 
           quiet = quiet)
  invisible()
}

