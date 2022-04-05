#' @title Create and Fill a Forecasting Directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}} to create a ready-to-run (via \code{\link{portalcast}}) directory where indicated. \cr \cr
#'              \code{setup_production} creates a standard production directory, which includes downloading the most recent archive of the directory. \cr \cr
#'              \code{setup_sandbox} creates a sandboxing directory. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param rodent_datasets \code{character} vector of name(s) of rodent dataset(s) to be created. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @param new_rodent_dataset_controls \code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets}.
#'
#' @param new_model_controls \code{list} of controls for any new models (not in the prefab models) listed in \code{models}.
#'
#' @return The \code{list} of directory settings \code{\link[base]{invisible}}-ly.
#'
#' @examples
#' \donttest{
#'  setup_dir()
#'  setup_sandbox()
#'  setup_production()
#' }
#'
#' @name directory setup
#'
#'
#' @export
#'
setup_dir <- function (main            = ".",
                       models          = prefab_models(), 
                       rodent_datasets = prefab_rodent_datasets(),
                       settings        = directory_settings(), 
                       quiet           = FALSE, 
                       verbose         = FALSE) {

  messageq(message_break(), "\nThis is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", message_break(), quiet = quiet)

  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet)

  fill_dir(main            = main,
           models          = models,
           rodent_datasets = rodent_datasets,
           settings        = settings,
           quiet           = quiet,
           verbose         = verbose)

  messageq(message_break(), "\nDirectory successfully instantiated\n", message_break(), quiet = quiet)

  read_directory_config(main     = main,
                        settings = settings,
                        quiet    = quiet)

}



#' @rdname directory-setup
#'
#' @export
#'
setup_production <- function (main            = ".",
                              models          = prefab_models(), 
                              rodent_datasets = prefab_rodent_datasets(),
                              settings        = directory_settings(portalPredictions = list(source = "github", version = "latest")), 
                              quiet           = FALSE, 
                              verbose         = FALSE) {


  setup_dir(main            = main,
            models          = models,
            rodent_datasets = rodent_datasets,
            settings        = settings,
            quiet           = quiet,
            verbose         = verbose)

}


#' @rdname directory-setup
#'
#' @export
#'
setup_sandbox <- function (main            = ".",
                           models          = prefab_models(), 
                           rodent_datasets = prefab_rodent_datasets(),
                           settings        = directory_settings(), 
                           quiet           = FALSE, 
                           verbose         = FALSE) {


  setup_dir(main            = main,
            models          = models,
            rodent_datasets = rodent_datasets,
            settings        = settings,
            quiet           = quiet,
            verbose         = verbose)

  castle <- "
                                            
             /\\                         
            /  \\                          ____
           /|  |\\                        / -- )   
          /_|__|_\\                      (_--_/ 
          |      |                       / /              
 __    __ |      | __    __             / / 
[  ]__[  ].      .[  ]__[  ]           / /  
|__         ____         __|      ____/ /__ 
   |      .|    |.      |        / .-----  )
   |      |      |      |       / /     / / 
   |      |      |      |      / /     / /  
~~~~~~~~~~~~~~~~~~~~~~~~~~------------~~~~~~~~~~~~~~
"
  messageq(castle, "Sandbox directory successfully set up at \n\n  ", normalizePath(file.path(main = main)), "\n\nHappy portalcasting!", quiet = quiet)

}