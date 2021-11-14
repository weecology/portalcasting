#' @title Create and Fill a Forecasting Directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}} to create a ready-to-run (via \code{\link{portalcast}}) directory where indicated. \cr \cr
#'              \code{setup_production} creates a standard production directory. \cr \cr
#'              \code{setup_sandbox} creates a sandboxing directory. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{list} of datasets to be created using \code{\link{do.call}} on the defined functions. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @return The \code{list} of directory settings \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
setup_dir <- function (main     = ".",
                       models   = prefab_models(), 
                       datasets = prefab_rodent_datasets(),
                       settings = directory_settings(), 
                       quiet    = FALSE, 
                       verbose  = FALSE){

  version_message(quiet = quiet)

  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet)

  config <- write_directory_config(main     = main, 
                                   settings = settings, 
                                   quiet    = quiet)

  fill_dir(main     = main,
           models   = models,
           datasets = datasets,
           settings = settings,
           quiet    = quiet,
           verbose  = verbose)

  setup_completion_message(quiet = quiet)

  invisible(config)

}



#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function (main     = ".",
                              models   = prefab_models(), 
                              datasets = prefab_rodent_datasets(),
                              settings = directory_settings(portalPredictions = list(source = "github", version = "latest")), 
                              quiet    = FALSE, 
                              verbose  = FALSE){

  setup_dir(main     = main,
            models   = models,
            datasets = datasets,
            settings = settings,
            quiet    = quiet,
            verbose  = verbose)

}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function (main     = ".",
                           settings = directory_settings(), 
                           models   = prefab_models(), 
                           datasets = prefab_rodent_datasets(),
                           quiet    = FALSE, 
                           verbose  = FALSE){

  setup_dir(main     = main,
            models   = models,
            datasets = datasets,
            settings = settings,
            quiet    = quiet,
            verbose  = verbose)

  sandbox_welcome(main  = main,
                  quiet = quiet)

}

