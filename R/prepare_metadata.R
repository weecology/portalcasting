#' @title Prepare a Model-Running Metadata List
#'
#' @description Sets up the metadata used for casting, in particular the matching of time period across the data sets. This should always be run after \code{\link{prep_moons}}, \code{\link{prep_rodents}}, and \code{\link{prep_covariates}} before any model is run.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{character} vector of name(s) of dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the number of timesteps forward a cast will cover.
#'
#' @param origin \code{Date} forecast origin, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @return \code{list} of casting metadata, which is also saved out as a YAML file (\code{.yaml}) if desired.
#'
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   prepare_metadata()
#'  }
#' 
#' @export
#'
prepare_metadata <- function (main      = ".",
                              models    = prefab_models(), 
                              datasets  = prefab_rodent_datasets(),
                              lead_time = 12,
                              origin    = Sys.Date(), 
                              settings  = directory_settings(), 
                              quiet     = FALSE, 
                              verbose   = FALSE) {


  moons      <- read_moons(main     = main,
                           settings = settings)

  rodents    <- read_rodents(main     = main,
                             datasets = datasets)

  covariates <- read_covariates(main     = main,
                                settings = settings)


  messageq("  - metadata file", quiet = quiet)


  # COMMENT TO ADD TO DOCUMENTATION
  #
  # the origin newmoonnumber is the moon from which the forecast is made
  #   ideally it would be the most recent moon
  #   in reality, however, there are often moons for which there were no censuses, which require starting at an earlier origin than ideal
  #


  ideal_origin_newmoonnumber  <- moons$newmoonnumber[max(which(moons$newmoondate < origin))]
  census_origin_newmoonnumber <- moons$newmoonnumber[max(which(moons$censusdate < origin))]
  rodent_origin_newmoonnumber <- max(unlist(lapply(mapply(FUN = getElement, object = rodents, name = "newmoonnumber"), max)))
  true_origin_newmoonnumber   <- min(c(census_origin_newmoonnumber, rodent_origin_newmoonnumber))

# how have we handled the gap here?? for the covariates i mean?

  covariates_origin_date <- max(covariates$date[covariates$source == "historic"])

  config <- read_directory_config(main     = main, 
                                  settings = settings,
                                  quiet    = quiet)


  out <- list(
           models                  = models, 
           datasets                = datasets, 
           directory_configuration = config)

  write_data(dfl       = out, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$metadata, 
             overwrite = settings$overwrite, 
             quiet     = !verbose)

}