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
#' @param lead \code{integer} (or integer \code{numeric}) value for the number of days forward a cast will cover.
#'
#' @param origin \code{Date} forecast origin, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @param t1 \code{Date} for the beginning of the rodent time series to include within the model, typically \code{1995-01-01}, corresponding to \code{newmoonnumber = 217}.
#'
#' @return \code{list} of casting metadata, which is also saved out as a YAML file (\code{.yaml}) if desired.
#' 
#' @export
#'
prep_metadata <- function (main     = ".",
                           models   = prefab_models(), 
                           datasets = prefab_rodent_datasets(),
                           lead     = 365,
                           origin   = Sys.Date(), 
                           t1       = as.Date("1995-01-01"),
                           settings = directory_settings(), 
                           quiet    = FALSE, 
                           verbose  = FALSE) {


  moons      <- read_moons(main = main, settings = settings)
  rodents    <- read_rodents(main = main, datasets = datasets, settings = settings)
  covariates <- read_covariates(main = main, settings = settings)



  messageq("  - metadata file", quiet = quiet)



  covariates_origin_date <- max(covariates$date[covariates$source == "historic"])

  config <- read_directory_config(main     = main, 
                                  settings = settings,
                                  quiet    = quiet)


  out <- list(
           models                  = models, 
           datasets                = datasets, 
           lead                    = lead,
           origin                  = as.character(origin), 
           t1                      = as.character(t1),
           directory_configuration = config)

  write_data(dfl       = out, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$metadata, 
             overwrite = settings$overwrite, 
             quiet     = !verbose)

}