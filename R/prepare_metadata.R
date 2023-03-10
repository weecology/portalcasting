#' @title Prepare a Model-Running Metadata List
#'
#' @description Sets up the metadata used for forecasting, in particular the matching of time period across the datasets. This should always be run after \code{\link{prepare_newmoons}}, \code{\link{prepare_rodents}}, and \code{\link{prepare_covariates}} but before any model is run.
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
#' @param cast_date \code{Date} from which future is defined (the origin of the cast). In the recurring forecasting, is set to today's date using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number of the first sample to be included. Default value is \code{217}, corresponding to \code{1995-01-01}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number of the last sample to be included. Default value is \code{NULL}, which equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the number of timesteps forward a cast will cover. \cr 
#'   As of version 0.51.0, default is now 13, was previously 12. We are now using 13 to align with the timestep being a lunar month, and 13 lunar months covers a full calendar year. 
#'
#' @param confidence_level \code{numeric} confidence level used in summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @return \code{list} of casting metadata, which is also saved out as a YAML file (\code{.yaml}) if desired.
#' 
#' @export
#'
prepare_metadata <- function (main             = ".",
                              models           = prefab_models(), 
                              datasets         = prefab_datasets(),
                              end_moon         = NULL, 
                              start_moon       = 217, 
                              lead_time        = 13,
                              cast_date        = Sys.Date(), 
                              confidence_level = 0.95,
                              settings         = directory_settings(), 
                              quiet            = FALSE, 
                              verbose          = FALSE) {


  moons      <- read_newmoons(main     = main, 
                           settings = settings)
  rodents    <- read_rodents(main     = main, 
                             datasets = datasets, 
                             settings = settings)
  covariates <- read_covariates(main     = main, 
                                settings = settings)

  dataset_controls_list <- dataset_controls(main     = main, 
                                            settings = settings, 
                                            datasets = datasets)


  messageq("  - metadata file", quiet = quiet)

   
  which_last_moon <- max(which(moons$newmoondate < cast_date))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moon        <- ifnull(end_moon, last_moon)


  ncontrols_r      <- length(rodents)
  last_rodent_moon <- 0

  for (i in 1:ncontrols_r) {

    rodent_moon_i      <- rodents[[i]]$newmoonnumber
    last_rodent_moon_i <- max(rodent_moon_i[rodent_moon_i <= end_moon], na.rm = TRUE)
    last_rodent_moon   <- max(c(last_rodent_moon, last_rodent_moon_i, na.rm = TRUE))

  }

  last_covar_moon        <- max(covariates$newmoonnumber[covariates$source == "historic"], na.rm = TRUE)
  first_cast_covar_moon  <- last_covar_moon + 1
  first_cast_rodent_moon <- last_rodent_moon + 1
  last_cast_moon         <- end_moon + lead_time
  rodent_cast_moons      <- first_cast_rodent_moon:last_cast_moon

  which_r_nms        <- which(moons$newmoonnumber%in% rodent_cast_moons)
  rodent_nm_dates    <- as.Date(moons$newmoondate[which_r_nms])
  rodent_cast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_cast_years  <- as.numeric(format(rodent_nm_dates, "%Y"))

  covar_cast_moons  <- first_cast_covar_moon:last_cast_moon
  which_c_nms       <- which(moons$newmoonnumber %in% covar_cast_moons)
  covar_nm_dates    <- as.Date(moons$newmoondate[which_c_nms])
  covar_cast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_cast_years  <- as.numeric(format(covar_nm_dates, "%Y"))

  cast_type  <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  cast_meta  <- read_casts_metadata(main     = main, 
                                    settings = settings,
                                    quiet    = quiet)
  cast_group <- max(c(0, cast_meta$cast_group)) + 1






  covariates_origin_date <- max(covariates$date[covariates$source == "historic"])

  config <- read_directory_configuration(main     = main, 
                                         settings = settings,
                                         quiet    = quiet)


  out <- list(cast_group              = cast_group,
              cast_type               = cast_type,
              models                  = models, 
              datasets                = datasets, 
              dataset_controls        = dataset_controls_list,
              time                    = list(start_moon            = start_moon,
                                             end_moon              = end_moon,
                                             last_moon             = last_moon,
                                             cast_date             = as.character(cast_date), 
                                             lead_time             = lead_time,
                                             covariate_cast_moons  = covar_cast_moons, 
                                             covariate_cast_months = covar_cast_months, 
                                             covariate_cast_years  = covar_cast_years,
                                             rodent_cast_moons     = rodent_cast_moons, 
                                             rodent_cast_months    = rodent_cast_months, 
                                             rodent_cast_years     = rodent_cast_years),
              confidence_level        = confidence_level,
              directory_configuration = config)

  write_data(x       = out, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$metadata, 
             quiet     = !verbose)

}