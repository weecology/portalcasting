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
#' @param cast_date \code{Date} of the cast, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @return \code{list} of casting metadata, which is also saved out as a YAML file (\code{.yaml}) if desired.
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
                              models    = c("AutoArima", "ESSS", "NaiveArima", "nbGARCH", "nbsGARCH", "pevGARCH", "jags_RW"), 
                              datasets  = c("all", "all_interp", "controls", "controls_interp", "exclosures", "exclosures_interp", "dm_controls", "dm_controls_interp"),
                              lead_time = 12,
                              cast_date = Sys.Date(), 
                              settings  = directory_settings(), 
                              quiet     = FALSE, 
                              verbose   = FALSE) {




  moons      <- read_moons(main     = main,
                           settings = settings)

  rodents    <- read_rodents(main     = main,
                             datasets = datasets)

  covariates <- read_covariates(main     = main,
                                settings = settings)

return()


  messageq("  -metadata file", quiet = quiet)

  last_moon <- last_moon(main = main, moons = moons, date = cast_date)

  end_moon <- ifnull(end_moon, last_moon)
  ncontrols_r <- length(rodents)
  last_rodent_moon <- 0
  for(i in 1:ncontrols_r){
    rodent_moon_i <- rodents[[i]]$newmoonnumber
    last_rodent_moon_i <- max(rodent_moon_i[rodent_moon_i <= end_moon], 
                              na.rm = TRUE)
    last_rodent_moon <- max(c(last_rodent_moon, last_rodent_moon_i, 
                            na.rm = TRUE))
  }

  last_covar_moon <- max(c(covariates$moon, covariates$moon),
                         na.rm = TRUE)
  first_cast_covar_moon <- last_covar_moon + 1
  first_cast_rodent_moon <- last_rodent_moon + 1
  last_cast_moon <- end_moon + lead_time
  rodent_cast_moons <- first_cast_rodent_moon:last_cast_moon
  which_r_nms <- which(moons$moon %in% rodent_cast_moons)
  rodent_nm_dates <- as.Date(moons$moondate[which_r_nms])
  rodent_cast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_cast_years <- as.numeric(format(rodent_nm_dates, "%Y"))

  covar_cast_moons <- first_cast_covar_moon:last_cast_moon
  which_c_nms <- which(moons$moon %in% covar_cast_moons)
  covar_nm_dates <- as.Date(moons$moondate[which_c_nms])
  covar_cast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_cast_years <- as.numeric(format(covar_nm_dates, "%Y"))

  cast_type <- ifelse(end_moon == last_moon, "forecast", "hindcast")

  cast_meta <- read_casts_metadata(main = main, quiet = quiet)
  cast_group <- max(cast_meta$cast_group) + 1


  filename_config <- settings$files$directory_config
  filename_meta <- settings$files$metadata


  config <- read_directory_config(main = main, 
                                  filename_config = 
                                    control_files$filename_config,
                                  quiet = quiet)


  out <- list(cast_group = cast_group, models = models, datasets = datasets, 
              directory_configuration = config,
              controls_rodents = datasets,
              cast_type = cast_type, start_moon = start_moon, 
              end_moon = end_moon, last_moon = last_moon, 
              lead_time = lead_time, min_lag = min_lag, 
              cast_date = as.character(cast_date), 
              covariate_cast_moons = covar_cast_moons, 
              covariate_cast_months = covar_cast_months, 
              covariate_cast_years = covar_cast_years,
              rodent_cast_moons = rodent_cast_moons, 
              rodent_cast_months = rodent_cast_months, 
              rodent_cast_years = rodent_cast_years,
              confidence_level = confidence_level)
  write_data(out, main = main, save = control_files$save, 
             filename = control_files$filename_meta, 
             overwrite = control_files$overwrite, quiet = !verbose)
}