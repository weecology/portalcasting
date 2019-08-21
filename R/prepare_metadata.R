#' @title Prepare a metadata list
#'
#' @description Sets up the metadata used for casting, in particular the 
#'  matching of time period across the data sets. This should always be run
#'  after \code{\link{prep_moons}}, \code{\link{prep_rodents}}, and 
#'  \code{\link{prep_covariates}} before any model is run.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param rodents Rodents \code{list}. See \code{\link{prep_rodents}},
#'
#' @param covariates Covariates \code{data.frame}. See 
#'  \code{\link{prep_covariates}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.
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
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param filename_meta \code{character} filename for saving the metadata.
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
#' @return \code{list} of casting metadata, which is also saved out as a 
#'  YAML file (\code{.yaml}) if desired.
#' 
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
#'   moons <- prep_moons(main)
#'   rodents <- prep_rodents(main, moons = moons)
#'   covariates <- prep_covariates(main, moons = moons)
#'   prep_metadata(main, moons, rodents, covariates)
#'  }
#' 
#' @export
#'
prep_metadata <- function(main = ".", moons = NULL,
                          rodents = NULL,
                          covariates = NULL,
                          end_moon = NULL, 
                          lead_time = 12, min_lag = 6, cast_date = Sys.Date(),
                          start_moon = 217,
                          confidence_level = 0.9, 
                          quiet = FALSE, save = TRUE,
                          overwrite = TRUE, filename_meta = "metadata.yaml", 
                          arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  rodents  <- ifnull(rodents, read_rodents(main = main))
  covariates <- ifnull(covariates, read_covariates(main = main))

  messageq("Loading metadata file into data subdirectory", quiet)
  check_args(arg_checks)
  last_moon <- pass_and_call(last_newmoon, moons = moons)
  end_moon <- ifnull(end_moon, last_moon)
  ncontrols_r <- length(rodents)
  last_rodent_pd <- 0
  for(i in 1:ncontrols_r){
    rodent_pd_i <- rodents[[i]]$period
    last_rodent_pd_i <- max(rodent_pd_i)
    last_rodent_pd <- max(c(last_rodent_pd, last_rodent_pd_i))
  }
  which_last_rodent_pd <- which(moons$period == last_rodent_pd)
  last_rodent_newmoon <- moons$newmoonnumber[which_last_rodent_pd]
  last_covar_newmoon <- tail(covariates, 1)$newmoonnumber

  first_cast_covar_newmoon <- last_covar_newmoon + 1
  first_cast_rodent_newmoon <- last_rodent_newmoon + 1
  last_cast_newmoon <- last_moon + lead_time

  rodent_cast_newmoons <- first_cast_rodent_newmoon:last_cast_newmoon
  which_r_nms <- which(moons$newmoonnumber %in% rodent_cast_newmoons)
  rodent_nm_dates <- as.Date(moons$newmoondate[which_r_nms])
  rodent_cast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_cast_years <- as.numeric(format(rodent_nm_dates, "%Y"))

  covar_cast_newmoons <- first_cast_covar_newmoon:last_cast_newmoon
  which_c_nms <- which(moons$newmoonnumber %in% covar_cast_newmoons)
  covar_nm_dates <- as.Date(moons$newmoondate[which_c_nms])
  covar_cast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_cast_years <- as.numeric(format(covar_nm_dates, "%Y"))

  cast_type <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  list(cast_type = cast_type, lead_time = lead_time, 
       min_lag = min_lag, cast_date = as.character(cast_date), 
       covariate_cast_newmoons = covar_cast_newmoons, 
       covariate_cast_months = covar_cast_months, 
       covariate_cast_years = covar_cast_years,
       rodent_cast_newmoons = rodent_cast_newmoons, 
       rodent_cast_months = rodent_cast_months, 
       rodent_cast_years = rodent_cast_years,
       confidence_level = confidence_level) %>%
  data_out(main, save, filename_meta, overwrite, quiet)
}