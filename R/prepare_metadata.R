#' @title Prepare a model-running metadata list
#'
#' @description Sets up the metadata used for casting, in particular the 
#'  matching of time period across the data sets. This should always be run
#'  after \code{\link{prep_moons}}, \code{\link{prep_rodents}}, and 
#'  \code{\link{prep_covariates}} before any model is run.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param rodents Rodents \code{list}. See \code{\link{prep_rodents}},
#'  
#' @param data_sets \code{character} vector of the rodent data set names
#'  that the model is applied to. 
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
#' @param controls_r Control \code{list} or \code{list} of control 
#'  \code{list}s from \code{\link{rodents_controls}} specifying the 
#'  structuring of the rodents tables. See \code{\link{rodents_controls}} for 
#'  details. 
#'
#' @param controls_m Additional controls for models not in the prefab set. 
#'  \cr 
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
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param filename_config \code{character} value of the path to the directory
#'  config YAML.
#'
#' @return \code{list} of casting metadata, which is also saved out as a 
#'  YAML file (\code{.yaml}) if desired.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   moons <- prep_moons()
#'   rodents <- prep_rodents(moons = moons)
#'   covariates <- prep_covariates(moons = moons)
#'   prep_metadata(moons = moons, rodents = rodents, covariates = covariates)
#'  }
#' 
#' @export
#'
prep_metadata <- function(main = ".", models = prefab_models(),
                          data_sets = NULL, 
                          moons = NULL, rodents = NULL,
                          covariates = NULL,
                          end_moon = NULL, 
                          lead_time = 12, min_lag = NULL, 
                          cast_date = Sys.Date(),
                          start_moon = 217,
                          confidence_level = 0.95, 
                          controls_r = NULL,
                          controls_m = NULL, 
                          filename_config = "dir_config.yaml", 
                          quiet = TRUE, verbose = FALSE, save = TRUE, 
                          overwrite = TRUE, filename_meta = "metadata.yaml", 
                          arg_checks = TRUE){
  check_args(arg_checks)
  min_lag_e <- extract_min_lag(models = models, controls_m = controls_m, 
                               arg_checks = arg_checks)
  data_sets_e <- extract_data_sets(models = models, controls_m = controls_m, 
                                   arg_checks = arg_checks)

  min_lag <- ifnull(min_lag, min_lag_e)
  data_sets <- ifnull(data_sets, data_sets_e)

  moons <- ifnull(moons, read_moons(main = main, arg_checks = arg_checks))
  rodents  <- ifnull(rodents, read_rodents(main = main, 
                                           data_sets = data_sets, 
                                           arg_checks = arg_checks))
  covariates <- ifnull(covariates, read_covariates(main = main))
  controls_r <- rodents_controls(data_sets = data_sets, 
                                 controls_r = controls_r, 
                                 arg_checks = arg_checks)
  messageq("  -metadata file", quiet)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                         arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  ncontrols_r <- length(rodents)
  last_rodent_moon <- 0
  for(i in 1:ncontrols_r){
    rodent_moon_i <- rodents[[i]]$moon
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

  cast_meta <- read_cast_metadata(main = main, quiet = quiet,
                                  arg_checks = arg_checks)
  cast_group <- max(cast_meta$cast_group) + 1

  config <- read_directory_config(main = main, 
                                  filename_config = filename_config,
                                  quiet = quiet, arg_checks = arg_checks)


  list(cast_group = cast_group, models = models, data_sets = data_sets, 
       directory_configuration = config,
       controls_r = controls_r,
       cast_type = cast_type, start_moon = start_moon, end_moon = end_moon,
       last_moon = last_moon, lead_time = lead_time, 
       min_lag = min_lag, cast_date = as.character(cast_date), 
       covariate_cast_moons = covar_cast_moons, 
       covariate_cast_months = covar_cast_months, 
       covariate_cast_years = covar_cast_years,
       rodent_cast_moons = rodent_cast_moons, 
       rodent_cast_months = rodent_cast_months, 
       rodent_cast_years = rodent_cast_years,
       confidence_level = confidence_level) %>%
  write_data(main = main, save = save, filename = filename_meta, 
             overwrite = overwrite, quiet = !verbose, arg_checks = arg_checks)
}