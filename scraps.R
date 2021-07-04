# code to use still!

#' @importFrom coda as.mcmc HPDinterval
#' @importFrom forecast Arima auto.arima ets forecast
#' @importFrom graphics abline axis mtext par plot points polygon rect text
#' @importFrom grDevices grey rgb

#' @importFrom jsonlite fromJSON serializeJSON unserializeJSON write_json
#' @importFrom portalr download_observations get_future_moons 
#'  load_trapping_data ndvi normalized_file_path summarize_rodent_data weather
#' @importFrom runjags run.jags runjags.options
#' @importFrom rEDM simplex tde_gp
#' @importFrom scoringRules crps_sample
#' @importFrom stats AIC lm na.omit predict qnorm quantile rgamma rnorm runif
#'  sd
#' @importFrom tools file_ext
#' @importFrom tscount tsglm
#' @importFrom utils download.file packageDescription read.csv sessionInfo
#'  tail unzip write.csv write.table
#' @importFrom viridis viridis
#' @importFrom yaml as.yaml yaml.load_file write_yaml



portalcast <- function (main = ".", 
                        models = prefab_models(),
                        end_moons = NULL)  {


}
#' @title Determine the start and end calendar dates for a cast window
#'
#' @description Based on the cast origin (\code{cast_date}), lead time
#'  (\code{lead_time}), and minimum non-0 lag (\code{min_lag}), determines
#'  the dates bracketing the requested window.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return Named \code{list} with elements \code{start} and \code{end},
#'  which are both \code{Dates}.
#'
#' @examples
#'  \donttest{
#'    create_dir()
#'    fill_raw()
#'    cast_window()
#'  }
#'
#' @export
#'
cast_window <- function(main = ".", moons = NULL, cast_date = Sys.Date(),
                        lead_time = 12, min_lag = 6,
                        control_files = files_control(), arg_checks = TRUE){
  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  lagged_lead <- lead_time - min_lag
  moons0 <- moons[moons$moondate < cast_date, ]
  last_moon <- tail(moons0, 1)
  last_moon$moondate <- as.Date(last_moon$moondate)
  moons0x <- moons0
  colnames(moons0x)[which(colnames(moons0x) == "moon")] <- "newmoonnumber"
  colnames(moons0x)[which(colnames(moons0x) == "moondate")] <- "newmoondate"
  future_moons <- get_future_moons(moons0x, num_future_moons = lead_time)
  start_day <- as.Date(last_moon$moondate) + 1
  end_day <- as.Date(future_moons$newmoondate[lead_time])
  list(start = start_day, end = end_day)
}

#' @title Measure error/fit metrics for forecasts
#' 
#' @description Summarize the cast-level errors or fits to the observations. 
#'  \cr 
#'  Presently included are root mean square error and coverage.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See 
#'  \code{\link{read_cast_tab}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{data.frame} of metrics for each cast for each species. 
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   cast_tab <- read_cast_tab(cast_id = 1)
#'   cast_tab <- add_lead_to_cast_tab(cast_tab = cast_tab)
#'   cast_tab <- add_obs_to_cast_tab(cast_tab = cast_tab)
#'   cast_tab <- add_err_to_cast_tab(cast_tab = cast_tab)
#'   cast_tab <- add_covered_to_cast_tab(cast_tab = cast_tab)
#'   measure_cast_level_error(cast_tab = cast_tab)
#' }
#'
#' @export
#'
measure_cast_level_error <- function(cast_tab = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(cast_tab)

  ucast_ids <- unique(cast_tab$cast_id)
  ncast_ids <- length(ucast_ids)
  uspecies <- unique(cast_tab$species)
  nspecies <- length(uspecies)
  RMSE <- rep(NA, ncast_ids * nspecies)
  coverage <- rep(NA, ncast_ids * nspecies)
  model <- rep(NA, ncast_ids * nspecies)
  counter <- 1
  for(i in 1:ncast_ids){
    for(j in 1:nspecies){
      ij <- cast_tab$cast_id == ucast_ids[i] & cast_tab$species == uspecies[j]
      cast_tab_ij <- cast_tab[ij, ]
      err <- cast_tab_ij$err
      if(!is.null(err)){
        RMSE[counter] <- sqrt(mean(err^2, na.rm = TRUE))
      }
      covered <- cast_tab_ij$covered
      if(!is.null(covered)){
        coverage[counter] <- mean(covered, na.rm = TRUE)            
      }
      if(length(cast_tab_ij$model) > 0){
        model[counter] <- unique(cast_tab_ij$model)
      }
      counter <- counter + 1
    }
  }
  ids <- rep(ucast_ids, each = nspecies)
  spp <- rep(uspecies, ncast_ids)
  data.frame(cast_id = ids, species = spp, model = model, 
             RMSE = RMSE, coverage = coverage)
}

#' @title Add the associated values to a cast tab
#' 
#' @description Add values to a cast's cast tab. If necessary components are
#'  missing (such as no observations added yet and the user requests errors),
#'  the missing components are added. \cr \cr
#'  \code{add_lead_to_cast_tab} adds a column of lead times. \cr \cr
#'  \code{add_obs_to_cast_tab} appends a column of observations. \cr \cr
#'  \code{add_err_to_cast_tab} adds a column of raw error values. \cr \cr
#'  \code{add_covered_to_cast_tab} appends a \code{logical} column indicating
#'  if the observation was within the prediction interval. 
#'
#' @details If an interpolated data set is used to fit a model, 
#'  \code{add_obs_to_cast_tab} adds the true (non-interpolated) observations
#'  so that model predictions are all compared to the same data.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See 
#'  \code{\link{read_cast_tab}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{data.frame} of \code{cast_tab} with an additional column or
#'  columns if needed. 
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   cast_tab <- read_cast_tab(cast_id = 1)
#'   add_lead_to_cast_tab(cast_tab = cast_tab)
#'   add_obs_to_cast_tab(cast_tab = cast_tab)
#'   add_err_to_cast_tab(cast_tab = cast_tab)
#'   add_covered_to_cast_tab(cast_tab = cast_tab)
#' }
#'
#' @name add_to_cast_tab
#'
NULL

#' @rdname add_to_cast_tab
#'
#' @export
#'
add_lead_to_cast_tab <- function(main = ".", cast_tab = NULL,
                                arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(cast_tab)
  cast_tab$lead <- cast_tab$moon - cast_tab$end_moon
  cast_tab
}

#' @rdname add_to_cast_tab
#'
#' @export
#'
add_err_to_cast_tab <- function(main = ".", cast_tab = NULL,
                                arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(cast_tab)
  if(is.null(cast_tab$obs)){
  cast_tab <- add_obs_to_cast_tab(main = main, cast_tab = cast_tab,
                                  arg_checks = arg_checks)
  }
  cast_tab$error <- cast_tab$estimate - cast_tab$obs
  cast_tab
}

#' @rdname add_to_cast_tab
#'
#' @export
#'
add_covered_to_cast_tab <- function(main = ".", cast_tab = NULL,
                                    arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(cast_tab)
  if(is.null(cast_tab$obs)){
  cast_tab <- add_obs_to_cast_tab(main = main, cast_tab = cast_tab,
                                  arg_checks = arg_checks)
  }
  cast_tab$covered <- cast_tab$obs >= cast_tab$lower_pi & 
                      cast_tab$obs <= cast_tab$upper_pi 
  cast_tab$covered[is.na(cast_tab$obs)] <- NA
  cast_tab
}

#' @rdname add_to_cast_tab
#'
#' @export
#'
add_obs_to_cast_tab <- function(main = ".", cast_tab = NULL, 
                                arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(cast_tab)
  cast_tab$obs <- NA
  cast_data_set <- gsub("_interp", "", cast_tab$data_set)
  ucast_data_set <- unique(cast_data_set)
  ncast_data_sets <- length(ucast_data_set)
  for(j in 1:ncast_data_sets){
    obs <- read_rodents_table(main = main, data_set = ucast_data_set[j],
                             arg_checks = arg_checks)
    matches <- which(cast_data_set == ucast_data_set[j])
    nmatches <- length(matches)
    obs_cols <- gsub("NA.", "NA", colnames(obs))
    for(i in 1:nmatches){
      spot <- matches[i]
      obs_moon <- which(obs$moon == cast_tab$moon[spot])
      obs_species <- which(obs_cols == cast_tab$species[spot])
      if(length(obs_moon) == 1 & length(obs_species) == 1){
        cast_tab$obs[spot] <- obs[obs_moon, obs_species]
      }
    }
  }
  cast_tab 
}



#' @title Read in cast output from a given cast
#'
#' @description Read in the various output files of a cast or casts in the 
#'  casts sub directory. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param cast_ids,cast_id \code{integer} (or integer \code{numeric}) value(s) 
#'  representing the cast(s) of interest, as indexed within the directory in
#'  the \code{casts} sub folder. See the casts metadata file 
#'  (\code{casts_metadata.csv}) for summary information. If \code{NULL} (the
#'  default), the most recently generated cast's output is read in. \cr
#'  \code{cast_ids} can be NULL, one value, or more than one values,
#'  \code{cast_id} can only be NULL or one value.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return 
#'  \code{read_cast_tab}: \code{data.frame} of the \code{cast_tab}. \cr \cr
#'  \code{read_cast_tabs}: \code{data.frame} of the \code{cast_tab}s with
#'  a \code{cast_id} column added to distinguish among casts. \cr \cr
#'  \code{read_cast_metadata}: \code{list} of \code{cast_metadata}. \cr \cr
#'  \code{read_model_fits}: \code{list} of \code{model_fits}. \cr \cr
#'  \code{read_model_casts}: \code{list} of \code{model_casts}.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   read_cast_tab(cast_id = 1)
#'   read_cast_tabs(cast_ids = 1:2)
#'   read_cast_metadata(cast_id = 1)
#'   read_model_fits(cast_id = 1)
#'   read_model_casts(cast_id = 1)
#' }
#'
#' @name read_cast_output
#'
NULL

#' @rdname read_cast_output
#'
#' @export
#'
read_cast_tab <- function(main = ".", cast_id = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  if(is.null(cast_id)){
    casts_meta <- select_casts(main = main, arg_checks = arg_checks)
    cast_id <- max(casts_meta$cast_id)
  }
  lpath <- paste0("cast_id_", cast_id, "_cast_tab.csv")
  cpath <- file_path(main, "casts", lpath, arg_checks)
  if(!file.exists(cpath)){
    stop("cast_id does not have a cast_table", call. = FALSE)
  }
  out <- read.csv(cpath, stringsAsFactors = FALSE) 
  na_conformer(out, arg_checks = arg_checks)
}

#' @rdname read_cast_output
#'
#' @export
#'
read_cast_tabs <- function(main = ".", cast_ids = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  if(is.null(cast_ids)){
    casts_meta <- select_casts(main = main, arg_checks = arg_checks)
    cast_ids <- max(casts_meta$cast_id)
  }
  cast_tab <- read_cast_tab(main = main, cast_id = cast_ids[1],
                            arg_checks = arg_checks)
  ncasts <- length(cast_ids)
  if(ncasts > 1){
    for(i in 2:ncasts){
      cast_tab_i <- read_cast_tab(main = main, cast_id = cast_ids[i],
                                  arg_checks = arg_checks)
      cast_tab <- rbind(cast_tab, cast_tab_i)
    }
  }
  cast_tab
}

#' @rdname read_cast_output
#'
#' @export
#'
read_cast_metadata <- function(main = ".", cast_id = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  if(is.null(cast_id)){
    casts_meta <- select_casts(main = main, arg_checks = arg_checks)
    cast_id <- max(casts_meta$cast_id)
  }
  lpath <- paste0("cast_id_", cast_id, "_metadata.yaml")
  cpath <- file_path(main, "casts", lpath, arg_checks)
  if(!file.exists(cpath)){
    stop("cast_id does not have a cast_metadata file", call. = FALSE)
  }
  yaml.load_file(cpath) 
}


#' @rdname read_cast_output
#'
#' @export
#'
read_model_fits <- function(main = ".", cast_id = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  if(is.null(cast_id)){
    casts_meta <- select_casts(main = main, arg_checks = arg_checks)
    cast_id <- max(casts_meta$cast_id)
  }
  lpath <- paste0("cast_id_", cast_id, "_model_fits.json")
  cpath <- file_path(main, "fits", lpath, arg_checks)
  if(!file.exists(cpath)){
    stop("cast_id does not have a model_fits file", call. = FALSE)
  }
  read_in_json <- fromJSON(readLines(cpath))
  unserializeJSON(read_in_json)
}

#' @rdname read_cast_output
#'
#' @export
#'
read_model_casts <- function(main = ".", cast_id = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  if(is.null(cast_id)){
    casts_meta <- select_casts(main = main, arg_checks = arg_checks)
    cast_id <- max(casts_meta$cast_id)
  }
  lpath <- paste0("cast_id_", cast_id, "_model_casts.json")
  cpath <- file_path(main, "casts", lpath, arg_checks)
  if(!file.exists(cpath)){
    stop("cast_id does not have a model_casts file", call. = FALSE)
  }
  read_in_json <- fromJSON(readLines(cpath))
  unserializeJSON(read_in_json)
}

#' @title Find casts that fit specifications
#'
#' @description Determines the casts that match user specifications. \cr
#'  Functionally, a wrapper on \code{\link{read_casts_metadata}} with 
#'  filtering for specifications that provides a simple user interface to
#'  the large set of available casts via the metadata. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values 
#'  representing the casts of interest, as indexed within the directory in
#'  the \code{casts} sub folder. See the casts metadata file 
#'  (\code{casts_metadata.csv}) for summary information.
#'
#' @param end_moons \code{integer} (or integer \code{numeric}) 
#'  newmoon numbers of the forecast origin. Default value is 
#'  \code{NULL}, which equates to no selection with respect to 
#'  \code{end_moon}.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value
#'  of the cast group to combine with an ensemble. If \code{NULL} (default),
#'  the most recent cast group is ensembled. 
#'
#' @param models \code{character} values of the names of the models to 
#'  include. Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{model}.
#'
#' @param data_sets \code{character} values of the rodent data sets to include
#'  Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{data_set}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param include_interp \code{logical} indicator of if the basic data set
#'  names should also be inclusive of the associated interpolated data sets.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @return \code{data.frame} of the \code{cast_tab}.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   select_casts(models = "AutoArima")
#' }
#'
#' @export
#'
select_casts <- function(main = ".", cast_ids = NULL, cast_groups = NULL,
                         end_moons = NULL, models = NULL, data_sets = NULL,
                         quiet = FALSE, include_interp = FALSE,
                         arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  subp <- sub_path(main = main, subs = "casts", arg_checks = arg_checks)
  verify(paths = subp, arg_checks = arg_checks)
  casts_metadata <- read_casts_metadata(main = main, quiet = quiet, 
                                        arg_checks = arg_checks)

  ucast_ids <- unique(casts_metadata$cast_id)
  cast_ids <- ifnull(cast_ids, ucast_ids)
  match_id <- casts_metadata$cast_id %in% cast_ids

  ucast_groups <- unique(casts_metadata$cast_group)
  cast_groups <- ifnull(cast_groups, ucast_groups)
  match_group <- casts_metadata$cast_group %in% cast_groups

  uend_moons <- unique(casts_metadata$end_moon)
  end_moons <- ifnull(end_moons, uend_moons)
  match_end_moon <- casts_metadata$end_moon %in% end_moons

  umodels <- unique(casts_metadata$model)
  models <- ifnull(models, umodels)
  match_model <- casts_metadata$model %in% models
  
  udata_sets <- unique(casts_metadata$data_set)
  data_sets <- ifnull(data_sets, udata_sets)
  if(include_interp){
    data_sets <- unique(c(data_sets, paste0(data_sets, "_interp")))
  }
  match_data_set <- casts_metadata$data_set %in% data_sets
  
  QAQC <- casts_metadata$QAQC

  match_all <- match_id & match_end_moon & match_model & match_data_set & QAQC
  casts_metadata[match_all, ]
}



#' @title Save cast output to files
#'
#' @description Save out any output from a cast of a model for a data set and
#'  update the cast metadata file accordingly to track the saved output. \cr
#'  Most users will want to at least save out model metadata and a table of
#'  predictions.
#'
#' @param cast Output from a model function (e.g., 
#'  \code{\link{AutoArima}}) run on any rodents data set. Required to be a 
#'  \code{list}, but otherwise has minimal strict requirements. \cr
#'  Names of the elements of the list (such as \code{"metadat"}) indicate the 
#'  specific saving procedures that happens to each of them. 
#'  See \code{Details} section for specifics. 
#'
#' @details Currently, four generalized output components are recognized and
#'  indicated by the names of the elements of \code{cast}. 
#'  \itemize{
#'   \item \code{"metadata"}: saved out with \code{\link[yaml]{as.yaml}}. Will
#'    typically be the model-specific metadata from the 
#'    \code{data/metadata.yaml} file, but can more generally be any 
#'    appropriate object (typically a \code{list}).  
#'   \item \code{"cast_tab"}: saved using \code{\link{write.csv}}, so is
#'    assumed to be a table such as a \code{matrix} or \code{data.frame} 
#'    or coercible to one. Used to summarize the output across instances
#'    of the model (across multiple species, for example). 
#'   \item \code{"model_fits"}: saved out as a serialized \code{JSON} file 
#'    via \code{\link[jsonlite]{serializeJSON}} and 
#'    \code{\link[jsonlite]{write_json}}, so quite flexible with respect to 
#'    specific object structure. Saving out a \code{list} of the actual model
#'    fit/return objects means that models do not need to be refit later.
#'   \item \code{"model_casts"}: saved out as a serialized \code{JSON} file 
#'    via \code{\link[jsonlite]{serializeJSON}} and 
#'    \code{\link[jsonlite]{write_json}}, so quite flexible with respect to 
#'    specific object structure. Is used to save \code{list}s
#'    of predictions across multiple instances of the model.
#'  }
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @return Relevant elements are saved to external files, and \code{NULL} is
#'  returned.
#'
#' @examples
#'  \donttest{
#'   setup_dir() 
#'   out <- AutoArima()
#'   save_cast_output(out)
#'  }
#'
#' @export
#'
save_cast_output <- function(cast = NULL, main = ".", 
                             quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  cast_meta <- read_casts_metadata(main = main, quiet = quiet, 
                                   arg_checks = arg_checks)
  cast_ids <- cast_meta$cast_id
  if(all(is.na(cast_ids))){
    next_cast_id <- 1
  } else{
    next_cast_id <- max(cast_ids) + 1
  }

  dir_config <- cast$metadata$directory_configuration
  pc_version <- dir_config$setup_portalcasting_version
  new_cast_meta <- data.frame(cast_id = next_cast_id,
                              cast_group = cast$metadata$cast_group,
                              cast_date = cast$metadata$cast_date,
                              model = cast$metadata$models,
                              data_set = cast$metadata$data_sets,
                              end_moon = cast$metadata$end_moon,
                              start_moon = cast$metadata$start_moon,
                              lead_time = cast$metadata$lead_time,
                              portalcasting_version = pc_version,
                              QAQC = TRUE, notes = NA)
  cast_meta <- rbind(cast_meta, new_cast_meta)
  meta_path <- file_path(main = main, sub = "casts", 
                         files = "casts_metadata.csv", 
                         arg_checks = arg_checks)
  write.csv(cast_meta, meta_path, row.names = FALSE)

  if(!is.null(cast$metadata)){
    meta_filename <- paste0("cast_id_", next_cast_id, "_metadata.yaml")
    meta_path <- file_path(main = main, sub = "casts", files = meta_filename,
                           arg_checks = arg_checks)
    yams <- as.yaml(cast$metadata)
    writeLines(yams, con = meta_path)
  }
  if(!is.null(cast$cast_tab)){
    cast_tab_filename <- paste0("cast_id_", next_cast_id, "_cast_tab.csv") 
    cast_tab_path <- file_path(main = main, sub = "casts", 
                               files = cast_tab_filename,
                               arg_checks = arg_checks)
    cast_tab <- cast$cast_tab
    cast_tab$cast_id <- next_cast_id
    cast_tab$cast_group <- cast$metadata$cast_group
    cast_tab$confidence_level <- cast$metadata$confidence_level
    write.csv(cast_tab, cast_tab_path, row.names = FALSE)
  }
  if(!is.null(cast$model_fits)){
    model_fits_filename <- paste0("cast_id_", next_cast_id, 
                                  "_model_fits.json") 
    model_fits_path <- file_path(main = main, sub = "fits", 
                                 files = model_fits_filename,
                                 arg_checks = arg_checks)
    model_fits <- cast$model_fits
    model_fits <- serializeJSON(model_fits)
    write_json(model_fits, path = model_fits_path)
  }
  if(!is.null(cast$model_casts)){
    model_casts_filename <- paste0("cast_id_", next_cast_id, 
                                   "_model_casts.json") 
    model_casts_path <- file_path(main = main, sub = "casts", 
                                  files = model_casts_filename, 
                                  arg_checks = arg_checks)
    model_casts <- cast$model_casts
    model_casts <- serializeJSON(model_casts)
    write_json(model_casts, path = model_casts_path)
  }
  invisible(NULL)
}

#' @title Read in the casts metadata file
#'
#' @description Read in the casts metadata file. If the data file does not
#'  exist, an effort is made to create the file.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'  
#' @return Data requested.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   read_casts_metadata()
#'  }
#'
#' @export
#'
read_casts_metadata <- function(main = ".", quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  meta_path <- file_path(main = main, sub = "casts", 
                         files = "casts_metadata.csv",
                         arg_checks = arg_checks)
  if(!file.exists(meta_path)){
    messageq("  **creating cast_metadata.csv**", quiet)
    casts_meta <- data.frame(cast_id = 0, cast_group = 0, 
                             cast_date = NA, start_moon = NA, end_moon = NA,
                             lead_time = NA, model = NA, data_set = NA,
                             portalcasting_version = NA,
                             QAQC = FALSE, notes = NA)
    write.csv(casts_meta, meta_path, row.names = FALSE)
  }
  read.csv(meta_path, stringsAsFactors = FALSE)
}


                      
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param controls_rodents Control \code{list} or \code{list} of control 
#'  \code{list}s from \code{\link{rodents_controls}} specifying the 
#'  structuring of the rodents tables. See \code{\link{rodents_controls}} for 
#'  details. 
#'
#' @param controls_model Additional controls for models not in the prefab
#'  set. \cr 
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
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
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
                          data_sets = NULL, moons = NULL, rodents = NULL,
                          covariates = NULL, end_moon = NULL, 
                          start_moon = 217, lead_time = 12, min_lag = NULL, 
                          cast_date = Sys.Date(), confidence_level = 0.95, 
                          controls_model = NULL, 
                          controls_rodents = NULL,
                          control_files = files_control(),
                          quiet = TRUE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  filename_config <- control_files$filename_config
  filename_meta <- control_files$filename_meta
  min_lag_e <- extract_min_lag(models = models,
                               controls_model = controls_model, 
                               arg_checks = arg_checks)
  data_sets_e <- extract_data_sets(models = models, 
                                   controls_model = controls_model, 
                                   arg_checks = arg_checks)

  min_lag <- ifnull(min_lag, min_lag_e)
  data_sets <- ifnull(data_sets, data_sets_e)

  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  rodents  <- ifnull(rodents, read_rodents(main = main, 
                                           data_sets = data_sets, 
                                           arg_checks = arg_checks))
  covariates <- ifnull(covariates, read_covariates(main = main, 
                                               control_files = control_files,
                                               arg_checks = arg_checks))
  controls_r <- rodents_controls(data_sets = data_sets, 
                                 controls_rodents = controls_rodents, 
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

  cast_meta <- read_casts_metadata(main = main, quiet = quiet,
                                   arg_checks = arg_checks)
  cast_group <- max(cast_meta$cast_group) + 1

  config <- read_directory_config(main = main, 
                                  filename_config = 
                                    control_files$filename_config,
                                  quiet = quiet, arg_checks = arg_checks)


  out <- list(cast_group = cast_group, models = models, data_sets = data_sets, 
              directory_configuration = config,
              controls_rodents = controls_r,
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
             overwrite = control_files$overwrite, quiet = !verbose, 
             arg_checks = arg_checks)
}

#' @title Prepare covariate data for casting
#'
#' @description Combine the historical and cast covariate data for a model
#'  run. \cr \cr See \code{\link{prep_hist_covariates}} and
#'  \code{\link{prep_cast_covariates}} for the historical and -casted
#'  covariate preparation details, respectively.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
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
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param control_climate_dl \code{list} of specifications for the download, 
#'  which are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @return \code{data.frame} of historical and -casted covariates, combined
#'  and saved out to \code{filename} if indicated by \code{save}.
#'  
#' @examples
#'  \donttest{
#'   setup_dir()
#'   prep_covariates()
#'  }
#'  
#' @export
#'
prep_covariates <- function(main = ".", moons = NULL, end_moon = NULL, 
                            start_moon = 217, lead_time = 12, min_lag = 6, 
                            cast_date = Sys.Date(),
                            control_climate_dl = climate_dl_control(), 
                            control_files = files_control(),
                            quiet = TRUE, verbose = FALSE, 
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  messageq("  -covariate data files", quiet)

  hist_cov <- prep_hist_covariates(main = main, moons = moons, quiet = quiet, 
                                   arg_checks = arg_checks)

  cast_cov <- prep_cast_covariates(main = main, moons = moons, 
                                   hist_cov = hist_cov,
                                   end_moon = end_moon, 
                                   lead_time = lead_time, min_lag = min_lag, 
                                   cast_date = cast_date, 
                                   control_files = control_files,
                                   control_climate_dl = control_climate_dl,
                                   quiet = quiet, verbose = verbose, 
                                   arg_checks = arg_checks)
  out <- combine_hist_and_cast(hist_tab = hist_cov, cast_tab = cast_cov, 
                               column = "moon", arg_checks = arg_checks)

  na_rows <- apply(is.na(out), 1, sum) > 0
  moon_in <- out$moon >= start_moon
  which_na_rows <- which(na_rows & moon_in)
  nna_rows <- length(which_na_rows)
  if(nna_rows > 0){

    cov_casts <- read_covariate_casts(main = main, 
                                      control_files = control_files, 
                                      arg_checks = arg_checks)
    for(i in 1:nna_rows){
      na_moon <- out$moon[which_na_rows[i]]
      possibles <- cov_casts[cov_casts$moon == na_moon, ]

      if(NROW(possibles > 0)){
        which_possible <- which.max(as.Date(possibles$date_made))
        cols_keep <- c("mintemp", "maxtemp", "meantemp", "precipitation",
                       "ndvi")
        patch <- data.frame(moon = na_moon, 
                            possibles[which_possible, cols_keep],
                            source = "cast")
        out[which_na_rows[i], ] <- patch
      }
    }
  }

  write_data(dfl = out, main = main, save = control_files$save, 
             filename = control_files$filename_cov, 
             overwrite = control_files$overwrite, 
             quiet = !verbose, arg_checks = arg_checks)
}


#' @title Prepare historical covariates data
#'
#' @description 
#'  \code{prep_hist_covariates} creates a \code{data.frame} of historical 
#'  weather and NDVI data. Automatically goes all the way back to the 
#'  beginning of the available data, as the table is automatically trimmed 
#'  later. \cr \cr
#'  NDVI data are generated by \code{\link[portalr]{ndvi}}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param moons Lunar data \code{data.frame}. See \code{\link{prep_moons}}.
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
#' @return 
#'  \code{prep_hist_covariates}: daily historical covariate data table as a 
#'   \code{data.frame}. 
#'
#' @examples
#'  \donttest{   
#'   create_dir()
#'   fill_raw()
#'   prep_hist_covariates()
#'  }
#'
#' @name prepare_historical_covariates
#'
NULL

#' @rdname prepare_historical_covariates
#'
#' @export
#'
prep_hist_covariates <- function(main = ".", moons = NULL,
                                 control_files = files_control(),
                                 quiet = TRUE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  raw_path <- raw_path(main = main, arg_checks = arg_checks)
  weather_data <- weather(level = "daily", fill = TRUE, path = raw_path)
  ndvi_data <- ndvi(level = "daily", fill = TRUE, path = raw_path)
  ndvi_data$date <- as.Date(ndvi_data$date)

  out <- weather_data
  out$ndvi <- NA
  spots <- na.omit(match(ndvi_data$date, weather_data$date))
  incl <- ndvi_data$date %in% weather_data$date
  out$ndvi[spots] <- ndvi_data$ndvi[incl]
  out <- add_moons_from_date(df = out, moons = moons, arg_checks = arg_checks)
  cols_in <- c("date", "moon", 
               "mintemp", "maxtemp", "meantemp", "precipitation", "ndvi")
  out <- out[ , colnames(out) %in% cols_in]
  out <- summarize_daily_weather_by_moon(out)
  out$source <- "hist"
  data.frame(out)
}



#' @title Summarize a daily weather table by newmoons
#'
#' @description Summarizes a daily weather table by newmoons. Taking the 
#'  max date, min of the min temperatures, max of the max temperatures,
#'  mean of the mean temperatures, and sum of the precipitation.
#'
#' @param x \code{data.frame} of daily weather, with columns named
#'  \code{"date"}, \code{"mintemp"}, \code{"maxtemp"}, \code{"meantemp"}, 
#'  \code{"precipitation"}, and \code{"moon"}.
#'
#' @return \code{data.frame} of \code{x} summarized and arranged by
#'  \code{x$moon}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   raw_path <- raw_path()
#'   moons <- prep_moons()
#'   weather <- portalr::weather("daily", TRUE, raw_path)
#'   weather <- add_date_from_components(weather)
#'   weather <- add_moons_from_date(weather, moons)
#'   summarize_daily_weather_by_moon(weather)
#'  }
#'
#' @export
#'
summarize_daily_weather_by_moon <- function(x){

  x <- x[!(is.na(x$moon)), ] 
  umoons <- unique(x$moon)
  numoons <- length(umoons)
  date <- rep(NA, numoons)
  mintemp <- rep(NA, numoons)
  maxtemp <- rep(NA, numoons)
  meantemp <- rep(NA, numoons)
  precipitation <- rep(NA, numoons)
  ndvi <- rep(NA, numoons)
  for(i in 1:numoons){
    moons_in <- x$moon == umoons[i]
    date[i] <- max(as.Date(x$date[moons_in], na.rm = TRUE))
    mm <- na.omit(x$mintemp[moons_in])
    if(length(mm) > 0){
      mintemp[i] <- min(mm, na.rm = TRUE)
    } else{
      mintemp[i] <- NA
    }

    mm <- na.omit(x$maxtemp[moons_in])
    if(length(mm) > 0){
      maxtemp[i] <- max(mm, na.rm = TRUE)
    } else{
      maxtemp[i] <- NA
    }

    mm <- na.omit(x$meantemp[moons_in])
    if(length(mm) > 0){
      meantemp[i] <- mean(mm, na.rm = TRUE)
    } else{
      meantemp[i] <- NA
    }

    pp <- na.omit(x$precipitation[moons_in])
    if(length(mm) > 0){
      precipitation[i] <- sum(pp, na.rm = TRUE)
    } else{
      precipitation[i] <- NA
    }

    nn <- na.omit(x$ndvi[moons_in])
    if(length(nn) > 0){
      ndvi[i] <- sum(nn, na.rm = TRUE)
    } else{
      ndvi[i] <- NA
    }

  }
  out <- data.frame(moon = umoons, mintemp, maxtemp, meantemp, precipitation,
                    ndvi)
  moon_order <- order(out$moon)
  out[moon_order, ]
}



#' @title Lag covariate data
#'
#' @description Lag the covariate data together based on the new moons
#'
#' @param covariates \code{data.frame} of covariate data to be lagged. 
#'  
#' @param lag \code{integer} lag between rodent census and covariate data, in
#'  new moons.
#'  
#' @param tail \code{logical} indicator if the data lagged to the tail end 
#'  should be retained.
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
#' @return \code{data.frame} with a \code{newmoonnumber} column reflecting
#'  the lag.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   covariate_casts <- read_covariate_casts()
#'   covar_casts_lag <- lag_covariates(covariate_casts, lag = 2, tail = TRUE)
#'  }
#'
#' @export
#'
lag_covariates <- function(covariates, lag, tail = FALSE, 
                           arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  covariates$moon_lag <- covariates$moon + lag
  
  if(tail == FALSE){
    oldest_included_moon <- covariates$moon[1]
    most_recent_moon <- covariates$moon[nrow(covariates)]
    hist_moons <- oldest_included_moon:most_recent_moon
    moon_match <- match(covariates$moon_lag, hist_moons)
    covariates$moon <- hist_moons[moon_match]
    if (lag > 0){
      covariates <- covariates[-(1:lag), ]
    }
  }
  covariates$moon <- covariates$moon_lag
  covariates$moon_lag <- NULL
  covariates
}



#' @title Cast Portal rodents models
#'
#' @description Cast the Portal rodent population data using the (updated if 
#'  needed) data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of 
#'  multiple models, with data preparation as needed between runs occurring
#'  via \code{prep_data}. \cr \cr
#'  \code{cast} runs a single cast of multiple models across data sets.
#'
#' @details Multiple models can be run together on the multiple, varying 
#'  data sets for a single new moon using \code{cast}. \code{portalcast}
#'  wraps around \code{cast}, providing updating, verification, and resetting
#'  utilities as well as facilitating multiple runs of \code{cast} across 
#'  new moons (e.g., when using a rolling origin).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param end_moons,end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number(s) of the last sample(s) to be included. Default value is 
#'  \code{NULL}, which equates to the most recently included sample. \cr
#'  \strong{\code{end_moons} allows for multiple moons, \code{end_moon}
#'  can only be one value}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
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
#'  summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set or for overriding controls of a prefab model (for example when 
#'  applying a model to a new data set). \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{data_sets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  }
#'  If only a single model's controls are included, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param controls_rodents Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details.  
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  made or not. For toggling separately from the more general \code{quiet}
#'  argument. 
#'
#' @param control_climate_dl \code{list} of specifications for the download, 
#'  which are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param downloads \code{list} of arguments to pass to \code{\link{download}}
#'  for raw file downloading. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param update_prefab_models \code{logical} indicator if all of the models'
#'  scripts should be updated, even if they do not have an explicit change
#'  to their model options via \code{controls_model}. Default is
#'  \code{FALSE}, which leads to only the models in \code{controls_model}
#'  having their scripts re-written. Switching to \code{TRUE} results in the
#'  models listed in \code{models} having their scripts re-written. \cr \cr
#'  This is particularly helpful when one is changing the global (with respect
#'  to the models) options \code{main}, \code{quiet}, \code{verbose}, or
#'  \code{control_files}.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages). 
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
#' @return Results are saved to files, \code{NULL} is returned.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast()
#'   cast()
#'  }
#'
#' @export
#'
portalcas <- function(main = ".", models = prefab_models(), end_moons = NULL, 
                       start_moon = 217, lead_time = 12, 
                       confidence_level = 0.95, cast_date = Sys.Date(),
                       controls_model = NULL, 
                       controls_rodents = rodents_controls(),
                       control_climate_dl = climate_dl_control(),
                       control_files = files_control(),
                       downloads = zenodo_downloads(c("1215988", "833438")), 
                       update_prefab_models = FALSE, bline = TRUE, 
                       quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  mainp <- main_path(main = main, arg_checks = arg_checks)
  verify(paths = mainp, arg_checks = arg_checks)
  portalcast_welcome(quiet = quiet, arg_checks = arg_checks)
  update_models(main = main, models = models, controls_model = controls_model, 
                update_prefab_models = update_prefab_models, quiet = quiet,
                verbose = verbose, control_files = control_files, 
                arg_checks = arg_checks)
  verify_models(main = main, models = models, quiet = quiet, 
                arg_checks = arg_checks)
  fill_raw(main = main, downloads = downloads, only_if_missing = TRUE, 
           quiet = quiet, control_files = control_files, 
           arg_checks = arg_checks)
  last <- last_moon(main = main, date = cast_date, arg_checks = arg_checks)
  end_moons <- ifnull(end_moons, last)
  nend_moons <- length(end_moons)
  for(i in 1:nend_moons){
    cast(main = main, models = models, end_moon = end_moons[i], 
         start_moon = start_moon, lead_time = lead_time, 
         confidence_level = confidence_level, cast_date = cast_date, 
         controls_model = controls_model, controls_rodents = controls_rodents, 
         control_climate_dl = control_climate_dl, 
         control_files = control_files, downloads = downloads, 
         quiet = quiet, verbose = verbose, arg_checks = arg_checks)
  }
  if(end_moons[nend_moons] != last){
    data_resetting_message(bline = bline, quiet = quiet, 
                           arg_checks = arg_checks)
    fill_data(main = main, models = models, 
              end_moon = NULL, lead_time = lead_time, 
              cast_date = cast_date, start_moon = start_moon, 
              confidence_level = confidence_level, 
              controls_rodents = controls_rodents,
              controls_model = controls_model, 
              control_climate_dl = control_climate_dl, 
              downloads = downloads, control_files = control_files,
              quiet = !verbose, verbose = verbose, arg_checks = arg_checks)
  }
  portalcast_goodbye(bline = bline, quiet = quiet, arg_checks = arg_checks)
} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function(main = ".", models = prefab_models(), end_moon = NULL, 
                 start_moon = 217, lead_time = 12, confidence_level = 0.95, 
                 cast_date = Sys.Date(), controls_model = NULL, 
                 controls_rodents = rodents_controls(),
                 control_climate_dl = climate_dl_control(),
                 control_files = files_control(),
                 downloads = zenodo_downloads(c("1215988", "833438")), 
                 bline = TRUE, quiet = FALSE, verbose = FALSE,
                 arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  data_readying_message(end_moon = end_moon, bline = bline, 
                        quiet = quiet, arg_checks = arg_checks)
  fill_data(main = main, models = models, end_moon = end_moon, 
            lead_time = lead_time, cast_date = cast_date, 
            start_moon = start_moon, confidence_level = confidence_level, 
            controls_rodents = controls_rodents,
            controls_model = controls_model, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, control_files = control_files,
            quiet = !verbose, verbose = verbose, arg_checks = arg_checks)

  clear_tmp(main = main, quiet = quiet, cleanup = control_files$cleanup,
            arg_checks = arg_checks)

  last <- last_moon(main = main, date = cast_date, arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last)

  models_running_message(end_moon = end_moon, bline = bline, quiet = quiet, 
                         arg_checks = arg_checks)
  models_scripts <- models_to_cast(main = main, models = models,
                                   arg_checks = arg_checks)
  nmodels <- length(models)
  for(i in 1:nmodels){
    model <- models_scripts[i]
    model_running_message(model = model, quiet = quiet,
                          arg_checks = arg_checks)
    run_status <- tryCatch(
                     source(model),
                     error = function(x){NA}                     
                  )
    model_done_message(model = model, run_status = run_status, quiet = quiet,
                       arg_checks = arg_checks)
  }
  clear_tmp(main = main, quiet = quiet, verbose = verbose, 
            cleanup = control_files$cleanup, arg_checks = arg_checks)
}
#' @title Prepare lunar data for the portalcasting repository
#'
#' @description A set of functions that get time information (calendar dates, 
#'  census periods, and newmoon numbers) associated with trapping events 
#'  (achieved and missed) based on a lunar survey schedule. If needed, 
#'  additional moons will be added to both the in-use and raw versions of 
#'  the data table. \cr \cr
#'  \code{add_future_moons} adds future moon dates to the moon table, counting 
#'  forward from \code{cast_date}. Because the \code{moons} table might not 
#'  have the most recent moons, more rows than \code{lead_time} may need to 
#'  be added to the table. \cr \cr. \cr \cr
#'  \code{add_extra_future_moons} adds more more moons to accomplish required
#'  data for \code{lead_time}. Because the moon table might not have the most 
#'  recent moons, more rows than initially requested may need to be added to 
#'  the table for it to cover \code{lead_time} .\cr \cr
#'  \code{format_moons} formats the final output table with \code{year} and
#'  \code{month} columns formatted accordingly, and the \code{moondate}
#'  column formatted as a \code{\link{Date}}. 
#'
#' @details Sometimes the raw moon data table is not fully up-to-date. Because
#'  the \code{portalr} functions \code{\link[portalr]{weather}} and 
#'  \code{\link[portalr]{fcast_ndvi}} point to the raw moons data, that table
#'  needs to be updated to produce the correct current data table for 
#'  casting. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param cast_date \code{Date} of the cast, typically today's date (set 
#'  using \code{\link{Sys.Date}}).
#'
#' @param moons Moons \code{data.frame}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#' 
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return All functions here return some version of a moons \code{data.frame}
#'  \cr \cr. 
#'  \code{prep_moons}, \code{format_moons}: fully appended and formatted 
#'  \code{data.frame} (also saved out if \code{save = TRUE}). \cr \cr
#'  \code{add_future_moons} and \code{add_extra_future_moons}: appropriately 
#'  appended moons
#'  \code{data.frame}.
#'   
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   prep_moons()
#'   pth <- file_path(files = "raw/PortalData/Rodents/moon_dates.csv")
#'   moons <- read.csv(pth, stringsAsFactors = FALSE)
#'   moons <- add_future_moons(moons)
#'   format_moons(moons)
#'  }
#'
#' @name prepare_moons
#'
NULL

#' @rdname prepare_moons
#'
#' @export
#' 
prep_moons <- function(main = ".", lead_time = 12, cast_date = Sys.Date(), 
                       control_files = files_control(), 
                       quiet = TRUE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  raw_data_present <- verify_raw_data(main = main, 
                                      raw_data = control_files$raw_data,
                                      arg_checks = arg_checks)
  if(!raw_data_present){
    fill_raw(main = main, quiet = quiet, control_files = control_files, 
             arg_checks = arg_checks)
  }
  messageq("  -lunar data file", quiet)
  raw_path <- raw_path(main = main, arg_checks = arg_checks)
  traps_in <- load_trapping_data(path = raw_path, download_if_missing = FALSE,
                                 clean = FALSE, quiet = !verbose)
  moons_in <- traps_in[["newmoons_table"]]
  moons <- add_future_moons(main = main, moons = moons_in, 
                            lead_time = lead_time, cast_date = cast_date, 
                            arg_checks = arg_checks)
  moons_out <- format_moons(moons)
  write_data(dfl = moons_out, main = main, save = control_files$save, 
             filename = control_files$filename_moons, 
             overwrite = control_files$overwrite, 
             quiet = !verbose, arg_checks = arg_checks)
} 

#' @rdname prepare_moons
#'
#' @export
#'
format_moons <- function(moons, arg_checks = TRUE){
  check_args(arg_checks)
  moons$year <- as.numeric(format(as.Date(moons$newmoondate), "%Y"))
  moons$month <- as.numeric(format(as.Date(moons$newmoondate), "%m"))
  moons$newmoondate <- as.Date(moons$newmoondate)
  colnames(moons)[which(colnames(moons) == "newmoonnumber")] <- "moon"
  colnames(moons)[which(colnames(moons) == "newmoondate")] <- "moondate"

  moons
}

#' @rdname prepare_moons
#'
#' @export
#'
add_future_moons <- function(main = ".", moons = NULL, lead_time = 12, 
                             cast_date = Sys.Date(), 
                             control_files = files_control(),
                             arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  check_args(arg_checks)
  if(lead_time == 0){
    return(moons)
  }
  future_moons <- get_future_moons(moons, lead_time)
  future_moons <- add_extra_future_moons(future_moons, cast_date)
  future_moons$newmoondate <- as.character(future_moons$newmoondate)
  rbind(moons, future_moons)
}

#' @rdname prepare_moons
#'
#' @export
#'
add_extra_future_moons <- function(moons, cast_date = Sys.Date(), 
                                   arg_checks = TRUE){
  check_args(arg_checks)
  n_extra_future_moons <- length(which(moons$newmoondate < cast_date))
  if (n_extra_future_moons > 0){
    extra_moons <- get_future_moons(moons, n_extra_future_moons)
    moons <- rbind(moons, extra_moons)
  } 
  moons
}

#' @title Trim a moons table based on target moons
#'
#' @description Some functions require that a data table of time (the moons)
#'  table only includes specific time stamps (newmoon numbers). This function
#'  is a simple utility to reduce the moons table to that as well as the 
#'  columns.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param target_moons \code{integer}-conformable newmoon numbers, usually
#'  made through \code{\link{target_moons}} of the moons to either 
#'  drop or retain based on \code{retain_target_moons}. 
#' 
#' @param retain_target_moons \code{logical} value that dictates if 
#'  \code{target_moons} are to be dropped (\code{retain_target_moons = FALSE}
#'  or retained (\code{retain_target_moons = TRUE})
#'
#' @param target_cols \code{character} vector of columns to retain.
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
#' @return \code{moons} and a \code{data.frame} but trimmed.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   moons <- prep_moons()
#'   trim_moons(moons, 300:310)
#'  }
#'
#' @export
#'
trim_moons <- function(moons = NULL, target_moons = NULL, 
                       retain_target_moons = TRUE,
                       target_cols = c("moon", "moondate", 
                                       "period", "censusdate"), 
                       arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(moons)
  moons <- moons[, target_cols]
  new_moons <- moons
  which_target_moons <- which(moons$moon %in% target_moons)
  ntarget_moons <- length(which_target_moons)
  if (ntarget_moons > 0){
    if(retain_target_moons){
      new_moons <- new_moons[which_target_moons, ]
    } else{
      new_moons <- new_moons[-which_target_moons, ]
    }
  }
  new_moons
}

#' @title Add a newmoon number to a table that has the date
#' 
#' @description Add a \code{moon} (newmoon number) column to a table that has 
#'  a \code{date} (as a \code{Date}) column.
#' 
#' @param df \code{data.frame} with column of newmoon \code{Date}s 
#'  named \code{date}.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#' 
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{data.frame} \code{x} with column of \code{newmoonnumber}s 
#'  added.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
#'   moons <- prep_moons()
#'   raw_path <- raw_path()
#'   weather <- portalr::weather("daily", fill = TRUE, path = raw_path)
#'   add_moons_from_date(weather, moons)
#'  }
#'
#' @export
#'
add_moons_from_date <- function(df, moons = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(moons, df)
  moon_number <- moons$moon[-1]
  moon_start <- as.Date(moons$moondate[-nrow(moons)])
  moon_end <- as.Date(moons$moondate[-1])
  moon_match_number <- NULL
  moon_match_date <- NULL

  for (i in seq(moon_number)) {
    temp_dates <- seq.Date(moon_start[i] + 1, moon_end[i], 1)
    temp_dates <- as.character(temp_dates)
    temp_numbers <- rep(moon_number[i], length(temp_dates))
    moon_match_date <- c(moon_match_date, temp_dates)
    moon_match_number <- c(moon_match_number, temp_numbers)
  }
  moon_match_date <- as.Date(moon_match_date)
  if (is.null(df$date)){
    df <- add_date_from_components(df)
  }
  matches <- match(df$date, moon_match_date)
  df$moon <- moon_match_number[matches]
  df
}

#' @title Determine specific newmoon numbers
#'
#' @description 
#'  \code{target_moons} determines which moons are in a window, based on a
#'  date and window width. \cr \cr
#'  \code{last_moon} determines the most recently passed newmoon, based on
#'  a date.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param date \code{Date} used for defining a window (starting with moons
#'  subsequent to the date) or a previous moon (before or on the date).
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return 
#'  \code{target_moons}: \code{numeric} vector of the moon numbers 
#'  targeted by the date window.
#'  \code{last_newmoon}:  \code{numeric} value of the last moon number.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   target_moons()
#'   last_moon()
#'  }
#'
#' @export
#'
target_moons <- function(main = ".", moons = NULL, end_moon = NULL, 
                         lead_time = 12, date = Sys.Date(), 
                         control_files = files_control(),
                         arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  check_args(arg_checks)
  last_moon <- last_moon(main = main, moons = moons, date = date, 
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  (end_moon + 1):(end_moon + lead_time)
}

#' @rdname target_moons
#'
#' @export
#'
last_moon <- function(main = ".", moons = NULL, date = Sys.Date(), 
                      control_files = files_control(), arg_checks = TRUE){
  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  which_last_moon <- max(which(moons$moondate < date))
  moons$moon[which_last_moon]
}





#' @title Determine the paths to the scripts for models to cast with
#'
#' @description Translate a \code{character} vector of model name(s) into a 
#'  \code{character} vector of file path(s) corresponding to the model 
#'  scripts. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
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
#' @return \code{character} vector of the path(s) of the R script file(s) to 
#'   be run.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   models_to_cast()
#'  }
#'
#' @export
#'
models_to_cast <- function(main = ".", models = prefab_models(), 
                           arg_checks = TRUE){
  check_args(arg_checks)
  models_path <- models_path(main = main, arg_checks = arg_checks)
  file_names <- paste0(models, ".R")
  torun <- (list.files(models_path) %in% file_names)
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 
  normalizePath(torun_paths)
}

#' @title Plot the forecast coverage and RMSE 
#'
#' @description Plot the coverage (fraction of predictions within the CI) and
#'  RMSE (root mean squared error) of each model among multiple species.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), 
#'  the table will be efficiently (as defined by the inputs) loaded and 
#'  trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input,
#'  otherwise, all relevant casts will be plotted. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values 
#'  representing the casts of interest for restricting plotting, as indexed
#'  within the directory in the \code{casts} sub folder. 
#'  See the casts metadata file (\code{casts_metadata.csv}) for summary
#'  information.
#'
#' @param end_moons \code{integer} (or integer \code{numeric}) 
#'  newmoon numbers of the forecast origin. Default value is 
#'  \code{NULL}, which equates to no selection with respect to 
#'  \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not
#'  input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to 
#'  include. Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{model}. \code{NULL} translates to all \code{models}
#'  in the table.
#'
#' @param data_set \code{character} value of the rodent data set to include
#'  Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{data_set}.
#'
#' @param ensemble \code{logical} indicator of if an ensemble should be
#'  included. Presently only the unweighted average. See 
#'  \code{\link{ensemble_casts}}.
#'
#' @param include_interp \code{logical} indicator of if the models fit using
#'  interpolated data should be included with the models that did not.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param species \code{character} vector of the species code(s) 
#'  or \code{"total"} for the total across species) to be plotted 
#'  \code{NULL} translates to the species defined by  
#'  \code{evalplot_species}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = "AutoArima", end_moons = 515:520)
#'   plot_casts_cov_RMSE()
#' }
#'
#' @export
#'
plot_casts_cov_RMSE <- function(main = ".", cast_ids = NULL, 
                                cast_tab = NULL, end_moons = NULL, 
                                models = NULL, ensemble = TRUE, 
                                data_set = NULL, include_interp = TRUE,
                                species = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  if(is.null(cast_tab)){
    cast_choices <- select_casts(main = main, cast_ids = cast_ids, 
                                 models = models, end_moons = end_moons, 
                                 data_sets = data_set, 
                                 include_interp = include_interp,
                                 arg_checks = arg_checks)
    if(NROW(cast_choices) == 0){
      stop("no casts available for requested plot", call. = FALSE)
    }else{
      cast_tab <- read_cast_tabs(main = main, cast_ids = cast_choices$cast_id,
                                 arg_checks = arg_checks)
      cast_tab <- add_obs_to_cast_tab(main = main, cast_tab = cast_tab,
                                      arg_checks = arg_checks)
      cast_tab <- add_err_to_cast_tab(main = main, cast_tab = cast_tab,
                                      arg_checks = arg_checks)
      cast_tab <- add_lead_to_cast_tab(main = main, cast_tab = cast_tab,
                                       arg_checks = arg_checks)
      cast_tab <- add_covered_to_cast_tab(main = main, cast_tab = cast_tab,
                                          arg_checks = arg_checks)
    }
  }

  cast_tab$data_set <- gsub("_interp", "", cast_tab$data_set)
  cast_ids <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models <- ifnull(models, unique(cast_tab$model))
  data_set <- ifnull(data_set, unique(cast_tab$data_set)[1])
  species <- ifnull(species, evalplot_species(arg_checks = arg_checks)) 
  end_moons <- ifnull(end_moons, unique(cast_tab$end_moon)) 
  cast_id_in <- cast_tab$cast_id %in% cast_ids
  model_in <- cast_tab$model %in% models
  data_set_in <- cast_tab$data_set == data_set
  species_in <- cast_tab$species %in% species
  end_moon_in <- cast_tab$end_moon %in% end_moons
  all_in <- cast_id_in & model_in & data_set_in & species_in & end_moon_in
  if(sum(all_in) == 0){
    stop("no casts available for requested plot", call. = FALSE)
  }
  cast_tab <- cast_tab[all_in, ]



  lp <- file_path(main, "raw", "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")

  if(ensemble){
    ecast_tab <- data.frame()
    for(i in 1:length(end_moons)){
      ecast_tab <- rbind(ecast_tab, 
                         ensemble_casts(main = main, cast_tab = cast_tab,
                                        end_moon = end_moons[i],
                                        models = models, data_set = data_set,
                                        species = species, 
                                        arg_checks = arg_checks))
    }
    ecast_tab <- ecast_tab[ , -which(colnames(ecast_tab) == "var")]
    models <- c(models, as.character(unique(ecast_tab$model)))
    cast_tab <- cast_tab[ , colnames(cast_tab) %in% colnames(ecast_tab)]
    cast_tab <- rbind(cast_tab, ecast_tab)
  }
  cast_level_errs <- measure_cast_level_error(cast_tab = cast_tab,
                                              arg_checks = arg_checks)
  nmodels <- length(models)
  nspecies <- length(species)


  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(fig = c(0, 1, 0, 1), mar = c(0.5, 0, 0, 0.5))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n")
  models2 <- gsub("ensemble_unwtavg", "Ensemble", models)
  x1 <- 0
  x2 <- 0.48
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = models2, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)
  x1 <- 0.49
  x2 <- 0.97
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = models2, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)

  for (i in 1:nspecies){

    x1 <- 0
    x2 <- 0.48
    y1 <- 0.05 + (i - 1) * 0.95 * (1/nspecies)
    y2 <- y1 + 0.94 * (1/nspecies)
    par(mar = c(0, 2.5, 1.5, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", 
         xlab = "", xlim = c(0.5, nmodels + 0.5), ylim = c(0,1), bty = "L")
    axis(2, at = seq(0, 1, 0.2), cex.axis = 0.6, las = 1, line = -0.5, 
         lwd = 0)
    axis(2, at = seq(0, 1, 0.2), labels = FALSE, tck = -0.025)
    mtext(side = 2, "Coverage", line = 1.5, cex = 0.75)
    axis(1, at = 1:nmodels, labels = FALSE, tck = -0.025)
    for(j in 1:nmodels){
      in_ij <- which(cast_level_errs$species == species[i] & 
                     cast_level_errs$model == models[j])

      ys <- na.omit(cast_level_errs$coverage[in_ij])
      ys2 <- runif(length(ys), ys - 0.005, ys + 0.005)
      xs <- runif(length(ys), j - 0.05, j + 0.05)
      quants <- quantile(ys, seq(0, 1, 0.25))
      points(rep(j, 2), quants[c(1, 5)], type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[1], 2), type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[5], 2), type = "l")
      rect(j - 0.1, quants[2], j + 0.1, quants[4], col = "white")
      points(c(j - 0.1, j + 0.1), rep(quants[3], 2), type = "l", lwd = 2)
      points(xs, ys2, col = rgb(0.3, 0.3, 0.3, 0.4), pch = 1, cex = 0.5)
    }
    
    abline(h = 0.95, lwd = 2, lty = 3)

    in_i <- which(cast_level_errs$species == species[i])
    cast_level_errs_i <- cast_level_errs[in_i, ]
    ymax <- max(max(cast_level_errs_i$RMSE, na.rm = TRUE))
    x1 <- 0.49
    x2 <- 0.97
    y1 <- 0.05 + (i - 1) * 0.95 * (1/nspecies)
    y2 <- y1 + 0.94 * (1/nspecies)
    par(mar = c(0, 2.5, 1.5, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", bty = "L",
         xlab = "", xlim = c(0.5, nmodels + 0.5), ylim = c(0, ymax))
    axis(2, cex.axis = 0.6, las = 1, line = -0.5, lwd = 0)
    axis(2, labels = FALSE, tck = -0.025)
    mtext(side = 2, "RMSE", line = 1.625, cex = 0.75)
    axis(1, at = 1:nmodels, labels = FALSE, tck = -0.025)
    for(j in 1:nmodels){
      in_ij <- which(cast_level_errs$species == species[i] & 
                     cast_level_errs$model == models[j])

      ys <- na.omit(cast_level_errs$RMSE[in_ij])
      ys2 <- runif(length(ys), ys - 0.005, ys + 0.005)
      xs <- runif(length(ys), j - 0.05, j + 0.05)
      quants <- quantile(ys, seq(0, 1, 0.25))
      points(rep(j, 2), quants[c(1, 5)], type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[1], 2), type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[5], 2), type = "l")
      rect(j - 0.1, quants[2], j + 0.1, quants[4], col = "white")
      points(c(j - 0.1, j + 0.1), rep(quants[3], 2), type = "l", lwd = 2)
      points(xs, ys2, col = rgb(0.3, 0.3, 0.3, 0.4), pch = 1, cex = 0.5)
    }
    par(mar = c(0, 0, 0, 0), fig = c(0.97, 1, y1, y2), new = TRUE)   
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",
         bty = "n") 
    if (species[i] == "total"){
      spt <- "Total Rodents"
      spf <- 2
    } else{
      spptextmatch <- which(sptab[ , "speciescode"] == species[i])
      spt <- sptab[spptextmatch, "scientificname"]
      spf <- 4
    }  
    text(0.9, 1, spt, font = spf, cex = 0.8, xpd = TRUE, srt = 270)
  }


}


#' @title Plot the forecast error as a function of lead time 
#'
#' @description Plot the raw error (estimate - observation) as a function
#'  of lead time across model runs from different forecast origins 
#'  (\code{end_moons}) for multiple models and multiple species (or total) 
#'  within a data set.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), 
#'  the table will be efficiently (as defined by the inputs) loaded and 
#'  trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input,
#'  otherwise, all relevant casts will be plotted. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values 
#'  representing the casts of interest for restricting plotting, as indexed
#'  within the directory in the \code{casts} sub folder. 
#'  See the casts metadata file (\code{casts_metadata.csv}) for summary
#'  information.
#'
#' @param end_moons \code{integer} (or integer \code{numeric}) 
#'  newmoon numbers of the forecast origin. Default value is 
#'  \code{NULL}, which equates to no selection with respect to 
#'  \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not
#'  input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to 
#'  include. Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{model}. \code{NULL} translates to all \code{models}
#'  in the table.
#'
#' @param data_set \code{character} value of the rodent data set to include
#'  Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{data_set}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param ensemble \code{logical} indicator of if an ensemble should be
#'  included. Presently only the unweighted average. See 
#'  \code{\link{ensemble_casts}}.
#'
#' @param include_interp \code{logical} indicator of if the models fit using
#'  interpolated data should be included with the models that did not.
#'
#' @param species \code{character} vector of the species code(s) 
#'  or \code{"total"} for the total across species) to be plotted 
#'  \code{NULL} translates to the species defined by  
#'  \code{evalplot_species}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = "AutoArima", end_moons = 515:520)
#'   plot_casts_err_lead()
#' }
#'
#' @export
#'
plot_casts_err_lead <- function(main = ".", cast_ids = NULL, 
                                cast_tab = NULL, end_moons = NULL, 
                                models = NULL, ensemble = TRUE, 
                                data_set = "controls", include_interp = TRUE,
                                species = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  if(is.null(cast_tab)){
    cast_choices <- select_casts(main = main, cast_ids = cast_ids, 
                                 models = models, end_moons = end_moons, 
                                 data_sets = data_set, 
                                 include_interp = include_interp,
                                 arg_checks = arg_checks)
    if(NROW(cast_choices) == 0){
      stop("no casts available for requested plot", call. = FALSE)
    }else{
      cast_tab <- read_cast_tabs(main = main, cast_ids = cast_choices$cast_id,
                                 arg_checks = arg_checks)
      cast_tab <- add_obs_to_cast_tab(main = main, cast_tab = cast_tab,
                                      arg_checks = arg_checks)
      cast_tab <- add_err_to_cast_tab(main = main, cast_tab = cast_tab,
                                      arg_checks = arg_checks)
      cast_tab <- add_lead_to_cast_tab(main = main, cast_tab = cast_tab,
                                       arg_checks = arg_checks)
    }
  }
  cast_tab$data_set <- gsub("_interp", "", cast_tab$data_set)
  cast_ids <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models <- ifnull(models, unique(cast_tab$model))
  data_set <- ifnull(data_set, unique(cast_tab$data_set)[1])
  species <- ifnull(species, evalplot_species(arg_checks = arg_checks)) 
  end_moons <- ifnull(end_moons, unique(cast_tab$end_moon)) 
  cast_id_in <- cast_tab$cast_id %in% cast_ids
  model_in <- cast_tab$model %in% models
  data_set_in <- cast_tab$data_set == data_set
  species_in <- cast_tab$species %in% species
  end_moon_in <- cast_tab$end_moon %in% end_moons
  all_in <- cast_id_in & model_in & data_set_in & species_in & end_moon_in
  if(sum(all_in) == 0){
    stop("no casts available for requested plot", call. = FALSE)
  }


  cast_tab <- cast_tab[all_in, ]
  lp <- file_path(main, "raw", "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")

  if(ensemble){
    ecast_tab <- data.frame()
    for(i in 1:length(end_moons)){
      ecast_tab <- rbind(ecast_tab, 
                         ensemble_casts(main = main, cast_tab = cast_tab,
                                        end_moon = end_moons[i],
                                        models = models, data_set = data_set,
                                        species = species, 
                                        arg_checks = arg_checks))
    }
    ecast_tab <- ecast_tab[ , -which(colnames(ecast_tab) %in% 
                                     c("var", "covered"))]
    models <- c(models, as.character(unique(ecast_tab$model)))
    cast_tab <- cast_tab[ , colnames(cast_tab) %in% colnames(ecast_tab)]
    cast_tab <- rbind(cast_tab, ecast_tab)
  }
  nmodels <- length(models) 
  nspecies <- length(species)

  if(nmodels == 1 & nspecies == 1){

    yy <- round(cast_tab$error, 3)
    yrange <- range(c(0, yy), na.rm = TRUE)
    xrange <- c(max(cast_tab$lead) + 0.25, 0)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(bty = "L", mar = c(4, 4.5, 3, 1))

    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         ylim = yrange, xlim = xrange)
    abline(h = 0, lty = 3, lwd = 2, col = grey(0.6))
    ucast_ids <- unique(cast_tab$cast_id)
    ncast_ids <- length(ucast_ids)
    cols <- viridis(ncast_ids, 0.8, 0, 0.75)
    for(k in 1:ncast_ids){
      matches <- which(cast_tab$cast_id == ucast_ids[k])
      x <- cast_tab$lead[matches]
      y <- cast_tab$error[matches]
      x <- x[!is.na(y)]
      y <- y[!is.na(y)]
      points(x, y, type = "o", pch = 16, col = cols[k], cex = 1)
    }
    axis(1, cex.axis = 1.25)
    axis(2, cex.axis = 1.25, las = 1)
    mtext(side = 1, "Lead time (new moons)", cex = 1.5, line = 2.75)
    mtext(side = 2, "Forecast error", cex = 1.75, line = 3)
    models <- gsub("ensemble_unwtavg", "Ensemble", models)

    if (species == "total"){
      spp <- "total abundance"
      title <- paste0(models, ", ", data_set, ", ", spp)
    } else{
      sppmatch <- which(sptab[ , "speciescode"] == species)
      spp <- sptab[sppmatch , "scientificname"]
      title <- eval(substitute(
                      expression(
                        paste(models_i, ", ", data_set, ", ", italic(spp))), 
                      env = list(spp = spp, data_set = data_set,  
                                 models_i = models)))
    }
    mtext(title, side = 3, cex = 1.25, line = 0.5, at = xrange[1], adj = 0)

  }else{

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(fig = c(0, 1, 0, 1), mar = c(0.5, 0, 0, 0.5))
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
         bty = "n")
    mtext(side = 1, "Lead time (new moons)", cex = 1.25, line = -0.5)
    mtext(side = 2, "Forecast error", cex = 1.25, line = -1)

    x1 <- seq(0.04, 0.96 - 0.92/nmodels, 0.92/nmodels)
    x2 <- seq(0.04 + 0.92/nmodels, 0.96, 0.92/nmodels)
    y1 <- seq(0.04, 0.96 - 0.92/nspecies, 0.92/nspecies)
    y2 <- seq(0.04 + 0.92/nspecies, 0.96, 0.92/nspecies)

    for(j in 1:nspecies){

      species_in <- cast_tab$species %in% species[j]
      yy <- round(cast_tab$error[species_in], 3)
      yrange <- range(c(0, yy), na.rm = TRUE)

      for(i in 1:nmodels){

        if(tolower(models[i]) == "ensemble_unwtavg"){
          cast_id_in <- cast_tab$cast_id %in% 
                        as.numeric(paste0(9999, cast_ids))
        } else{
          cast_id_in <- cast_tab$cast_id %in% cast_ids
        }
        data_set_in <- cast_tab$data_set == data_set
        model_in <- cast_tab$model %in% models[i]
        species_in <- cast_tab$species %in% species[j]
        end_moon_in <- cast_tab$end_moon %in% end_moons

        all_in <- cast_id_in & model_in & data_set_in & species_in & 
                  end_moon_in
        pcast_tab <- cast_tab[all_in, ]
        par(bty = "L", mar = c(0.5, 0.5, 0.25, 0.25), 
            fig = c(x1[i], x2[i], y1[j], y2[j]), new = TRUE)

        xrange <- c(max(pcast_tab$lead) + 0.25, 0)
        plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
             ylim = yrange, xlim = xrange)
        abline(h = 0, lty = 3, lwd = 2, col = grey(0.6))
        ucast_ids <- unique(pcast_tab$cast_id)
        ncast_ids <- length(ucast_ids)
        cols <- viridis(ncast_ids, 0.8, 0, 0.75)
        for(k in 1:ncast_ids){
          matches <- which(pcast_tab$cast_id == ucast_ids[k])
          x <- pcast_tab$lead[matches]
          y <- pcast_tab$error[matches]
          x <- x[!is.na(y)]
          y <- y[!is.na(y)]
          points(x, y, type = "o", pch = 16, col = cols[k], cex = 0.5)
        }
        xaxl <- ifelse(j == 1, TRUE, FALSE)
        xat <- seq(0, max(pcast_tab$lead), 2)
        axis(1, tck = -0.06, labels = FALSE, at = xat)
        axis(1, tck = -0.06, labels = xaxl, at = xat, cex.axis = 0.6, 
             line = -0.9, lwd = 0)
        xat <- seq(0, max(pcast_tab$lead), 1)
        axis(1, tck = -0.04, labels = FALSE, at = xat)

        yaxl <- ifelse(i == 1, TRUE, FALSE)
        axis(2, tck = -0.05, labels = FALSE)
        axis(2, tck = -0.05, labels = yaxl, cex.axis = 0.6, las = 1, 
             line = -0.55, lwd = 0)
        if(j == nspecies){
          mod_name <- models[i]
          mod_name <- gsub("ensemble_unwtavg", "Ensemble", mod_name)
          mtext(side = 3, mod_name, cex = 0.85, font = 2, line = 0.5) 
        }
        if(i == nmodels){
          par(mar = c(0, 0, 0, 0), fig = c(x2[i], 1, y1[j], y2[j]),
              new = TRUE)   
          plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",
               bty = "n")   
          if (species[j] == "total"){
            spt <- "Total Abundance"
            spf <- 2
          } else{
            spptextmatch <- which(sptab[ , "speciescode"] == species[j])
            spt <- sptab[spptextmatch, "scientificname"]
            spf <- 4
          }  
          text(0.75, 1, spt, font = spf, cex = 0.55, xpd = TRUE, srt = 270)
        }

      }
    }
  }  
}





#' @title Plot predictions for a given point in time across multiple species
#'
#' @description Plot the point value with confidence interval for a time point
#'  across multiple species. Casts can be selected either by supplying a 
#'  \code{cast_id} number or any combination of \code{data_set},
#'  \code{model}, and \code{end_moon}, which filter the available casts in
#'  unison. This plot type can only handle output from a single cast, so
#'  if multiple casts still remain, the one with the highest number is 
#'  selected. To be more certain about cast selection, use the \code{cast_id}
#'  input.
#'
#' @details The resulting plot shows predictions as points (open white
#'  circles) with error, where the point represents the \code{estimate} and
#'  the bounds of the error are \code{lower_pi} and \code{upper_pi} in the
#'  \code{cast_table} saved output from a model. \cr
#'  As of \code{portalcasting v0.9.0}, 
#'  this represents the mean and the 95\% prediction interval. If
#'  \code{with_census = TRUE}, the observations from the associated moon
#'  are plotted as blue filled squares. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number of the forecast origin. Default value is 
#'  \code{NULL}, which equates to no selection with respect to 
#'  \code{end_moon}.
#'
#' @param model \code{character} value of the name of the model to 
#'  include. Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{model}. Also available is \code{"Ensemble"}, which
#'  combines the models via \code{\link{ensemble_casts}}. 
#'
#' @param data_set \code{character} value of the rodent data set to include
#'  Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{data_set}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param moon \code{integer} (or integer \code{numeric}) newmoon number for
#'  the plot. 
#'
#' @param cast_id \code{integer} (or integer \code{numeric}) value 
#'  representing the cast of interest, as indexed within the directory in
#'  the \code{casts} sub folder. See the casts metadata file 
#'  (\code{casts_metadata.csv}) for summary information.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param species \code{character} vector of the species codes (or 
#'  \code{"total"} for the total across species) to be plotted or 
#'  \code{NULL} (default) to plot all species in \code{data_set}.
#' 
#' @param highlight_sp \code{character} vector of the species codes (or 
#'  \code{"total"} for the total across species) to be highlighted or 
#'  \code{NULL} (default) to not highlight anything.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value
#'  of the cast group to combine with an ensemble. If \code{NULL} (default),
#'  the most recent cast group is ensembled. 
#'
#' @param with_census \code{logical} toggle if the plot should include the
#'  observed data collected during the predicted census.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   plot_cast_point()
#' }
#'
#' @export
#'
plot_cast_point <- function(main = ".", cast_id = NULL, cast_groups = NULL,
                            data_set = NULL, model = NULL, end_moon = NULL, 
                            species = NULL, highlight_sp = NULL,
                            moon = NULL, with_census = FALSE, 
                            control_files = files_control(),
                            quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- read_moons(main = main, control_files = control_files,
                      arg_checks = arg_checks)
  last_census_date <- last_census(main = main, arg_checks = arg_checks)
  which_last_census_moon <- which(moons$censusdate == last_census_date)
  last_census_moon <- moons$moon[which_last_census_moon]
  last_moon <- last_moon(main = main, moons = moons, arg_checks = arg_checks)
  alt_moon <- ifelse(with_census, last_census_moon, last_moon + 1)
  moon <- ifnull(moon, alt_moon)
  model <- ifnull(model, "Ensemble")
  model2 <- model
  if(!is.null(model) && tolower(model) == "ensemble"){
    model2 <- NULL
  }
  casts_meta <- select_casts(main = main, cast_ids = cast_id,
                             end_moons = end_moon, models = model2, 
                             data_sets = data_set, 
                             quiet = quiet, arg_checks = arg_checks)

  if(with_census){
    casts_meta_moon1 <- casts_meta$end_moon + 1
    casts_meta_moon2 <- casts_meta$end_moon + casts_meta$lead_time
    casts_last_census <- last_census_moon >= casts_meta_moon1 &
                         last_census_moon <= casts_meta_moon2 
    casts_meta <- casts_meta[casts_last_census, ]
  }
  if(NROW(casts_meta) > 1){
    which_max <- which.max(casts_meta$cast_id)
    casts_meta <- casts_meta[which_max, ]
  }
  if(NROW(casts_meta) == 0){
    stop("no casts available for requested plot", call. = FALSE)
  }

  max_obs <- 0
  if(with_census){
    obs <- read_rodents_table(main = main, data_set = casts_meta$data_set, 
                              arg_checks = arg_checks)
    colnames(obs) <- gsub("\\.", "", colnames(obs))
    sp_col <- is_sp_col(obs, nadot = TRUE, total = TRUE)
    species <- ifnull(species, colnames(obs)[sp_col])
    moon <- ifnull(moon, unique(obs$moon))
    obs <- obs[obs$moon %in% moon, species, drop = FALSE]
    if(NROW(obs) == 0){
      stop("no observations available for requested plot", call. = FALSE) 
    } 
    max_obs <- max(as.numeric(obs), na.rm = TRUE)
  }
  data_set <- casts_meta$data_set
  if(!is.null(model) && tolower(model) == "ensemble"){
    data_set <- gsub("_interp", "", data_set)
    preds <- ensemble_casts(main = main, cast_groups = cast_groups,
                            end_moon = casts_meta$end_moon, 
                            data_set = data_set, species = species,
                            arg_checks = arg_checks)

  } else{
    preds <- read_cast_tab(main = main, cast_id = casts_meta$cast_id, 
                           arg_checks = arg_checks)
  }
  preds <- na_conformer(preds, "species")
  species <- ifnull(species, unique(preds$species))
  match_sp <- (preds$species %in% species)
  moon <- ifnull(moon, unique(preds$moon))
  match_moon <- (preds$moon %in% moon)
  colnames <- c("moon", "species", "estimate", "lower_pi", "upper_pi")
  match_col <- (colnames(preds) %in% colnames)
  preds <- preds[match_sp & match_moon, match_col]

  moon_month <- moons$month[moons$moon == moon]
  moon_year <- moons$year[moons$moon == moon]
  title_date <- paste(month.abb[moon_month], moon_year, sep = " ")
  data_set_name <- gsub("_interp", " (interpolated)", data_set)
  model_name <- ifnull(model, casts_meta$model)
  title <- paste0(title_date, ", " , model_name, ", ", data_set_name)

  preds <- preds[order(preds$estimate, decreasing = TRUE), ]
  species <- preds$species
  nspp <- length(species)
  rangey <- c(nspp + 0.25, 0.75)
  rangex <- c(0, max(c(preds$upper_pi, max_obs), na.rm = TRUE))

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(3.5, 9.5, 2, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", yaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)
  mtext("Abundance", side = 1, cex = 1.5, line = 2.5)
  mtext("Species", side = 2, cex = 1.5, line = 8.25)
  mtext(title, side = 3, cex = 1.25, line = 0.5, at = 0, adj = 0)

  lpath <- file_path(main = main, sub = "raw",  
                     files = "PortalData/Rodents/Portal_rodent_species.csv",
                     arg_checks = arg_checks)
  sptab <- read.csv(lpath, stringsAsFactors = FALSE) 
  sptab <- na_conformer(sptab, "speciescode")

  for(i in 1:nspp){
    if (species[i] == "total"){
      lab_text <- "Total"
      lab_font <- 1
    } else{
      sppmatch <- which(sptab[ , "speciescode"] == species[i])
      lab_text <- sptab[sppmatch , "scientificname"]
      lab_font <- 3
    }
    axis(2, at = i, labels = lab_text, font = lab_font, las = 1, 
         cex.axis = 0.65, tck = 0, line = -0.5, lwd = 0)
    axis(2, at = i, labels = FALSE, las = 1, 
         cex.axis = 0.65, tck = -0.01)
  }

  for(i in 1:nspp){
    low <- max(c(preds$lower_pi[i], 0))
    up <- preds$upper_pi[i]
    est <- preds$estimate[i]
    vbars <- i + (0.015 * nspp * c(-1, 1))
    if (!is.null(highlight_sp) && species[i] %in% highlight_sp){
      col = rgb(0.2, 0.5, 0.9)
      lwd = 4
    } else {
      col = "black"
      lwd = 2
    }
    points(c(low, up), rep(i, 2), type = "l", col = col, lwd = lwd)
    points(rep(low, 2), vbars, type = "l", col = col, lwd = lwd)
    points(rep(up, 2), vbars, type = "l", col = col, lwd = lwd)
    points(est, i, pch = 16, col = "white", cex = 1.25)
    points(est, i, lwd = lwd, col = col, cex = 1.25)

   if (with_census){
      spmatch <- preds$species[i]
      spmatch[spmatch == "NA"] <- "NA."
      obsi <- obs[ , spmatch]
      points(obsi, i, pch = 15, col = rgb(0, 0.4, 0.9, 0.8), cex = 1.25)
      points(obsi, i, pch = 0, col = rgb(0.2, 0.2, 0.2, 0.8), cex = 1.25)
    }   
  }
  invisible(NULL)
}



#' @title Visualize a time series cast of a species
#'
#' @description Plot an observed timeseries and cast timeseries with a 
#'  prediction interval. \cr
#'  Casts can be selected either by supplying a 
#'  \code{cast_id} number or any combination of \code{data_set},
#'  \code{model}, and \code{end_moon}, which filter the available casts in
#'  unison. This plot type can only handle output from a single cast, so
#'  if multiple casts still remain, the one with the highest number is 
#'  selected. To be more certain about cast selection, use the \code{cast_id}
#'  input.
#'
#' @details The resulting plot shows observations as a solid black line 
#'  and predictions as a blue polygon with the bounds represent the error
#'  given by \code{lower_pi} and \code{upper_pi} and the bisecting blue line
#'  representing the \code{estimate} in the \code{cast_table} saved output 
#'  from a model. \cr
#'  As of \code{portalcasting v0.9.0}, 
#'  this represents the mean and the 95\% prediction interval. \cr
#'  Observations that occurred after the cast are shown connected directly
#'  to the pre-cast observation data (as the black solid line).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number of the forecast origin. Default value is 
#'  \code{NULL}, which equates to no selection with respect to 
#'  \code{end_moon}.
#'
#' @param model \code{character} value of the name of the model to 
#'  include. Default value is \code{NULL}, which equates to \code{"Ensemble"}
#'  or an unweighted combination of the most recent cast group's models via 
#'  \code{\link{ensemble_casts}}. 
#'
#' @param data_set \code{character} value of the rodent data set to include
#'  Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{data_set}.
#'
#' @param cast_id \code{integer} (or integer \code{numeric}) value 
#'  representing the cast of interest, as indexed within the directory in
#'  the \code{casts} sub folder. See the casts metadata file 
#'  (\code{casts_metadata.csv}) for summary information.
#'
#' @param species \code{character} value of the species codes (or 
#'  \code{"total"} for the total across species) to be plotted. 
#'  \code{NULL} (default) also gives the total.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon 
#'  number for the beginning of the x-axis of the plot. \cr
#'  Does not influence the fit of the models, just the presentation.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value
#'  of the cast group to combine with an ensemble. If \code{NULL} (default),
#'  the most recent cast group is ensembled. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   plot_cast_ts()
#' }
#'
#' @export
#'
plot_cast_ts <- function(main = ".", cast_id = NULL, cast_groups = NULL,
                         data_set = NULL, model = NULL, end_moon = NULL, 
                         species = "total", start_moon = 217, 
                         control_files = files_control(), 
                         quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  model <- ifnull(model, "Ensemble")
  model2 <- model
  if(!is.null(model) && tolower(model) == "ensemble"){
    model2 <- NULL
  }
  casts_meta <- select_casts(main = main, cast_ids = cast_id,
                             end_moons = end_moon, models = model2, 
                             data_sets = data_set, quiet = quiet, 
                             arg_checks = arg_checks)
  if(NROW(casts_meta) > 1){
    which_max <- which.max(casts_meta$cast_id)
    casts_meta <- casts_meta[which_max, ]
  }
  if(NROW(casts_meta) == 0){
    stop("no casts available for requested plot", call. = FALSE)
  }

  obs <- read_rodents_table(main = main, data_set = casts_meta$data_set, 
                            arg_checks = arg_checks)
  colnames(obs) <- gsub("\\.", "", colnames(obs))

  sp_col <- is_sp_col(obs, nadot = TRUE, total = TRUE)
  species <- ifnull(species, colnames(obs)[sp_col])

  if(!all(species %in% colnames(obs))){
    stop("cast not available for requested species", call. = FALSE)
  }
  obs <- obs[ , c("moon", species)]
  data_set <- casts_meta$data_set
  if(!is.null(model) && tolower(model) == "ensemble"){
    data_set <- gsub("_interp", "", data_set)
    preds <- ensemble_casts(main = main, cast_groups = cast_groups,
                            end_moon = casts_meta$end_moon, 
                            data_set = data_set, species = species,
                            arg_checks = arg_checks)
  } else{
    preds <- read_cast_tab(main = main, cast_id = casts_meta$cast_id, 
                           arg_checks = arg_checks)
  }
  species <- ifnull(species, unique(preds$species))
  match_sp <- (preds$species %in% species)
  colnames <- c("moon", "estimate", "lower_pi", "upper_pi")
  match_col <- (colnames(preds) %in% colnames)
  preds <- preds[match_sp, match_col]

  max_moon <- max(preds$moon)
  rangex <- c(start_moon, max_moon)
  obs_sp <- obs[ , species]
  obs_nm <- obs[ , "moon"]
  maxy <- max(c(preds$UpperPI, obs_sp), na.rm = TRUE)
  rangey <- c(0, maxy)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(3, 4.5, 2, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", xaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)

  moons <- read_moons(main = main, control_files = control_files,
                      arg_checks = arg_checks)
  minx <- as.character(moons$moondate[moons$moon == rangex[1]])
  maxx <- as.character(moons$moondate[moons$moon == rangex[2]])
  minx_yr <- as.numeric(format(as.Date(minx), "%Y"))
  maxx_yr <- as.numeric(format(as.Date(maxx), "%Y"))
  minx_yr2 <- ceiling(minx_yr/ 5) * 5
  maxx_yr2 <- floor(maxx_yr/ 5) * 5
  yrs <- seq(minx_yr2, maxx_yr2, 5)
  txt <- yrs
  nyd <- paste0(yrs, "-01-01")
  dx <- as.Date(as.character(moons$moondate))
  dy <- moons$moon
  dmod <- lm(dy ~ dx)
  loc <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(1, at = loc, labels = txt)
  yrs <- seq(minx_yr, maxx_yr, 1)
  nyd <- paste0(yrs, "-01-01")
  loc <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(1, at = loc, labels = FALSE, tck = -0.005)  

  lab <- list(text = "", font = 1)
  lp <- file_path(main, "raw", "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")
  if (species == "total"){
    lab$text <- "Total Abundance"
    lab$font <- 1
  } else{
    sppmatch <- which(sptab[ , "speciescode"] == species)
    lab$text <- sptab[sppmatch , "scientificname"]
    lab$font <- 3
  }
  mtext(lab$text, side = 2, font = lab$font, cex = 1.5, line = 3)

  o_x <- obs_nm
  o_y <- obs_sp
  p_y_m <- preds[ , "estimate"]
  p_y_l <- preds[ , "lower_pi"]
  p_y_u <- preds[ , "upper_pi"]
  p_x <- preds[ , "moon"]
  for(i in 1:length(p_y_l)){
    p_y_l[i] <- max(c(0, p_y_l[i]))
  }
  first_pred <- min(p_x)
  last_o_x <- max(o_x[!is.na(o_y) & o_x < first_pred])
  last_o_y <- o_y[o_x == last_o_x]

  poly_x <- c(last_o_x, p_x, p_x[length(p_x):1], last_o_x)
  poly_y <- c(last_o_y, p_y_l, p_y_u[length(p_y_u):1], last_o_y)
  polygon(poly_x, poly_y, col = rgb(0.6757, 0.8438, 0.8984), border = NA)
  points(c(last_o_x, p_x), c(last_o_y, p_y_m), type = "l", lwd = 2, lty = 1,
         col = rgb(0.2, 0.5, 0.9))

  o_x_1 <- o_x[!is.na(o_y) & o_x < first_pred]
  o_y_1 <- o_y[!is.na(o_y) & o_x < first_pred]
  n_o_1 <- length(o_x_1)
  o_x_2 <- c(o_x_1[n_o_1], o_x[!is.na(o_y) & o_x >= first_pred])
  o_y_2 <- c(o_y_1[n_o_1], o_y[!is.na(o_y) & o_x >= first_pred])
  points(o_x_1, o_y_1, type = "l", lwd = 2)
  points(o_x_2, o_y_2, type = "l", lwd = 2)

  model_name <- ifnull(model, casts_meta$model)
  data_set_name <- gsub("_interp", " (interpolated)", data_set)
  title <- paste0(model_name, ", ", data_set_name)
  mtext(title, side = 3, cex = 1.25, line = 0.5, at = 217, adj = 0)

}






#' @title Combine (ensemble) casts
#'
#' @description Combine multiple casts' output into a single ensemble. 
#'  Presently, only a general average ensemble is available.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), 
#'  the table will be efficiently (as defined by the inputs) loaded and 
#'  trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input,
#'  otherwise, all relevant casts from the stated \code{cast_groups}
#'  will be included. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param method \code{character} value of the name of the ensemble method
#'  to use. Presently, only \code{"unwtavg"} (unweighted average) is allowed.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value
#'  of the cast group to combine with an ensemble. If \code{NULL} (default),
#'  the most recent cast group is ensembled. 
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values 
#'  representing the casts of interest for restricting ensembling, as indexed
#'  within the directory in the \code{casts} sub folder. 
#'  See the casts metadata file (\code{casts_metadata.csv}) for summary
#'  information.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number of the forecast origin. Default value is 
#'  \code{NULL}, which equates to no selection with respect to 
#'  \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not
#'  input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to 
#'  include. Default value is \code{NULL}, which equates to no selection with 
#'  respect to \code{model}. \code{NULL} translates to all \code{models}
#'  in the table.
#'
#' @param data_set \code{character} value of the rodent data set to include
#'  Default value is \code{NULL}, which equates to the first data set 
#'  encountered.
#'
#' @param include_interp \code{logical} indicator of if the basic data set
#'  names should also be inclusive of the associated interpolated data sets.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param species \code{character} vector of the species code(s) 
#'  or \code{"total"} for the total across species) to be plotted 
#'  \code{NULL} translates to the species defined by  
#'  \code{evalplot_species}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "ESSS"), end_moon = 515)
#'   ensemble_casts()
#' }
#'
#' @export
#'
ensemble_casts <- function(main = ".", method = "unwtavg", 
                           cast_groups = NULL, cast_ids = NULL, 
                           cast_tab = NULL, end_moon = NULL, 
                           models = NULL, data_set = NULL, 
                           include_interp = TRUE,
                           species = NULL, arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  if(is.null(cast_tab)){
    cast_choices <- select_casts(main = main, cast_ids = cast_ids, 
                                 models = models, end_moons = end_moon, 
                                 data_sets = data_set, 
                                 include_interp = include_interp,
                                 arg_checks = arg_checks)
    if(NROW(cast_choices) == 0){
      stop("no casts available for request", call. = FALSE)
    }else{
      cast_tab <- read_cast_tabs(main = main, cast_ids = cast_choices$cast_id,
                                 arg_checks = arg_checks)
      cast_tab <- add_obs_to_cast_tab(main = main, cast_tab = cast_tab,
                                      arg_checks = arg_checks)
      cast_tab <- add_err_to_cast_tab(main = main, cast_tab = cast_tab,
                                      arg_checks = arg_checks)
      cast_tab <- add_lead_to_cast_tab(main = main, cast_tab = cast_tab,
                                       arg_checks = arg_checks)
      cast_tab <- add_covered_to_cast_tab(main = main, cast_tab = cast_tab,
                                          arg_checks = arg_checks)
    }
  }
  cast_tab$data_set <- gsub("_interp", "", cast_tab$data_set)
  cast_ids <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models <- ifnull(models, unique(cast_tab$model))
  data_set <- ifnull(data_set, "controls")
  species <- ifnull(species, 
                    base_species(total = TRUE, arg_checks = arg_checks)) 
  end_moon <- ifnull(end_moon, max(unique(na.omit(cast_tab)$end_moon))) 
  cast_groups <- ifnull(cast_groups, unique(cast_tab$cast_group))
  cast_id_in <- cast_tab$cast_id %in% cast_ids
  model_in <- cast_tab$model %in% models
  data_set_in <- cast_tab$data_set == data_set
  species_in <- cast_tab$species %in% species
  end_moon_in <- cast_tab$end_moon %in% end_moon
  cast_group_in <- cast_tab$cast_group %in% cast_groups
  all_in <- cast_id_in & model_in & data_set_in & species_in & end_moon_in &
            cast_group_in
  if(sum(all_in) == 0){
    stop("no casts available for requested", call. = FALSE)
  }
  cast_tab <- cast_tab[all_in, ]
  nspecies <- length(species)
  moons <- unique(cast_tab$moon)
  nmoons <- length(moons)
  nmodels <- length(models)
  weight <- 1/nmodels
  estimate <- rep(NA, nspecies * nmoons)
  mvar <- rep(NA, nspecies * nmoons)
  l_pi <- rep(NA, nspecies * nmoons)
  u_pi <- rep(NA, nspecies * nmoons)
  obs <- rep(NA, nspecies * nmoons)
  error <- rep(NA, nspecies * nmoons)
  end_moon_id <- rep(NA, nspecies * nmoons)
  ecast_id <- rep(NA, nspecies * nmoons)
  species_id <- rep(NA, nspecies * nmoons)
  moon_id <- rep(NA, nspecies * nmoons)
  covered <- rep(NA, nspecies * nmoons)
  nmodels <- length(models)
  counter <- 1
  for(i in 1:nspecies){
    for(j in 1:nmoons){
      species_in <- cast_tab$species %in% species[i]
      moon_in <- cast_tab$moon %in% moons[j]
      all_in <- species_in & moon_in
      pcast_tab <- cast_tab[all_in, ]
  
      estimates <- na.omit(pcast_tab$estimate)
      if(length(estimates) > 0){
        mvars <- ((na.omit(pcast_tab$upper_pi) - na.omit(pcast_tab$estimate))/
                  (na.omit(pcast_tab$confidence_level))) ^2
        estimate[counter] <- sum(estimates * weight)
        wt_ss <- sum(weight * (estimates - estimate[counter]) ^ 2)
        if(nmodels > 1){
          mvar[counter] <- sum((mvars * weight) + (wt_ss / (nmodels - 1)))
        }else{
          mvar[counter] <- mvars
        }
        CL <- unique(na.omit(pcast_tab$confidence_level))
        u_pi[counter] <- estimate[counter] + sqrt(mvar[counter] * CL)
        l_pi[counter] <- estimate[counter] - sqrt(mvar[counter] * CL)
        obs[counter] <- unique(pcast_tab$obs)
        error[counter] <- estimate[counter] - obs[counter]
        end_moon_id[counter] <- unique(pcast_tab$end_moon)
        ecast_id[counter] <- as.numeric(paste0(9999, min(pcast_tab$cast_id)))
        moon_id[counter] <- moons[j]
        species_id[counter] <- species[i]
        covered[counter] <- estimate[counter] >= l_pi[counter] &
                            estimate[counter] <= u_pi[counter]
      }
      counter <- counter + 1
      
    }  
  }

  ensemble_name <- paste0("ensemble_", method)

  lead <- moon_id - end_moon_id
  data.frame(model = ensemble_name, moon = moon_id, species = species_id,
             estimate = estimate, var = mvar, lower_pi = l_pi, 
             upper_pi = u_pi, obs = obs, error = error, 
             end_moon = end_moon_id, lead = lead, data_set = data_set,
             cast_id = ecast_id, covered = covered, stringsAsFactors = FALSE) 
}


#' @title Read in a data file and format it for specific class
#'
#' @description Read in a specified data file. \cr \cr
#'  Current options include \code{"rodents"} (produces a list), 
#'  \code{"rodents_table"} (produces a rodents table), \code{"covariates"},
#'  \code{"covariate_casts"}, \code{"moons"}, and \code{"metadata"}, which
#'  are available as calls to \code{read_data} with a specified 
#'  \code{data_name} or as calls to the specific \code{read_<data_name>} 
#'  functions (like \code{read_moons}). \cr \cr
#'  If the requested data do not exist, an effort is made to prepare them
#'  using the associated \code{prep_<data_name>} functions (like
#'  \code{prep_moons}).\cr \cr
#'  \code{read_cov_casts} reads in the current or (if no current version)
#'  local archived version of the covariate casts.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'  
#' @param data_name \code{character} representation of the data needed.
#'  Current options include \code{"rodents"}, \code{"rodents_table"}, 
#'  \code{"covariates"}, \code{"covariate_forecasts"}, \code{"moons"}, and 
#'  \code{"metadata"}.
#'
#' @param data_set,data_sets \code{character} representation of the grouping
#'  name(s) used to define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}. \code{data_set} can only be length 1, 
#'  \code{data_sets} is not restricted in length.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or just tidy messages. 
#'  
#' @return Data requested.
#' 
#' @examples
#' \donttest{
#'  setup_dir()
#'  read_data(data_name = "rodents")
#'  read_data(data_name = "rodents_table")
#'  read_data(data_name = "rodents_table", data_set = "controls")
#'  read_data(data_name = "covariates")
#'  read_data(data_name = "covariate_casts")
#'  read_data(data_name = "moons")
#'  read_data(data_name = "metadata")
#'
#'  read_rodents()
#'  read_rodents_table()
#'  read_covariates()
#'  read_covariate_casts()
#'  read_moons()
#'  read_metadata()
#' }
#'
#' @export
#'
read_data <- function(main = ".", data_name = NULL, data_set = "all", 
                      data_sets = c("all", "controls"), 
                      control_files = files_control(), arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(data_name)
  data_name <- tolower(data_name)
  data_set <- tolower(data_set)
  data_sets <- tolower(data_sets)
  if (data_name == "rodents"){
    data <- read_rodents(main = main, data_sets = data_sets, 
                         arg_checks = arg_checks)
  }
  if (data_name == "rodents_table"){
    data <- read_rodents_table(main = main, data_set, arg_checks = arg_checks)
  }
  if (data_name == "covariates"){
    data <- read_covariates(main = main, control_files = control_files, 
                            arg_checks = arg_checks)
  }
  if (data_name == "covariate_casts"){
    data <- read_covariate_casts(main = main, control_files = control_files, 
                                 arg_checks = arg_checks)
  }
  if (data_name == "moons"){
    data <- read_moons(main = main, control_files = control_files, 
                       arg_checks = arg_checks)
  }
  if (data_name == "metadata"){
    data <- read_metadata(main = main, control_files = control_files, 
                          arg_checks = arg_checks)
  }
  data
}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function(main = ".", data_set = "all", 
                               arg_checks = TRUE){
  check_args(arg_checks)
  data_set <- tolower(data_set)
  lpath <- paste0("rodents_", data_set, ".csv") 
  fpath <- file_path(main = main, sub = "data", files = lpath, 
                     arg_checks = arg_checks)
  if(!file.exists(fpath)){
    rodents <- prep_rodents(main = main, data_sets = data_set, 
                            arg_checks = arg_checks)
    rodents_tab <- rodents[[1]]
    return(rodents_tab)
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function(main = ".", data_sets = c("all", "controls"), 
                         arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(data_sets)
  data_sets <- tolower(data_sets)
  ndata_sets <- length(data_sets)
  rodents <- vector("list", length = ndata_sets)
  for(i in 1:ndata_sets){
    rodents[[i]] <- read_rodents_table(main = main, data_set = data_sets[i], 
                                       arg_checks = arg_checks)
  }
  names(rodents) <- data_sets
  rodents
}


#' @rdname read_data
#'
#' @export
#'
read_covariates <- function(main = ".", control_files = files_control(),
                            arg_checks = TRUE){
  check_args(arg_checks)
  fpath <- file_path(main = main, sub = "data", 
                     files = control_files$filename_cov, 
                     arg_checks = arg_checks)
  if(!file.exists(fpath)){
    return(prep_covariates(main = main, arg_checks = arg_checks))
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_covariate_casts <- function(main = ".", control_files = files_control(),
                           quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  curr_path <- file_path(main = main, sub = "data", 
                         files = control_files$filename_cov_casts, 
                         arg_checks = arg_checks)
  curr_path2 <- gsub("covariate_casts", "covariate_forecasts", curr_path)

  arch_path <- paste0(control_files$directory, "/data/", 
                      control_files$filename_cov_casts)
  arch_path <- file_path(main = main, sub = "raw", files = arch_path,
                         arg_checks = arg_checks)
  arch_path2 <- gsub("covariate_casts", "covariate_forecasts", arch_path)

  if(file.exists(curr_path)){
    cov_cast <- read.csv(curr_path, stringsAsFactors = FALSE)
  } else if (file.exists(curr_path2)){
    cov_cast <- read.csv(curr_path2, stringsAsFactors = FALSE)
  } else {
    if(file.exists(arch_path)){
      cov_cast <- read.csv(arch_path, stringsAsFactors = FALSE)
    } else if (file.exists(arch_path2)){
      cov_cast <- read.csv(arch_path2, stringsAsFactors = FALSE)
    } else {
      msg <- "current and archive versions missing, run `fill_raw`"
      stop(msg, call. = FALSE)
    }
  }
  if(any(grepl("forecast_newmoon", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("forecast_newmoon", "cast_moon", 
                                colnames(cov_cast))
  }
  if(any(grepl("newmoonnumber", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("newmoonnumber", "moon", colnames(cov_cast))
  } 
  cov_cast
}

#' @rdname read_data
#'
#' @export
#'
read_moons <- function(main = ".", control_files = files_control(),
                       arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  fpath <- file_path(main = main, sub = "data", 
                     files = control_files$filename_moons, 
                     arg_checks = arg_checks)
  if(!file.exists(fpath)){
    return(prep_moons(main = main, arg_checks = arg_checks))
  }
  read.csv(fpath, stringsAsFactors = FALSE)
}

#' @rdname read_data
#'
#' @export
#'
read_metadata <- function(main = ".", control_files = files_control(),
                          arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  fpath <- file_path(main = main, sub = "data", 
                     files = control_files$filename_meta, 
                     arg_checks = arg_checks)
  if(!file.exists(fpath)){
    md <- prep_metadata(main = main, arg_checks = arg_checks)
    return(md)
  }
  yaml.load_file(fpath) 
}


#' @rdname directory_config
#'
#' @export
#'
update_config <- function(main = ".", 
                                   filename_config = "dir_config.yaml",
                                    downloads_versions = NULL, 
                                    quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  config_path <- file_path(main = main, files = filename_config)
  config <- read_directory_config(main = main, 
                                 filename_config = filename_config,
                                 quiet = quiet, arg_checks = arg_checks)
  if(!is.null(downloads_versions)){
    config$downloads_versions <- downloads_versions
  }
  yams <- as.yaml(config)
  writeLines(yams, con = config_path)  
  invisible(NULL)
}



#' @rdname directory_config
#'
#' @export
#'
read_config <- function(main = ".", 
                                  filename_config = "dir_config.yaml",
                                  quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  config_path <- file_path(main = main, files = filename_config,
                           arg_checks = arg_checks)
  if(!file.exists(config_path)){
    stop("dir_config.yaml file is missing, recreate directory", call. = FALSE)
  }
  yaml.load_file(config_path)  
}


#' @title Save data out to a csv, appending the file if it already exists
#'
#' @description Appending a \code{.csv} without re-writing the header of the
#'  file. If the doesn't exist, it will be created.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param filename \code{character} filename of existing \code{.csv} to be 
#'  appended.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'   checked using standard protocols via \code{\link{check_args}}. The 
#'   default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'   formatted correctly and provides directed error messages if not. \cr
#'   However, in sandboxing, it is often desirable to be able to deviate from 
#'   strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'   many/most/all enclosed functions to not check any arguments using 
#'   \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'
#' @return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   df <- data.frame(x = 1:10)
#'   fpath <- file_path(files = "xx.csv")
#'   append_csv(df, fpath)
#'  }
#'
#' @export
#'
append_csv <- function(df, filename, arg_checks = TRUE){
  check_args(arg_checks)
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
  NULL
}

#' @title Calculate the fraction of the year from a date
#' 
#' @description Based on the year in which the date occurred, determine the
#'   fraction of the year (foy) for the date (in relation to New Year's Eve
#'   in that year). 
#'
#' @param dates \code{Date}(s) or \code{Date}-conformable value(s) to be 
#'   converted to the fraction of the year.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'   checked using standard protocols via \code{\link{check_args}}. The 
#'   default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'   formatted correctly and provides directed error messages if not. \cr
#'   However, in sandboxing, it is often desirable to be able to deviate from 
#'   strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'   many/most/all enclosed functions to not check any arguments using 
#'   \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'
#' @return \code{numeric} value(s) of the fraction of the year.
#'
#' @examples
#'  foy(Sys.Date())
#'
#' @export
#'
foy <- function(dates = NULL, arg_checks = TRUE){
  return_if_null(dates)
  check_args(arg_checks)
  dates <- as.Date(dates)
  jday <- as.numeric(format(dates, "%j"))
  nye <- as.Date(paste0(format(dates, "%Y"), "-12-31"))
  nyejday <- as.numeric(format(nye, "%j"))
  round(jday / nyejday, 3)
}

#' @title Remove files from the tmp subdirectory
#'
#' @description Clear the files from the tmp subdirectory.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or just tidy messages. 
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  made or not. For toggling separately from the more general \code{quiet}
#'  argument. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{NULL}, with the tmp subdirectory's files removed.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   clear_tmp
#'  }
#'
#' @export
#'
clear_tmp <- function(main = ".", bline = TRUE, quiet = FALSE, 
                      verbose = FALSE, cleanup = TRUE, arg_checks = TRUE){
  check_args(arg_checks)
  tmp_path <- tmp_path(main = main, arg_checks = arg_checks)
  tmp_exist <- dir.exists(tmp_path)
  tmp_files <- list.files(tmp_path)
  ntmp_files <- length(tmp_files)
  if(!cleanup){
    return()
  }
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  messageq("Clearing tmp subdirectory", quiet)

  if(tmp_exist){
    if(ntmp_files > 0){
      tmp_files_full_paths <- file_path(main = main, sub = "tmp", 
                                        files = tmp_files, 
                                        arg_checks = arg_checks)
      unlink(tmp_files_full_paths, force = TRUE, recursive = TRUE)
      msg <- "    *temporary files cleared from tmp subdirectory*"
    } else {
      msg <- "    *tmp subdirectory already clear*"
    }
  } else{
    msg <- "    *tmp subdirectory not present for clearing*"
  }
  messageq(msg, !verbose)
  NULL
}



#' @title Combine a historical table and a cast table
#'
#' @description A simple utility for combining a table of historical data
#'  and a table of cast data that might need to be assigned to either one
#'  or the other.
#'
#' @param hist_tab,cast_tab A pair of \code{data.frame}s with the same columns
#'  including a code{date} column of \code{Date}s, which is used to align 
#'  them.
#'
#' @param winner \code{character} value either {"hist"} or \code{"cast"} to
#'  decide who wins any ties. In the typical portalcasting space, this is 
#'  kept at its default value throughout. In the case of \code{NA} values,
#'  this will be overriden to use the entry that has no missing entries.
#'
#' @param column \code{character} indicating the column to use for identifying
#'  entries in combining.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{data.frame} combining \code{hist_tab} and \code{cast_tab}.
#' 
#' @examples
#'  hist_tab <- data.frame(date = seq(Sys.Date(), Sys.Date() + 5, 1), x = 1:6)
#'  cast_tab <- data.frame(date = seq(Sys.Date() + 5, Sys.Date() + 10, 1),
#'                         x = 101:106)
#'  combine_hist_and_cast(hist_tab, cast_tab, "hist") 
#'  combine_hist_and_cast(hist_tab, cast_tab, "cast")  
#'
#' @export
#'
combine_hist_and_cast <- function(hist_tab = NULL, cast_tab = NULL, 
                                  winner = "hist", column = "date",
                                  arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(hist_tab, cast_tab)
  return_if_null(cast_tab, hist_tab)

  hist_tab$x_source <- "hist"
  cast_tab$x_source <- "cast"
  out <- rbind(hist_tab, cast_tab)
  in_out <- rep(TRUE, NROW(out))
  dupes <- names(which(table(out[,column]) > 1))

  ndupes <- length(dupes) 
  if(ndupes > 0){
    for(i in 1:ndupes){
      which_duped <- which(out$moon == dupes[i])

      which_duped_hist <- which(as.character(out[,column]) == dupes[i] &
                                out$x_source == "hist")
      which_duped_cast <- which(as.character(out[,column]) == dupes[i] &
                                out$x_source == "cast") 

      hist_dupe_NA <- any(is.na(out[which_duped_hist, ]))
      cast_dupe_NA <- any(is.na(out[which_duped_cast, ]))

      if(winner == "hist"){
        if(!hist_dupe_NA){
          in_out[which_duped_cast] <- FALSE
        } else{
          in_out[which_duped_hist] <- FALSE   
        }
      } else if(winner == "cast"){
        if(!cast_dupe_NA){
          in_out[which_duped_hist] <- FALSE
        } else{
          in_out[which_duped_cast] <- FALSE   
        }
      }
    }
  }
  out <- out[ , -which(colnames(out) == "x_source")]
  out[in_out, ]
}

#' @title Add a date to a table that has the year month and day as components 
#' 
#' @description Add a date (as a \code{Date}) column to a table that has the 
#'  year month and day as components.
#' 
#' @param df \code{data.frame} with columns named \code{year}, \code{month},
#'  and \code{day}. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'   checked using standard protocols via \code{\link{check_args}}. The 
#'   default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'   formatted correctly and provides directed error messages if not. \cr
#'   However, in sandboxing, it is often desirable to be able to deviate from 
#'   strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'   many/most/all enclosed functions to not check any arguments using 
#'   \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'
#' @return \code{data.frame} \code{df} with column of \code{Date}s 
#'  named \code{date} added.
#'
#' @examples
#'  df <- data.frame(year = 2010, month = 2, day = 1:10)
#'  add_date_from_components(df)
#'
#' @export
#'
add_date_from_components <- function(df, arg_checks = TRUE){
  check_args(arg_checks)
  yrs <- df$year
  mns <- df$month
  dys <- df$day
  df$date <- as.Date(paste(yrs, mns, dys, sep = "-"))
  df
}




#' @title Remove any specific incomplete entries as noted by an NA
#'
#' @description Remove any incomplete entries in a table, as determined by
#'  the presence of an \code{NA} entry in a specific column 
#'  (\code{colname}).
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param colname A single \code{character} value of the column to use
#'  to remove incomplete entries. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{df} without any incomplete entries. 
#'
#' @examples
#'  df <- data.frame(c1 = c(1:9, NA), c2 = 11:20)
#'  remove_incompletes(df, "c1")
#'
#' @export
#'
remove_incompletes <- function(df, colname, arg_checks = TRUE){
  check_args(arg_checks)
  incompletes <- which(is.na(df[ , colname]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}

#' @title Determine the depth of a list
#'
#' @description Evaluate an input for the depth of its nesting. 
#'
#' @details If \code{xlist = list()}, then technically the input value is a 
#'  list, but is empty (of length \code{0}), so depth is returned as \code{0}.
#'
#' @param xlist Focal input \code{list}.
#'
#' @return \code{integer} value of the depth of the list.
#' 
#' @examples
#'  list_depth("a")
#'  list_depth(list())
#'  list_depth(list("a"))
#'  list_depth(list(list("a")))
#'
#' @export 
#'
list_depth <- function(xlist){
  xx <- match.call()
  xxx <- deparse(xx[[2]])
  if(xxx == "list()"){
    0L
  } else if (is.list(xlist)){
    1L + max(sapply(xlist, list_depth))
  } else {
    0L
  }
}


#' @title Cast the covariates
#'
#' @description Cast the covariates for a model run. \cr \cr
#'  \code{prep_cast_covariates} is the primary function, which produces the 
#'  covariates (min, mean, and max temperature; precipitation; and NDVI) 
#'  required for a model to be cast. Saves the cast data out via
#'  \code{save_cast_cov_csv} and tidying the data for use within the model 
#'  (by removing the vestigial columns that are only needed for saving). 
#'
#' @param main \code{character} name of the main directory tree component.
#'
#' @param moons Lunar data \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param hist_cov \code{data.frame} of historic covariates. See 
#'  \code{\link{prep_hist_covariates}}.
#'
#' @param cast_cov \code{data.frame} of cast covariates. See
#'  \code{\link{cast_covariates}}.
#'
#' @param lead_time \code{integer}-conformable number of timesteps forward 
#'  a cast will proceed.
#'
#' @param min_lag \code{integer}-conformable minimum non-0 covariate lag time 
#'  used in any model.
#'
#' @param cast_date \code{Date} on which the cast happens. Generally should
#'  be \code{\link{Sys.Date}}.
#'
#' @param end_moon Forecast origin. \code{integer}-conformable newmoon number 
#'  or \code{NULL}, which equates to the most recently included census'
#'  newmoon number. 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_climate_dl \code{list} of specifications for the climate 
#'  download. Sent to \code{\link{NMME_urls}} to create the specific URLs. See 
#'  \code{\link{climate_dl_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @details If forecasting from the current newmoon, casts the covariates 
#'  If casting from a previous newmoon, the
#'  historical forecasted weather and NDVI data are retrieved from the file 
#'  pointed to by \code{filename_cov_casts}.
#'
#' @return 
#'  \code{prep_cast_covariates}: \code{data.frame} of the cast covariates
#'   after saving, with excess columns removed. \cr \cr
#'  \code{save_cast_cov_csv}: \code{cast_cov} \code{data.frame} exactly as 
#'   input. 
#'  
#' @examples
#'  \donttest{
#'   setup_dir()
#'   hist_cov <- prep_hist_covariates()
#'   prep_cast_covariates(hist_cov = hist_cov)
#'   save_cast_cov_csv(cast_cov = cast_cov)
#'  }
#'
#' @name cast_covariates
#'

#' @rdname cast_covariates
#'
#' @export
#'
prep_cast_covariates <- function(main = ".", moons = NULL,
                                 hist_cov = NULL, end_moon = NULL, 
                                 lead_time = 12, min_lag = 6, 
                                 cast_date = Sys.Date(), 
                                 control_files = files_control(),
                                 control_climate_dl = climate_dl_control(),
                                 quiet = TRUE, verbose = FALSE, 
                                 arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  last_moon <- last_moon(main = main, moons = moons, date = cast_date, 
                         arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  min_lag <- ifna(min_lag, 0)
  hist_cov <- ifnull(hist_cov, prep_hist_covariates(main = main,
                                                    quiet = quiet,
                                                    arg_checks = arg_checks)) 
  if(last_moon == end_moon){

    win <- cast_window(main = main, moons = moons, cast_date = cast_date,
                       lead_time = lead_time, min_lag = 6,
                       arg_checks = arg_checks)
    control_climate_dl <- do.call(climate_dl_control, control_climate_dl)
    control_climate_dl <- update_list(control_climate_dl, start = win$start, 
                                     end = win$end)
    download_climate_casts(main = main, 
                           control_climate_dl = control_climate_dl, 
                           quiet = quiet, verbose = verbose, 
                           arg_checks = arg_checks)

    cast_cov <- read_climate_casts(main = main, 
                                   control_climate_dl = control_climate_dl, 
                                   arg_checks = arg_checks)
    win_vec <- seq(win$start, win$end, 1)
    win_length <- length(win_vec)
    ndvi_fit <- auto.arima(hist_cov$ndvi)
    ndvi_cast <- forecast(ndvi_fit, h = win_length)
    ndvi_cast <- as.numeric(ndvi_cast$mean)


    spots <- na.omit(match(win_vec, cast_cov$date))
    incl <- win_vec %in% cast_cov$date
    cast_cov$ndvi[spots] <- ndvi_cast[incl]

    cast_cov <- add_moons_from_date(df = cast_cov, moons = moons, 
                                   arg_checks = arg_checks)
    cast_cov_tab <- summarize_daily_weather_by_moon(cast_cov)
    cast_cov$cast_moon <- end_moon

    cast_moon <- end_moon
    covariates_tab <- data.frame(cast_moon, cast_cov_tab)
    lagged_lead <- lead_time - min_lag

    out <- save_cast_cov_csv(main = main, moons = moons, end_moon = end_moon, 
                           cast_date = cast_date, cast_cov = cast_cov,
                           control_files = control_files,
                           quiet = quiet, verbose = verbose,
                           arg_checks = arg_checks)
    
    out <- covariates_tab[1:lagged_lead, ]


  } else {

    target_moons <- target_moons(main = main, moons = moons,
                                 end_moon = end_moon, lead_time = lead_time, 
                                 date = cast_date, arg_checks = arg_checks)
    cov_cast <- read_covariate_casts(main = main, 
                                     control_files = control_files,
                                     quiet = quiet, verbose = verbose,
                                     arg_checks = arg_checks)

    target_in <- cov_cast$moon %in% target_moons
    origin_in <- cov_cast$cast_moon %in% end_moon 
    cov_cast2 <- cov_cast[which(target_in & origin_in), ]

    if(NROW(cov_cast2) == 0){
      stop("covariates not available for requested end moon and lead time",
           call. = FALSE)
    }
    most_recent <- max(cov_cast2$date_made)
    made_in <- cov_cast2$date_made == most_recent
    out <- cov_cast2[which(made_in), ]
    out$date_made <- NULL
    out$date <- NULL
  }

  out$cast_moon <- NULL
  out$source <- "cast"
  out
}




#' @rdname cast_covariates
#'
#' @export
#'
save_cast_cov_csv <- function(main = ".", moons = NULL,
                              end_moon = NULL, cast_date = Sys.Date(),
                              cast_cov = NULL, 
                              control_files = files_control(),
                              quiet = TRUE, verbose = FALSE, 
                              arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  return_if_null(cast_cov)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                         arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  if(!control_files$save | !control_files$append_cast_csv | 
     !control_files$overwrite | end_moon != last_moon){
    return(cast_cov)
  }
  new_cast <- cast_cov
  new_cast$source <- control_files$source_name
  date_made <- Sys.time()
  tz <- format(date_made, "%Z")
  new_cast$date_made <- paste0(date_made, " ", tz)

  hist_cast <- read_covariate_casts(main = main, 
                                    control_files = control_files,
                                    quiet = quiet, verbose = verbose, 
                                    arg_checks = arg_checks)
  if(!("date" %in% colnames(hist_cast))){
    hist_cast$date <- NA
  }
  
  out <- rbind(hist_cast, new_cast)
  out$date <- as.character(out$date)
  out_path <- file_path(main = main, sub = "data", 
                        files = control_files$filename_cov_casts,
                        arg_checks = arg_checks)
  msg <- "    **covariates_casts.csv saved**"
  messageq(msg, !verbose)
  write.csv(out, out_path, row.names = FALSE)
  cast_cov
}


#  \code{read_climate_casts}: reads the downloaded files into R and does
#  some minimal tidying: makes sure \code{-9999}s become \code{NA}s, 
#  makes sure the \code{date} column is named as such and is actually a 
#  \code{Date}, and converts the temperatures to C and the precipitation
#  to mm. \cr \cr

read_climate_casts <- function(main = ".", 
                               control_climate_dl = climate_dl_control(), 
                               arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  control_climate_dl <- do.call(climate_dl_control, control_climate_dl)
  urls <- do.call(NMME_urls, control_climate_dl)
  return_if_null(urls)

  dat_list <- vector("list", length(control_climate_dl$data))
  ndatas <- length(control_climate_dl$data)
  for(i in 1:ndatas){
    csv_path <- paste0("cov_casts/",  names(urls)[i], ".csv")
    fpath <- file_path(main = main, sub = "raw", files = csv_path, 
                       arg_checks = arg_checks)
    dat_list[[i]] <- read.csv(fpath)
  }
  dat_tab <- dat_list[[1]]
  colnames(dat_tab)[ncol(dat_tab)] <- names(control_climate_dl$data)[1]
  colnames(dat_tab)[1] <- "date"
  dat_tab[,1] <- as.Date(dat_tab[,1])
  dat_tab <- dat_tab[ , c(1, ncol(dat_tab))]
  
  if(ndatas > 1){
    for(i in 2:ndatas){
      dat_tab_i <- dat_list[[i]]
      x <- dat_tab_i[ , ncol(dat_tab_i)]
      dat_tab <- data.frame(dat_tab, x)
      colnames(dat_tab)[ncol(dat_tab)] <- names(control_climate_dl$data)[i]
    }
  }
  for(i in 2:ncol(dat_tab)){
    if(grepl("temp", colnames(dat_tab)[i])){
      x <- dat_tab[ , i]
      x[x == -9999] <- NA
      dat_tab[ , i] <- (x - 32) * 5 / 9      
    } else if (grepl("precip", colnames(dat_tab)[i])){
      x <- dat_tab[ , i]
      x[x == -9999] <- NA
      x[x < 0] <- 0
      dat_tab[ , i] <- x * 25.4
    }
  }
  dat_tab
}


#' @title Replace a value with an alternative if it is NULL or if it is NA
#'
#' @description 
#'  \code{ifnull} replaces the focal input with the alternative value if it
#'   is \code{NULL}. \cr \cr
#'  \code{ifna} replaces the focal input with the alternative value if it
#'   is \code{NA}.
#'
#' @param x Focal input.
#'
#' @param alt Alternative value.
#'
#' @return 
#'  \code{ifnull}: \code{x} if not \code{NULL}, \code{alt} otherwise. \cr \cr
#'  \code{ifna}:  \code{x} if not \code{NA}, \code{alt} otherwise. 
#' 
#' @examples
#'  ifnull(NULL, 123)
#'  ifnull(TRUE, 123)
#'  ifnull(FALSE, 123)
#'  ifna(NA, 123)
#'  ifna(FALSE, 123)
#'  ifna(NA, NA)
#'
#' @name alternative_values
#'
NULL

#' @rdname alternative_values
#'
#' @export 
#'
ifnull <- function(x = NULL, alt = NULL){
  if(is.null(x)){
    x <- alt
  }
  x
}

#' @rdname alternative_values
#'
#' @export 
#'
ifna <- function(x = NULL, alt = NA){
  ifelse(is.na(x), alt, x)
}



#' @title Conform NA entries to "NA" entries
#'
#' @description Given the species abbreviation NA, when data are read in, 
#'  there can be an \code{NA} when it should be an \code{"NA"}. This function
#'  conforms the entries to be proper character values. 
#'
#' @param dfv Either [1] a \code{data.frame} containing \code{colname} as a 
#'  column with \code{NA}s that need to be conformed to \code{"NA"}s or [2]
#'  a vector with \code{NA}s that need to be conformed to \code{"NA"}s.
#'
#' @param colname \code{character} value of the column name in \code{tab} to 
#'  conform the \code{NA}s to \code{"NA"}s.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'   checked using standard protocols via \code{\link{check_args}}. The 
#'   default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'   formatted correctly and provides directed error messages if not. \cr
#'   However, in sandboxing, it is often desirable to be able to deviate from 
#'   strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'   many/most/all enclosed functions to not check any arguments using 
#'   \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'
#' @return \code{x} with any \code{NA} in \code{colname} replaced with 
#'  \code{"NA"}.
#'
#' @examples
#'  na_conformer(c("a", "b", NA, "c"))
#'
#' @export
#'
na_conformer <- function(dfv, colname = "species", arg_checks = TRUE){
  check_args(arg_checks)
  if (is.vector(dfv)){
    naentries <- which(is.na(dfv))
    dfv[naentries] <- "NA"
  } else if (is.data.frame(dfv)){
    nasppname <- which(is.na(dfv[ , colname]))
    if (length(nasppname) > 0){
      dfv[nasppname, colname] <- "NA"
    }
  } 
  dfv
}




#' @title Argument matching with defaults
#'
#' @description Expansion of \code{\link[base]{match.call}} to include
#'  default formal values.
#'
#' @param definition A \code{function}, by default the function from which 
#'  \code{match.call.defaults} is called. 
#'
#' @param call An unevaluated \code{call} to the function specified by 
#'  \code{definition}, as generated by \code{\link[base]{call}}.
#'
#' @param expand.dots \code{logical} defining if arguments matching 
#'  \code{...} in the call be included or left as a \code{...} argument
#'
#' @param envir An \code{environment}, from which the \code{...} in 
#'  \code{call} are retrieved, if any.
#'
#' @references 
#'  DesignLibrary's \code{match.call.defaults} function. \cr
#'  Stack overflow post reply by Roland. \href{https://bit.ly/2PtEgy1}{URL}
#'
#' @examples
#'  fun <- function(arg1 = "ok", ...) {
#'    match.call.defaults()
#'  }
#'  fun()
#'  fun(arg2 = "hi")
#'
#' @export
#'
match.call.defaults <- function(definition = sys.function(sys.parent()), 
                                call = sys.call(sys.parent()), 
                                expand.dots = TRUE, envir = parent.frame(2L)){
  call <- match.call(definition = definition, call = call, 
                     expand.dots = expand.dots, envir = envir)
  formals <- formals(fun = definition)
  if (expand.dots && "..." %in% names(formals)){
      formals[["..."]] <- NULL
  }
  diffs <- setdiff(names(formals), names(call))
  for (i in diffs){
    call[i] <- list(formals[[i]])
  }
  match.call(definition = definition, call = call, expand.dots = TRUE, 
             envir = envir)
}
#' @title Create a named empty list
#'
#' @description Produces a list with \code{NULL} for each element named 
#'  according to \code{element_names}.
#' 
#' @param element_names \code{character} vector of names for the elements
#'  in the list.
#'
#' @return \code{list} with names \code{element_names} and values \code{NULL}.
#'
#' @examples
#'  named_null_list(c("a", "b", "c"))
#'
#' @export
#'
named_null_list <- function(element_names = NULL){
  return_if_null(element_names)
  nelements <- length(element_names)
  out <- vector("list", nelements)
  names(out) <- element_names
  out
}

#' @title Error if a function's request is deeper than can be handled
#'
#' @description Produces an informative error message when a function 
#'  that should only be called inside of other functions is called outside
#'  of a function (hence the request to the function is too deep for
#'  what it can handle).
#' 
#' @param lev The number of frames back in the stack where the request needs
#'  to be able to be evaluated.
#'
#' @return Throws an error if the function is called in a place where it 
#'  cannot operate and returns \code{NULL} otherwise.
#'
#' @examples
#'  \dontrun{
#'  # will error:
#'  # error_if_deep(-10)
#'  }
#'  error_if_deep(0)
#'
#' @export
#'
error_if_deep <- function(lev){
  lev2 <- lev - 1
  too_deep <- tryCatch(sys.call(lev2), error = function(x){NA})
  if(!is.null(too_deep) && !is.call(too_deep) && is.na(too_deep)){
    msg <- "too deep; function should only be called inside other functions"
    stop(msg, call. = FALSE)
  } 
}



#' @title Create a control list for files
#'
#' @description Most users will not want or need to change data file and 
#'  folder names or saving conditions, but it is helpful to have them be 
#'  flexible for certain circumstances, and this function gathers them into a
#'  list for higher-in-the-pipeline functions.
#'
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'  data directory. A standard portalcasting directory downloads the raw data
#'  files into from the PortalData repository, so 
#'  \code{raw_data = "PortalData"}.
#'
#' @param filename_moons \code{character} name of the file for saving the 
#'  moons data.
#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param cleanup \code{logical} indicator of whether or not the tmp files
#'  should be cleaned up.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts later use.
#'
#' @param source_name \code{character} value for the name to give the 
#'  covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
#'
#' @return Named \code{list} of names of the folders and files within the
#'  directory structure as well as saving strategies (directly as input).
#'
#' @export
#'
files_control <- function(directory = "portalPredictions",
                          raw_data = "PortalData",
                          filename_moons = "moon_dates.csv",
                          filename_config = "dir_config.yaml", 
                          filename_cov = "covariates.csv", 
                          filename_cov_casts = "covariate_casts.csv",
                          filename_meta = "metadata.yaml", 
                          save = TRUE, overwrite = TRUE, cleanup = TRUE,
                          source_name = "current_archive",
                          append_cast_csv = TRUE, 
                          arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  list(directory = directory, raw_data = raw_data, 
       filename_moons = filename_moons, filename_cov = filename_cov,
       filename_cov_casts = filename_cov_casts,
       filename_config = filename_config, filename_meta = filename_meta,
       save = save, overwrite = overwrite, cleanup = cleanup,
       source_name = source_name, append_cast_csv = append_cast_csv)

}

#' @title Provide the names of the prefab data sets
#'
#' @description Create a \code{character} vector of the names of the rodents
#'  data sets to be included including the pre-fabricated (prefab) data sets,
#'  extracting them from the \code{prefab_rodents_controls} internal 
#'  function. \cr \cr
#'  The currently prefabricated options include (\code{"all"}, 
#'  \code{"controls"}, \code{"all_interp"}, and \code{"controls_interp"}). 
#'
#' @param interpolate \code{logical} value indicating if the interpolated data
#'  only should be listed (\code{interpolate = TRUE}), if the non-interpolated
#'  data only should be listed (\code{interpolate = FALSE}), or if both
#'  should be listed (\code{interpolate = NULL}, default).  
#'
#' @return \code{character} vector of data set names.
#'
#' @examples
#'  prefab_datasets()
#'
#' @export
#'
prefab_datasetsff <- function(interpolate = NULL){
  dsnames <- names(prefab_rodents_controls())
  return_if_null(interpolate, dsnames)
  if(interpolate){
    dsnames <- dsnames[grepl("_interp", dsnames)]
  }else{
    dsnames <- dsnames[!grepl("_interp", dsnames)]
  }
  dsnames
}


#' @title Determine the most recent data collection
#'
#' @description Determine the most recent census.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{Date} of the last census.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   last_census()
#'  }
#'
#' @export
#'
last_census <- function(main = ".", 
                        control_files = files_control(), arg_checks = TRUE){
  check_args(arg_checks)
  moons <- read_moons(main = main, control_files = control_files,
                      arg_checks = arg_checks)
  as.Date(max(moons$censusdate, na.rm = TRUE))
}

#' @title Select the most abundant species from a data set
#'
#' @description Not including the total, determine the most abundant 
#'  rodent species within a data set.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param dataset \code{character} representation of the grouping
#'  name used to define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @param topx Positive \code{integer} (or \code{integer}-conformable) 
#'  value of how many species to include.
#'  
#' @return \code{character} vector of the species identifiers.
#' 
#' @examples
#' \donttest{
#'  setup_dir()
#'  most_abundant_species()
#' }
#'
#' @export
#'
most_abundant_species <- function(main = ".", dataset = "all", topx = 3,
                                  arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  rodents_tab <- read_rodents_table(main = main, dataset = dataset, 
                                    arg_checks = arg_checks)
  col_keep <- which(colnames(rodents_tab) %in% all_species())
  rodents_tab <- rodents_tab[ , col_keep]
  tots <- apply(rodents_tab, 2, sum, na.rm = TRUE)
  tots_order <- order(tots)
  names(tots) <- colnames(rodents_tab)
  names(sort(tots, decreasing = TRUE)[1:topx])
}

#' @rdname fill_directory
#'
#' @export
#'
fill_datax <- function(main = ".", models = prefab_models(),
                      end_moon = NULL, start_moon = 217, lead_time = 12,
                      confidence_level = 0.95, cast_date = Sys.Date(), 
                      controls_model = NULL,
                      controls_rodents = rodents_controls(), 
                      control_climate_dl = climate_dl_control(),
                      control_files = files_control(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)

  min_lag <- extract_min_lag(models = models, 
                             controls_model = controls_model, 
                             quiet = quiet, arg_checks = arg_checks)
  datasets <- extract_datasets(models = models, 
                                 controls_model = controls_model, 
                                 quiet = quiet, arg_checks = arg_checks)

  fill_raw(main = main, downloads = downloads, only_if_missing = TRUE, 
           quiet = quiet, control_files = control_files, 
           arg_checks = arg_checks)

  messageq(" -Adding data files to data subdirectory", quiet)
  data_m <- prep_moons(main = main, lead_time = lead_time, 
                       cast_date = cast_date, 
                       quiet = quiet, verbose = verbose,
                       control_files = control_files, 
                       arg_checks = arg_checks)
  data_r <- prep_rodents(main = main, moons = data_m, 
                         datasets = datasets, end_moon = end_moon, 
                         controls_rodents = controls_rodents,
                         quiet = quiet, verbose = verbose, 
                         control_files = control_files, 
                         arg_checks = arg_checks)
  data_c <- prep_covariates(main = main, moons = data_m, end_moon = end_moon, 
                            start_moon = start_moon, lead_time = lead_time, 
                            min_lag = min_lag, cast_date = cast_date, 
                            control_climate_dl = control_climate_dl,
                            quiet = quiet, control_files = control_files,
                            arg_checks = arg_checks)
  prep_metadata(main = main, models = models,
                datasets = datasets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_model = controls_model,
                controls_rodents = controls_rodents, quiet = quiet, 
                control_files = control_files, arg_checks = arg_checks)

  invisible(NULL)
}



ff <- function(){

  if(time == "newmoon"){
    newmoon_col <- which(colnames(rodents_tab) == "newmoonnumber")
    colnames(rodents_tab)[newmoon_col] <- "moon" 
    if(!na_drop){ 
      present_moons <- rodents_tab[ , "moon"]
      range_moons <- range(present_moons, na.rm = TRUE)
      seq_moons <- seq(range_moons[1], range_moons[2], by = 1)
      needed_moons <- !(seq_moons %in% present_moons)
      which_needed_moons <- seq_moons[needed_moons]
      nneeded_rows <- length(which_needed_moons)
      ndf_moons <- ncol(rodents_tab)
      needed_rows <- matrix(NA, nrow = nneeded_rows, ncol = ndf_moons)
      colnames(needed_rows) <- colnames(rodents_tab)
      needed_rows[ , "moon"] <- seq_moons[needed_moons]
      rodents_tab <- rbind(rodents_tab, needed_rows)
      rodents_tab_ord <- order(rodents_tab[ , "moon"])
      rodents_tab <- rodents_tab[rodents_tab_ord, ]
    }
    if(interpolate){
      for(i in 1:nspecies){
        interped <- round(na.interp(rodents_tab[ , species[i]]))
        rodents_tab[ , species[i]] <- interped

      }
      rodents_tab[ , "total"] <- apply(rodents_tab[ , species, drop = FALSE],
                                       1, sum)
    }


    end_moon <- ifnull(end_moon, max(rodents_tab$moon))
    last_moon_in <- min(c(end_moon, max(rodents_tab$moon)))
    rows_in <- rodents_tab$moon <= last_moon_in
    rodents_tab <- rodents_tab[rows_in, ]
    messageq("    data trimmed according to moon window", !verbose)
  } else{
    messageq("    no time processing conducted", !verbose)
  }
  rodents_tab
}

#' @title Create control lists for generating rodents data tables
#'
#' @description Given the number of arguments into 
#'  \code{\link[portalr]{summarize_rodent_data}} via 
#'  \code{\link{prep_rodents_table}}, it helps to have a control \code{list}  
#'  to organize them. \cr \cr
#'  \code{rodents_controls} produces the \code{list}s for any user-requested
#'  datasets from the prefab datasets (\code{\link{prefab_data_sets}}) and any 
#'  user-defined sets. \code{\link{rodents_control}} provides a template
#'  for the controls \code{list} which can be used to define the specific 
#'  arguments for a user's novel data set. 
#'
#' @details Any data set that is part of the \code{prefab} rodents data sets 
#'  (\code{c("all", "controls", "exclosures", "all_interp", 
#'           "controls_interp", "exclosures_interp", "dm_controls")}) has its 
#'  controls already included internally via the non-exported function
#'  \code{prefab_rodents_controls}. Users only need to include controls for
#'  non-prefab \code{data_sets}. \cr \cr
#'  Any user-defined \code{data_sets} that are not included in 
#'  \code{controls_rodents} will throw an error. \cr \cr 
#'  If any user-defined \code{controls_rodents} duplicate any existing 
#'  controls for the prefab data sets or if \code{controls_r} contains any 
#'  duplicate named elements, an error will be thrown. \cr \cr
#'  Users interested in adding data sets to the prefab rodent data sets
#'  should add the controls to the \code{prefab_rodents_controls} 
#'  non-exported function found in the \code{prepare_rodents.R} script.
#
#' @param data_sets \code{character} value(s) of the rodent data set name(s) 
#'  used to enforce certain arguments. Currently available prefab data
#'  sets are \code{"all"}, \code{"all_interp"}, \code{"controls"}, 
#'  \code{"controls_interp"}, \code{"exclosures_interp"}, 
#'  \code{"exclosures"} and \code{"dm_controls"}.  
#'
#' @param controls_rodents Additional controls for datasets not in the 
#'  prefab set. \cr
#'  A \code{list} of a single dataset's controls or a \code{list} of 
#'  \code{list}s, each of which is a single dataset's controls. \cr \cr
#'  Presently, each dataset's controls should include 18 elements, as 
#'  our \code{\link{rodents_control}} template shows:
#'  \itemize{
#'   \item \code{name}: \code{character} value of the data set's name,
#'   \item \code{species}: \code{character}-valued vector of species names 
#'   to include.  
#'   \item \code{total}: \code{logical} value indicating if a total 
#'   (sum across species) should be added or not. Only available if more than 
#'   one species is included. 
#'   \item \code{interpolate}: \code{logical} value indicating if the  
#'   \code{NA} values should be interpolated.
#'   \item \code{clean}: \code{logical} indicator of if only the rodent data
#'    that passed QA/QC (\code{clean = TRUE}) or if all data 
#'    (\code{clean = FALSE}) should be loaded.
#'   \item \code{type}: \code{character} value of the rodent data set type, 
#'    according to pre-existing definitions. An alternative toggle to 
#'    \code{species}. \cr
#'    Either all species 
#'    (\code{type = "Rodents"}) or only granivoes 
#'    (\code{type = "Granivores"}).
#'   \item \code{level}: \code{character} indicating the type of summary:
#'    \code{"Plot"}, \code{"Treatment"}, or \code{"Site"}. Pipes 
#'    directly to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{plots}: Specification of subset of plots. Can be a vector of 
#'    \code{numeric} plots indicators or specific sets indicated by
#'    \code{character} values: \code{"all"} plots or \code{"Longterm"} plots
#'    (plots that have had the same treatment for the entire time series).
#'   \item \code{treatment}: \code{character} indicating the specific 
#'    treatment(s) to trim to if \code{level = "Treatment"}: \code{"control"},
#'    \code{"exclosure"}, \code{"removal"}, or \code{"spectabs"} 
#'   \item \code{min_plots}: \code{integer} (or integer \code{numeric}) of the 
#'    minimum number of plots surveyed for a survey to be used. Pipes 
#'    directly to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{min_traps}: \code{integer} (or integer \code{numeric}) of the 
#'    minimum number of traps collected for a plot to be used. Pipes directly
#'    to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{output}: \code{character} indicating the type of data:
#'    \code{"abundance"}, \code{"biomass"}, or \code{"energy"}. Pipes 
#'    directly to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{fillweight}: \code{logical} specifier of whether to fill in
#'    unknown weights with other records from that individual or species,
#'    where possible.
#'   \item \code{unknowns}: \code{logical} indicator to either remove all 
#'    individuals not identified to species (\code{unknowns = FALSE}) or 
#'    sum them in an additional column (\code{unknowns = TRUE}).
#'   \item \code{time}: \code{character} value specifying the format of the 
#'    time index in the output. Options are \code{"period"} (sequential 
#'    Portal surveys), \code{"newmoon"} (lunar cycle numbering), and 
#'    \code{"date"} (calendar date). \cr
#'    The default \code{time = "newmoon"} produces an equispaced observation 
#'    timestep, a common format format for discrete-time modeling. 
#'   \item \code{na_drop}: \code{logical} indicator of if \code{NA} values 
#'    (representing insufficient sampling) should be dropped.
#'   \item \code{zero_drop}: \code{logical} indicator of if \code{0} values 
#'    (representing sufficient sampling but no detection) should be dropped.
#'   \item \code{effort}: \code{logical} indicator of if the effort columns 
#'    should be included in the output.
#'   \item \code{filename}: \code{character} name of the file for saving the 
#'    output.
#'  }
#'  If only a single dataset is added, the name of the set from the element
#'  \code{data_set} will be used to name the model's \code{list} in the 
#'  larger \code{list}. If multiple models are added, each element \code{list} 
#'  must be named according to the dataset and the \code{data_set} element.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return Named \code{list} of length equal to the number of elements in 
#'  \code{data_sets} and with elements that are each 
#'  \code{list}s of those \code{data_set}'s data-generating controls, for
#'  input as \code{controls_rodents} in \code{\link{prep_rodents}}.
#'
#' @examples
#'  rodents_controls(c("all", "controls"))
#'  all_PPonly <- rodents_control("all_PPonly", species = "PP")
#'  rodents_controls("all_PPonly", all_PPonly, arg_checks = FALSE)
#'  rodents_controls(c("all", "all_PPonly"), all_PPonly, arg_checks = FALSE)
#'
#' @export
#'
rodents_controls <- function(data_sets = NULL, controls_rodents = NULL, 
                             arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(data_sets)
  if(list_depth(controls_rodents) == 1){
    controls_rodents <- list(controls_rodents)
    names(controls_rodents) <- controls_rodents[[1]]$name
  }
  nadd <- length(controls_rodents)

  prefab_controls <- prefab_rodents_controls()
  nprefab <- length(prefab_controls)
  for(i in 1:nprefab){
    controls_rodents[nadd + i] <- list(prefab_controls[[i]])
    names(controls_rodents)[nadd + i] <- names(prefab_controls)[i]
  }
  included_data <- which(names(controls_rodents) %in% data_sets)
  missing_controls <- which((data_sets %in% names(controls_rodents)) == FALSE)
  replicates <- table(names(controls_rodents))
  if(length(missing_controls) > 0){
    which_missing <- data_sets[missing_controls]
    all_missing <- paste(which_missing, collapse = ", ")
    msg <- paste0("missing controls for dataset(s): ", all_missing)
    stop(msg, call. = FALSE)
  }
  if(any(replicates > 1)){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of dataset(s): ", all_conflicting)
    stop(msg, call. = FALSE)
  }
  controls_rodents[included_data]

}

#' @title Create a control lists for generating a rodents data table
#'
#' @description Given the number of arguments into 
#'  \code{\link[portalr]{summarize_rodent_data}} via 
#'  \code{\link{prep_rodents_table}}, it helps to have a control \code{list}  
#'  to organize them. \code{rodents_control} provides a template for the 
#'  controls \code{list} used to define the specific arguments for a user's
#'  novel data set, setting the formal arguments to the basic default 
#'  values
#
#' @param name \code{character} value of the rodent data set name
#'  used to enforce certain arguments. Currently used prefab data
#'  sets are \code{"all"}, \code{"all_interp"}, \code{"controls"}, 
#'  and \code{"controls_interp"}. 
#'
#' @param species \code{character}-valued vector of species names 
#'  to include.  
#'
#' @param total \code{logical} value indicating if a total 
#'  (sum across species) should be added or not. Only available if more than 
#'  one species is included. 
#'
#' @param interpolate \code{logical} value indicating if the data should be
#'  interpolated to fill in \code{NA} values (using 
#'  \code{\link[forecast]{na.interp}}). 
#'
#' @param clean \code{logical} indicator of if only the rodent data
#'  that passed QA/QC (\code{clean = TRUE}) or if all data 
#'  (\code{clean = FALSE}) should be loaded.
#'
#' @param type \code{character} value of the rodent data set type, according 
#'  to pre-existing definitions. An alternative toggle to \code{species}. \cr
#'  Either all species 
#'  (\code{type = "Rodents"}) or only granivores (\code{type = "Granivores"}). 
#'
#' @param level \code{character} indicating the type of summary:
#'  \code{"Plot"}, \code{"Treatment"}, or \code{"Site"}. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param plots Specification of subset of plots. Can be a vector of 
#'  \code{numeric} plots indicators or specific sets indicated by
#'  \code{character} values: \code{"all"} plots or \code{"Longterm"} plots
#'  (plots that have had the same treatment for the entire time series).
#'
#' @param treatment \code{character} indicating the specific 
#'  treatment(s) to trim to if \code{level = "Treatment"}: \code{"control"},
#'  \code{"exclosure"}, \code{"removal"}, or \code{"spectabs"} 
#'
#' @param min_plots \code{integer} (or integer \code{numeric}) of the 
#'  minimum number of plots surveyed for a survey to be used. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param min_traps \code{integer} (or integer \code{numeric}) of the 
#'  minimum number of traps collected for a plot to be used. Pipes directly
#'  to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param output \code{character} indicating the type of data:
#'  \code{"abundance"}, \code{"biomass"}, or \code{"energy"}. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param fillweight \code{logical} specifier of whether to fill in unknown 
#'  weights with other records from that individual or species, where 
#'  possible.
#'
#' @param unknowns \code{logical} indicator to either remove all 
#'  individuals not identified to species (\code{unknowns = FALSE}) or 
#'  sum them in an additional column (\code{unknowns = TRUE}.
#'
#' @param time \code{character} value specifying the format of the time index 
#'  in the output. Options are \code{"period"} (sequential Portal surveys), 
#'  \code{"newmoon"} (lunar cycle numbering), and \code{"date"} (calendar 
#'  date). \cr
#'  The default \code{time = "newmoon"} produces an equispaced observation 
#'  timestep, a common format format for discrete-time modeling. 
#'
#' @param na_drop \code{logical} indicator of if \code{NA} values 
#'  (representing insufficient sampling) should be dropped.
#'
#' @param zero_drop \code{logical} indicator of if \code{0} values 
#'  (representing sufficient sampling but no detection) should be dropped.
#'
#' @param effort \code{logical} indicator of if the effort columns should
#'  be included in the output.
#'
#' @param filename \code{character} name of the file for saving the 
#'  output. The extension of the filename may be used for identifying
#'  object requirements (for example, if the extension of \code{filename} is
#'  \code{".csv"}, it will trigger use of \code{\link[utils]{write.csv}}). 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return Named \code{list} of \code{data_set}'s data-generating controls, 
#'  for input as part of \code{controls_rodents} in 
#'  \code{\link{prep_rodents}}.
#'
#' @examples
#'  rodents_controls(c("all", "controls"))
#'  all_PPonly <- rodents_control("all_PPonly", species = "PP")
#'  rodents_controls("all_PPonly", all_PPonly, arg_checks = FALSE)
#'  rodents_controls(c("all", "all_PPonly"), all_PPonly, arg_checks = FALSE)
#'
#' @export
#'
rodents_control <- function(name = "name", species = base_species(), 
                            total = TRUE, interpolate = FALSE,
                            clean = FALSE, type = "Rodents", 
                            level = "Site", plots = "all", treatment = NULL, 
                            min_plots = 24, min_traps = 1, 
                            output = "abundance",
                            fillweight = (output != "abundance"), 
                            unknowns = FALSE, time = "newmoon", 
                            na_drop = FALSE,
                            zero_drop = switch(tolower(level), 
                                                    plot = FALSE, 
                                                    treatment = TRUE,
                                                    site = TRUE),
                            effort = FALSE,
                            filename = paste0("rodents_", name, ".csv"),
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(name)
  list(name = name, species = species, total = total, 
       interpolate = interpolate, clean = clean, type = type, level = level, 
       plots = plots, treatment = treatment, min_plots = min_plots, 
       min_traps = min_traps, output = output, fillweight = fillweight, 
       unknowns = unknowns, time = time, na_drop = na_drop, 
       zero_drop = zero_drop, effort = effort, filename = filename)
}



#' @title Prepare a list of rodents data tables for forecasting
#'
#' @description This function loops around \code{\link{prep_rodents_table}},
#'  which wraps around \code{\link[portalr]{summarize_rodent_data}} to 
#'  produce a list of rodents \code{data.frame}s associated with multiple
#'  sets of specifications. The default settings are for a general 
#'  portalcasting directory, which models the "all" and "controls" data 
#'  subsets.
#'
#' @param controls_rodents Control \code{list} or \code{list} of control 
#'  \code{list}s from \code{\link{rodents_controls}} specifying the 
#'  structuring of the rodents tables. See \code{\link{rodents_controls}}. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#
#' @param data_sets \code{character} values of the treatment 
#'  types. If left as \code{NULL} (default), it is replaced with
#'  the output from \code{\link{prefab_data_sets}}. \cr
#'  Used to set arguments for \code{\link{prep_rodents_table}}.
#'
#' @param ref_species \code{character}-valued vector of all possible species 
#'  names that could be used. Should be set up via \code{\link{all_species}}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{list} of \code{data.frame}s, one for each data subset
#'  specified, as generated by \code{\link{prep_rodents_table}}.
#'
#' @examples
#'  \donttest{
#'    create_dir()
#'    fill_raw()
#'    prep_rodents()
#'  }
#'
#' @export
#'
prep_rodents <- function(main = ".", moons = NULL, data_sets = NULL,
                         end_moon = NULL, controls_rodents = NULL, 
                         control_files = files_control(), 
                         ref_species = all_species(), quiet = TRUE,
                         verbose = FALSE,  arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_sets <- ifnull(data_sets, prefab_data_sets())
  messageq("  -rodents data files", quiet)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  controls_rodents <- rodents_controls(data_sets = data_sets, 
                                 controls_rodents = controls_rodents, 
                                 arg_checks = arg_checks)
  rodents <- named_null_list(data_sets)
  ndata_sets <- length(rodents)
  for(i in 1:ndata_sets){
    ctrl_r_i <- controls_rodents[[data_sets[i]]]
    messageq(paste0("   -", data_sets[i]), quiet)


    rodents[[i]] <- prep_rodents_table(main = main, moons = moons, 
                                       end_moon = end_moon, 
                                       species = ctrl_r_i[["species"]],
                                       total = ctrl_r_i[["total"]],
                                       interpolate = 
                                         ctrl_r_i[["interpolate"]],
                                       clean = ctrl_r_i[["clean"]],
                                       type = ctrl_r_i[["type"]],
                                       level = ctrl_r_i[["level"]],
                                       plots = ctrl_r_i[["plots"]],
                                       treatment = ctrl_r_i[["treatment"]],
                                       min_plots = ctrl_r_i[["min_plots"]],
                                       min_traps = ctrl_r_i[["min_traps"]],
                                       output = ctrl_r_i[["output"]],
                                       fillweight = ctrl_r_i[["fillweight"]],
                                       unknowns = ctrl_r_i[["unknowns"]],
                                       time = ctrl_r_i[["time"]],
                                       na_drop = ctrl_r_i[["na_drop"]],
                                       zero_drop = ctrl_r_i[["zero_drop"]],
                                       ref_species = ref_species,
                                       effort = ctrl_r_i[["effort"]],
                                       quiet = quiet,
                                       verbose = verbose,
                                       save = control_files$save,
                                       overwrite = control_files$overwrite,
                                       filename = ctrl_r_i[["filename"]],
                                       arg_checks = arg_checks)
  }
  rodents
}



prep_rodents_table <- function(main = ".", moons = NULL, end_moon = NULL, 
                               species = base_species(), total = TRUE, 
                               interpolate = TRUE,
                               clean = FALSE, type = "Rodents", 
                               level = "Site", plots = "all", 
                               treatment = NULL,
                               min_plots = 24, min_traps = 1, 
                               output = "abundance", shape = "crosstab",
                               fillweight = (output != "abundance"),
                               unknowns = FALSE, time = "newmoon",
                               na_drop = FALSE, 
                               zero_drop = switch(tolower(level), 
                                                  plot = FALSE, 
                                                  treatment = TRUE,
                                                  site = TRUE),
                               ref_species = all_species(),
                               effort = FALSE,  quiet = TRUE, 
                               verbose = FALSE,
                               control_files = files_control(),
                               save = TRUE, overwrite = TRUE, 
                               filename = "rodents_all.csv", 
                               arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(species) 
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  raw_path <- raw_path(main = main, arg_checks = arg_checks)
  rodents_tab <- summarize_rodent_data(path = raw_path, clean = clean, 
                                       level = level, type = type, 
                                       plots = plots, unknowns = unknowns,
                                       shape = shape, time = time, 
                                       output = output,
                                       fillweight = fillweight, 
                                       na_drop = na_drop,
                                       zero_drop = zero_drop, 
                                       min_traps = min_traps,
                                       min_plots = min_plots, effort = effort, 
                                       quiet = !verbose) 
  out <- process_rodent_data(rodents_tab = rodents_tab, main = main, 
                             moons = moons, end_moon = end_moon,
                            species = species, total = total, 
                            interpolate = interpolate, clean = clean, 
                            level = level, treatment = treatment, 
                            na_drop = na_drop, time = time, 
                            ref_species = ref_species, quiet = quiet, 
                            verbose = verbose, arg_checks = arg_checks) 
  write_data(out, main = main, save = save, filename = filename, 
             overwrite = overwrite, quiet = !verbose, arg_checks = arg_checks)
}

#' @rdname prepare_rodents_table
#'
#' @export
#'
process_rodent_data <- function(rodents_tab, main = ".", moons = NULL,
                                end_moon = NULL, species = base_species(), 
                                total = TRUE, interpolate = FALSE,
                                clean = FALSE, level = "Site", 
                                treatment = NULL, na_drop = FALSE,
                                time = "newmoon",
                                ref_species = all_species(),
                                quiet = TRUE, verbose = FALSE,
                                arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  nspecies <- length(species)
  total <- ifelse(nspecies == 1, FALSE, total)
  drop_species <- ref_species[which(ref_species %in% species == FALSE)]
  if(length(drop_species) > 0){
    cols_in <- !(colnames(rodents_tab) %in% drop_species)
    rodents_tab <- rodents_tab[ , cols_in] 
    spp <- paste(drop_species, collapse = ", ")
    msg <- paste0("    removing species: ", spp)
    messageq(msg, !verbose)
  }
  if(total){
    spp_col <- is_sp_col(rodents_tab = rodents_tab, species = ref_species,
                         arg_checks = arg_checks)
    rodents_tab$total <- rowSums(rodents_tab[ , spp_col])
    messageq("    adding total column", !verbose)
  }
  if(level == "Treatment"){
    rows_in <- rodents_tab$treatment %in% treatment
    cols_in <- colnames(rodents_tab) != "treatment"
    rodents_tab <- rodents_tab[rows_in, cols_in]
  }
  if(time == "newmoon"){
    newmoon_col <- which(colnames(rodents_tab) == "newmoonnumber")
    colnames(rodents_tab)[newmoon_col] <- "moon" 
    if(!na_drop){ 
      present_moons <- rodents_tab[ , "moon"]
      range_moons <- range(present_moons, na.rm = TRUE)
      seq_moons <- seq(range_moons[1], range_moons[2], by = 1)
      needed_moons <- !(seq_moons %in% present_moons)
      which_needed_moons <- seq_moons[needed_moons]
      nneeded_rows <- length(which_needed_moons)
      ndf_moons <- ncol(rodents_tab)
      needed_rows <- matrix(NA, nrow = nneeded_rows, ncol = ndf_moons)
      colnames(needed_rows) <- colnames(rodents_tab)
      needed_rows[ , "moon"] <- seq_moons[needed_moons]
      rodents_tab <- rbind(rodents_tab, needed_rows)
      rodents_tab_ord <- order(rodents_tab[ , "moon"])
      rodents_tab <- rodents_tab[rodents_tab_ord, ]
    }
    if(interpolate){
      for(i in 1:nspecies){
        interped <- round(na.interp(rodents_tab[ , species[i]]))
        rodents_tab[ , species[i]] <- interped

      }
      rodents_tab[ , "total"] <- apply(rodents_tab[ , species, drop = FALSE],
                                       1, sum)
    }


    end_moon <- ifnull(end_moon, max(rodents_tab$moon))
    last_moon_in <- min(c(end_moon, max(rodents_tab$moon)))
    rows_in <- rodents_tab$moon <= last_moon_in
    rodents_tab <- rodents_tab[rows_in, ]
    messageq("    data trimmed according to moon window", !verbose)
  } else{
    messageq("    no time processing conducted", !verbose)
  }
  rodents_tab
}



#' @title Determine if columns in a table are species columns
#'
#' @description Given a table, returns a \code{logical} vector indicating
#'  if each column is a species' column or not.
#'
#' @param rodents_tab \code{data.frame} of columns to be checked. 
#'
#' @param species \code{character} vector indicating species names to 
#'  use in determining if columns are species columns. Defaults to
#'  \code{\link{all_species}}.
#'
#' @param total \code{logical} indicator if the total should be included.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param nadot \code{logical} indicator if the dot should be added to the 
#'   \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @return 
#'  \code{is_sp_col}: \code{logical} vector indicating if each column is a 
#'  species' column or not. \cr \cr
#'  \code{species_from_table}: \code{character} vector of species names from
#'  the column names in the \code{rodents_tab} data table. 
#'  
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   rodents_tab <- prep_rodents_table()
#'   is_sp_col(rodents_tab)
#'   species_from_table(rodents_tab)
#'  }
#'
#' @name species_in_tables
#'
NULL 

#' @rdname species_in_tables
#'
#' @export
#'
is_sp_col <- function(rodents_tab, species = NULL, total = FALSE, 
                      nadot = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  species <- ifnull(species, all_species(total = total, nadot = nadot))
  colnames(rodents_tab) %in% species
}

#' @rdname species_in_tables
#'
#' @export
#'
species_from_table <- function(rodents_tab = NULL, total = FALSE, 
                               nadot = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  sp_col <- is_sp_col(rodents_tab = rodents_tab, total = total, nadot = nadot,
                      arg_checks = arg_checks)
  colnames(rodents_tab)[sp_col]
}



prefab_model_controls <- function(){
  list(
    AutoArima = list(name = "AutoArima", 
                     data_sets = c("all", "controls", "exclosures",
                                   "dm_controls"),
                     covariatesTF = FALSE, 
                     lag = NA), 
    ESSS = list(name = "ESSS", 
                data_sets = c("all_interp", "controls_interp", 
                              "exclosures_interp", "dm_controls_interp"),
                covariatesTF = FALSE, 
                lag = NA), 
    NaiveArima = list(name = "NaiveArima", 
                      data_sets = c("all", "controls", "exclosures",
                                    "dm_controls"),
                      covariatesTF = FALSE, 
                      lag = NA), 
    nbGARCH = list(name = "nbGARCH", 
                   data_sets = c("all_interp", "controls_interp", 
                                 "exclosures_interp", "dm_controls_interp"),
                   covariatesTF = FALSE, 
                   lag = NA), 
    nbsGARCH = list(name = "nbsGARCH", 
                    data_sets = c("all_interp", "controls_interp", 
                                  "exclosures_interp", "dm_controls_interp"),
                    covariatesTF = FALSE, 
                    lag = NA), 
    pevGARCH = list(name = "pevGARCH",  
                    data_sets = c("all_interp", "controls_interp", 
                                  "exclosures_interp", "dm_controls_interp"),
                    covariatesTF = TRUE, lag = 6), 
    simplexEDM = list(name = "simplexEDM", 
                      data_sets = c("all_interp", "controls_interp", 
                                    "exclosures_interp", 
                                     "dm_controls_interp"), 
                      covariatesTF = FALSE, lag = NA, max_E = 7),
    GPEDM = list(name = "GPEDM", 
                 data_sets = c("all_interp", "controls_interp", 
                               "exclosures_interp", "dm_controls_interp"), 
                 covariatesTF = FALSE, lag = NA, max_E = 7),
    
    jags_RW = list(name = "jags_RW", 
                   data_sets = c("all", "controls", "exclosures",
                                 "dm_controls"), 
                   covariatesTF = FALSE, lag = NA,
                   control_runjags = runjags_control())
  )
}


#' @title Produce the control lists for models
#' 
#' @description The models are written and managed using generalized functions
#'  that take a few arguments that vary among models, and so this function
#'  produces the appropriate \code{list} of \code{list}s of model
#'  script-writing control arguments. The specific \code{models} must be
#'  input by the user, with the default being \code{NULL}, which returns.
#'  \code{NULL}. The prefab models (\code{\link{prefab_models}}) have their
#'  control lists included already, but any user-created model needs to
#'  have controls provided. See \code{Details}. \cr 
#'  \strong{Users adding models to the prefab set should add their
#'  script-writing controls lists to the \code{prefab_controls} object in 
#'  the source code of the function.} \cr \cr
#'  \code{extract_min_lag} provides a simple function to extract the 
#'  minimum non-0 lag time used across models from the controls lists.
#'
#' @details Any model that is part of the \code{prefab} set 
#'  (\code{c("AutoArima", "NaiveArima", "ESSS", "nbGARCH", "nbsGARCH", 
#'  "pevGARCH", "simplexEDM", "GPEDM", "jags_RW")}) 
#'  has its script-writing controls already included internally
#'  via the non-exported function \code{prefab_model_controls}. Users 
#'  only need to include controls for non-prefab \code{models}. \cr \cr
#'  Any user-defined \code{models} that are not included in \code{controls_m}
#'  will throw an error. \cr \cr 
#'  If any user-defined \code{controls_model} duplicate any existing controls
#'  for the prefab models or if \code{controls_model} contains any duplicate 
#'  named elements, an error will be thrown unless \code{arg_checks = FALSE},
#'  in which case, the first user-input element for any and all conflicting 
#'  copies is selected. This override allows users to test an existing
#'  prefab model with different configurations, such as on a new data set, 
#'  without requiring a new model. \cr \cr
#'  Users interested in adding models to the prefab set should add the
#'  controls to the \code{prefab_model_controls} non-exported function
#'  found in the \code{prepare_models.R} script.
#'
#' @param models \code{character} vector of name(s) of model(s) whose 
#'  script-writing controls need to be included in the \code{list} used
#'  to write the scripts. 
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{data_sets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  In addition, some models require additional specific elements: 
#'  \itemize{
#'   \item \code{max_E}: \code{integer} (or integer \code{numeric}) 
#'    maximum embedding dimension to search amongst for EDM models. See 
#'    \code{\link[rEDM]{simplex}} for more information.
#'   \item \code{control_runjags}: \code{list} of arguments passed to 
#'    \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}. 
#'  }
#'  If only a single model is added, the name of the model from the element
#'  \code{name} will be used to name the model's \code{list} in the larger
#'  \code{list}. If multiple models are added, each element \code{list} must
#'  be named according to the model and the \code{name} element. \cr 
#'  See \code{Details} and \code{Examples}. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @return \code{model_controls}: named \code{list} of length equal to
#'  the number of elements in \code{models} and with elements that are each 
#'  \code{list}s of those \code{models}'s script-writing controls. \cr \cr
#'  \code{extract_min_lag}: \code{numeric} value of the minimum non-0 lag
#'  from any included model or \code{NA} if no models have lags. \cr \cr
#'  \code{extract_data_sets}: \code{character} vector of the data set names
#'  from all included models.
#'
#' @examples
#'  model_controls(prefab_models())
#'  controls <- list(name = "xx", data_sets = prefab_data_sets(), 
#'                   interpolate = FALSE, covariatesTF = FALSE, lag = NA)
#'  model_controls("xx", controls)
#'  model_controls(c(prefab_models(), "xx"), controls)
#'  model_controls(c("xx", "ESSS"), controls)
#'  extract_min_lag()
#'  extract_min_lag("AutoArima")
#'  extract_data_sets()
#'  extract_data_sets("AutoArima")
#'
#' @export
#'
model_controls <- function(models = NULL, controls_model = NULL, 
                           quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  if(list_depth(controls_model) == 1){
    controls_model <- list(controls_model)
    names(controls_model) <- controls_model[[1]]$name
  }
  nadd <- length(controls_model)
  prefab_controls <- prefab_model_controls()
  nprefab <- length(prefab_controls)
  for(i in 1:nprefab){
    controls_model[nadd + i] <- list(prefab_controls[[i]])
    names(controls_model)[nadd + i] <- names(prefab_controls)[i]
  }
  missing_controls <- !(models %in% names(controls_model))
  which_missing_controls <- which(missing_controls == TRUE)
  nmissing_controls <- length(which_missing_controls) 
  if(nmissing_controls > 0){
    which_missing <- models[missing_controls]
    all_missing <- paste(which_missing, collapse = ", ")
    msg <- paste0("  ~no controls input for ", all_missing)
    msg <- c(msg, "  **assuming controls follow `model_control()`**")  
    messageq(msg, quiet)
    nadd <- length(controls_model)
    for(i in 1:nmissing_controls){
      mod_name <- models[which_missing_controls[i]]

      controls_model[[nadd + i]] <- model_control(name = mod_name,
                                                   arg_checks = arg_checks)
      names(controls_model)[nadd + i] <- mod_name
    }
  }
  replicates <- table(names(controls_model))
  if(any(replicates > 1) & arg_checks){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of model(s): ", all_conflicting)
    msg2 <- paste0(msg, "\n   to override, set `arg_checks = FALSE`")
    stop(msg2, call. = FALSE)
  }
  if(any(replicates > 1) & !arg_checks){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of model(s): ", all_conflicting)
    msg2 <- c(msg, " using first user-defined input for each")
    messageq(msg2, quiet)
    umods <- unique(names(controls_model))
    nmods <- length(umods)
    controls_model2 <- vector("list", length = nmods)
    for(i in 1:nmods){
      choice <- which(names(controls_model) == umods[i])[1]
      controls_model2[[i]] <- controls_model[[choice]]
    }
    names(controls_model2) <- umods
    controls_model <- controls_model2
  }

  included_models <- which(names(controls_model) %in% models)
  controls_model[included_models]
}

#' @rdname model_controls
#'
#' @export
#'
extract_min_lag <- function(models = prefab_models(), controls_model = NULL, 
                            quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  controls <- model_controls(models = models,
                             controls_model = controls_model,
                             quiet = quiet, arg_checks = arg_checks)
  nmods <- length(controls)
  lags <- rep(NA, nmods)
  for(i in 1:nmods){
    lag_i <- controls[[i]]$lag
    lags[i] <- ifelse(is.na(lag_i), Inf, lag_i)
  }
  min_lag <- min(lags)
  ifelse(min_lag == Inf, NA, min_lag)
}

#' @rdname model_controls
#'
#' @export
#'
extract_data_sets <- function(models = prefab_models(), 
                              controls_model = NULL, 
                              quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  controls <- model_controls(models = models, 
                             controls_model = controls_model,
                             quiet = quiet, arg_checks = arg_checks)
  nmods <- length(controls)
  data_sets <- NULL
  for(i in 1:nmods){
    data_sets <- unique(c(data_sets, controls[[i]]$data_sets))
  }
  data_sets
}



#' @rdname write_model
#'
#' @export
#'
control_list_arg <- function(control_list = NULL, list_function = NULL,
                             arg_checks = TRUE){                             
  return_if_null(control_list)
  return_if_null(list_function)

  list_name <- paste(strsplit(list_function, "_")[[1]][2:1], collapse = "_")
  nvals <- length(control_list)
  val_values <- rep(NA, nvals)
  for(i in 1:nvals){
    val_name <- names(control_list)[i]
    val_value <- control_list[[i]]
    formal_value <- formals(eval(parse(text = list_function)))[[val_name]]

    if(!identical(val_value, formal_value)){
      if(is.character(val_value)){
        val_value <- paste0('"', val_value, '"')
      }
      if(is.null(val_value)){
        val_value <- "NULL"
      }
      val_values[i] <- val_value
    }
  }
  update_values <- which(is.na(val_values) == FALSE)
  nupdate_values <- length(update_values)
  val_texts <- NULL
  if(nupdate_values > 0){
    val_text <- rep(NA, nupdate_values)
    update_val_names <- names(control_list)[update_values]
    update_val_values <- val_values[update_values]
    for(i in 1:nupdate_values){
      val_text[i] <- paste0(update_val_names[i], ' = ', update_val_values[i])
    }
    val_texts <- paste0(val_text, collapse = ', ')
  }
  paste0(', ', list_name, ' = ', list_function, '(', val_texts, ')')
}


#' @title Create a control list for a model
#'
#' @description Provides a ready-to-use template for the 
#'  controls \code{list} used to define the specific arguments for a user's
#'  novel model, setting the formal arguments to the basic default 
#'  values.
#
#' @param name \code{character} value of the name of the model.
#'
#' @param data_sets \code{character} vector of the rodent data set names
#'  that the model is applied to. Defaults to all non-interpolated data sets.
#'
#' @param covariatesTF \code{logical} indicator for if the model requires 
#'  covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NULL} if \code{covariatesTF} is \code{FALSE}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return Named \code{list} of model's script-generating controls, 
#'  for input as part of \code{controls_m} in \code{\link{write_model}}.
#'
#' @export
#'
model_control <- function(name = "model", 
                          data_sets = prefab_data_sets(interpolate = FALSE),
                          covariatesTF = FALSE, lag = NA, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  list(name = name, data_sets = data_sets, covariatesTF = covariatesTF, 
      lag = lag)
}

#' @title Verify that models requested to cast with exist
#'
#' @description Verify that models requested have scripts in the models 
#'   subdirectory. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param models \code{character} vector of the names of models to verify. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{NULL} with messaging. 
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   verify_models()
#'  }
#'
#' @export
#'
verify_models <- function(main = ".", models = prefab_models(), 
                          quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  messageq("Checking model availability", quiet)
  model_dir <- sub_path(main = main, subs = "models", arg_checks = arg_checks)
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist", call. = FALSE)
  }
  available <- list.files(model_dir)
  if (models[1] != "all"){
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      msg <- paste0("Requested model(s) ", missmod, " not in directory \n")
      stop(msg, call. = FALSE)
    }
  }
  messageq(" *All requested models available*", quiet)
  messageq_break(quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}


#' @title Update models based on user input controls
#'
#' @description Update model scripts based on the user-defined model control
#'  inputs. This allows users to define their own models or re-define prefab
#'  models within the \code{\link{portalcast}} pipeline.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s), used to 
#'  restrict the models to update from \code{controls_model}. The default
#'  (\code{NULL}) translates to all models in the \code{controls_model} list,
#'  which is recommended for general usage and is enforced within the main
#'  \code{\link{portalcast}} pipeline. 
#'
#' @param controls_model Controls for models not in the prefab set or for 
#'  overriding those in the prefab set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{data_sets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  included in messaging.
#'
#' @param update_prefab_models \code{logical} indicator if all of the models'
#'  scripts should be updated, even if they do not have an explicit change
#'  to their model options via \code{controls_model}. Default is
#'  \code{FALSE}, which leads to only the models in \code{controls_model}
#'  having their scripts re-written. Switching to \code{TRUE} results in the
#'  models listed in \code{models} having their scripts re-written. \cr \cr
#'  This is particularly helpful when one is changing the global (with respect
#'  to the models) options \code{main}, \code{quiet}, \code{verbose}, or
#'  \code{control_files}.
#'
#' @return \code{NULL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   cm <- model_control(name = "AutoArima", data_sets = "all")
#'   update_models(controls_model = cm)
#'  }
#'
#' @export
#'
update_models <- function(main = ".", models = NULL,
                          controls_model = NULL, update_prefab_models = FALSE, 
                          control_files = files_control(), bline = FALSE,
                          quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  if(list_depth(controls_model) == 1){
    controls_model <- list(controls_model)
    names(controls_model) <- controls_model[[1]]$name
  }
  if(update_prefab_models){
    controls_model <- model_controls(models = models, 
                                     controls_model = controls_model, 
                                     quiet = quiet, arg_checks = arg_checks)
  } else{
    models <- NULL
  }
  return_if_null(c(controls_model, models))
  models <- unique(c(models, names(controls_model)))
  messageq("Updating model scripts", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, verbose = verbose, 
                control_files = control_files, 
                control_model = controls_model[[models[i]]], 
                arg_checks = arg_checks)
  }
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks) 
  invisible(NULL)
}


#' @title Create a covariate model list
#'
#' @description Convenience function for creating covariate model 
#'  \code{list}s.
#'
#' @param model \code{character} name for covariate models. Currently only 
#'  \code{"pevGARCH"} is supported.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{list} of covariate model structures.
#'
#' @examples
#'  covariate_models()
#'
#' @export
#'
covariate_models <- function(model = "pevGARCH", arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  out <- NULL
  if (model == "pevGARCH"){
    out <- list(c("maxtemp", "meantemp", "precipitation", "ndvi"),
                c("maxtemp", "mintemp", "precipitation", "ndvi"),
                c("mintemp", "maxtemp", "meantemp", "precipitation"),
                c("precipitation", "ndvi"),
                c("mintemp", "ndvi"),
                c("mintemp"),
                c("maxtemp"),
                c("meantemp"),
                c("precipitation"),
                c("ndvi"),
                c(NULL))
  }
  out
}
