

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
#'  casts sub directory. \cr \cr
#'  \code{read_cast_tab} retrieves the \code{cast_tab}. \cr \cr
#'  \code{read_cast_tabs} combines one or more \code{cast_tab}s. \cr \cr
#'  \code{read_cast_tab} retrieves the \code{cast_metdata}
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
#'  \code{read_cast_metadata}: \code{list} of the \code{cast_metadata}.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   read_cast_tab(cast_id = 1)
#'   read_cast_tabs(cast_ids = 1:2)
#'   read_cast_metadata(cast_id = 1)
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
    stop("cast_id does not have a cast_table")
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
    stop("cast_id does not have a cast_metadata file")
  }
  yaml.load_file(cpath) 
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
#'  names should also be inclusive of the asssociated interpolated data sets.
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
#'   \item \code{"model_fits"}: saved out via \code{\link{save}}, so quite
#'    flexible with respect to object structure. Is used to save actual model
#'    fit/return objects so that models do not need to be refit later.
#'   \item \code{"casts"}: saved with \code{\link{save}}, so quite
#'    flexible with respect to object structure. Is used to save \code{list}s
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
                                  "_model_fits.RData") 
    model_fits_path <- file_path(main = main, sub = "casts", 
                                 files = model_fits_filename,
                                 arg_checks = arg_checks)
    model_fits <- cast$model_fits
    save(model_fits, file = model_fits_path)
  }
  if(!is.null(cast$model_casts)){
    model_casts_filename <- paste0("cast_id_", next_cast_id, 
                                   "_model_casts.RData") 
    model_casts_path <- file_path(main = main, sub = "casts", 
                                  files = model_casts_filename, 
                                  arg_checks = arg_checks)
    model_casts <- cast$model_casts
    save(model_casts, file = model_casts_path)
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

