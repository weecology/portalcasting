#' @title Optionally generate a message based on a logical input
#'
#' @description Given the input to \code{quiet}, generate the message(s) 
#'   in \code{msg} or not.
#'
#' @param msg \code{character} vector of the message(s) to generate or 
#'   \code{NULL}. If more than one element is contained in \code{msg}, they
#'   are concatenated with a newline between.
#'
#' @param quiet \code{logical} indicator controlling if the message is
#'   generated.
#'
#' @export
#'
messageq <- function(msg = NULL, quiet = FALSE){
  check_args()
  if (!quiet){
    msg2 <- paste(msg, collapse = "\n")
    message(msg2)
  }
}


#' @title Conform NA entries to "NA" entries
#'
#' @description Given the species abbreviation NA, when data are read in, 
#'   there can be an \code{NA} when it should be an \code{"NA"}. This function
#'   conforms the entries to be proper character values. 
#'
#' @param x Either [1] a \code{data.frame} containing \code{colname} as a 
#'   column with \code{NA}s that need to be conformed to \code{"NA"}s or [2]
#'   a vector with \code{NA}s that need to be conformed to \code{"NA"}s
#'
#' @param colname \code{character} value of the column name in \code{tab} to 
#'   conform the \code{NA}s to \code{"NA"}s.
#'
#' @return \code{x} with any \code{NA} in \code{colname} replaced with 
#'   \code{"NA"}.
#'
#' @export
#'
na_conformer <- function(x, colname = "species"){
  check_args()
  if (is.vector(x)){
    naentries <- which(is.na(x))
    x[naentries] <- "NA"
  } else if (is.data.frame(x)){
    nasppname <- which(is.na(x[ , colname]))
    if (length(nasppname) > 0){
      x[nasppname, colname] <- "NA"
    }
  } 
  x
}

#' @title Save data out to a file and return it	
#'
#' @description Save inputted data out to a data file if requested via an 
#'   options list (request is handled by the \code{save} element and the file
#'   name to use is handled by the \code{filename} element) and return it 
#'   to the console.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param options_out an options \code{list} that includes a save element and 
#'   a filename element, and potentially a class element.
#'
#' @return df (as input)
#'
#' @export
#'
dataout <- function(df, options_out = moons_options()){
  check_args()
  if (!is.null(options_out$save)){
    if (options_out$save){
      file_paths(options_out$tree, paste0("data/", options_out$filename)) %>%
      write.csv(df, ., row.names = FALSE)
    }
  }
  if (!is.null(options_out$class)){
    class(df) <- unique(c(options_out$class, class(df)))
  }
  df
}

#' @title Save data out to a csv, appending the file if it already exists
#'
#' @description Appending a \code{.csv} without re-writing the header of the
#'   file. If the doesn't exist, it will be created.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param filename \code{character} filename of existing \code{.csv} to be 
#'   appended.
#'
#' @export
#'
append_csv <- function(df, filename){
  check_args()
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
}

#' @title Zero-abundance forecast
#'
#' @description Create a 0-abundance forecast for fill-in usage when a model 
#'   fails or there is no non-0 historical abundance.
#'
#' @param nfcnm \code{integer} number of forecast newmoons.
#'
#' @param colname \code{character} name for the predictor column (to match
#'   variable model output names).
#'
#' @return Two-element \code{list} of means and interval values for a 
#'   0-abundance forecast to be used as a filler when a model fails or there 
#'   is no non-0 historical abundance.
#'
#' @export
#'
fcast0 <- function(nfcnm, colname = "pred"){
  check_args()
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list(mean_0, interval = int_0)
  names(out)[1] <- colname
  out
}

#' @title Today's date (potentially with time)
#'
#' @description Provide the current date (and optionally time) of the system.
#'
#' @param time \code{logical} indicator of whether the output should include
#'   a timestamp as well as date.
#'
#' @return Today's date, as a \code{Date} or date and time as a 
#'   \code{POSIXct}.
#' 
#' @examples
#' \dontrun{
#' 
#' today()
#' today(time = TRUE)
#' }
#'
#' @export
#'
today <- function(time = FALSE){
  check_args()
  if (time){
    Sys.time()
  } else{
    Sys.Date()
  }
}

#' @title Set the class(es) of an object
#'
#' @description For inclusion in a simple pipeline, set the class of an object
#'   and return it. 
#'
#' @param x The object to get the \code{class}(es).
#'
#' @param class The \code{class}(es) to apply to \code{x}.
#'
#' @return \code{x} with class(es) of \code{class}.
#'
#' @export
#'
classy <- function(x, class = NULL){
  check_args()
  class(x) <- class
  x
}

#' @title Remove any specific incomplete entries as noted by an NA
#'
#' @description Remove any incomplete entries in a table, as determined by
#'   the presence of an \code{NA} entry in a specific column 
#'   (\code{col_to_check}).
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param colname A single \code{character} value of the column to use
#'   to remove incomplete entries. 
#'
#' @return df without any incomplete entries. 
#'
#' @export
#'
remove_incompletes <- function(df, colname){
  check_args()
  incompletes <- which(is.na(df[ , colname]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}

#' @title Check a function's arguments values for validity
#'
#' @description \code{check_args} checks that all of the arguments to a given
#'   function have valid values within the pipeline to avoid naming collision
#'   and improper formatting, by wrapping around \code{check_arg} for each
#'   argument. See \code{Details} for argument-specific rules.
#'
#' @export
#'
check_args <- function(){
  fun_call <- match.call.defaults(definition = sys.function(-1), 
                                  call = sys.call(-1))
  fun_name <- toString(fun_call[[1]])
  arg_values <- fun_call[-1]
  arg_names <- names(arg_values)
  nargs <- length(arg_names)

  if(nargs > 0){
    for(i in 1:nargs){
      check_arg(arg_names[i], eval.parent(arg_values[[i]], 2), fun_name)
    }
  }

}

#' @rdname check_args
#'
#' @param arg_name \code{character} value of the argument name.
#'
#' @param arg_value Input value for the argument.
#'
#' @param fun_name \code{character} value of the function name or \code{NULL}.
#'
#' @details Usage and rules for arguments are as follows: \cr \cr
#'   \code{add}: must be \code{NULL} or a \code{character} vector in
#'     \code{\link{model_names}}. \cr \cr
#'   \code{add_error}: must be a length-1 \code{logical} vector in
#'     \code{\link{append_observed_to_cast}}. \cr \cr
#'   \code{add_in_window}: must be a length-1 \code{logical} vector in
#'     \code{\link{append_observed_to_cast}}. \cr \cr
#'   \code{add_lead}: must be a length-1 \code{logical} vector in
#'     \code{\link{append_observed_to_cast}}. \cr \cr
#'   \code{add_obs}: must be a length-1 \code{logical} vector in
#'     \code{\link{plot_cast_ts}}. \cr \cr
#'   \code{all}: must be a \code{list} with elements named \code{forecast} 
#'     and \code{aic} in \code{\link{save_forecast_output}} \cr \cr
#'   \code{all_forecasts}: must be a \code{data.frame} in
#'     \code{\link{make_ensemble}}. \cr \cr
#'   \code{append_fcast_csv}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, and 
#'       \code{\link{data_options}}. \cr \cr
#'   \code{append_missing_to_raw}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{moons_options}}. \cr \cr
#'   \code{base} must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{cast_options}},
#'     \code{\link{data_options}}, \code{\link{dirtree}}, 
#'     \code{\link{dir_options}}, \code{\link{models_options}}, 
#'     \code{\link{PortalData_options}}, and
#'     \code{\link{predictions_options}}. \cr \cr
#'   \code{cast}: must be a valid -cast \code{data.frame} as checked by
#'     \code{\link{cast_is_valid}} in \code{\link{verify_cast}}. \cr \cr
#'   \code{casts}: must be of class \code{casts} in
#'     \code{\link{append_observed_to_cast}} and 
#'     \code{\link{measure_cast_error}}. \cr \cr
#'   \code{cast_date}: must be \code{NULL} or a length-1 \code{Date} or 
#'     \code{Date}-conformable vector in \code{\link{add_addl_future_moons}},
#'     \code{\link{all_options}}, \code{\link{cast_options}}, 
#'     \code{\link{covariates_options}}, \code{\link{data_options}},
#'     \code{\link{metadata_options}}, \code{\link{moons_options}}, 
#'     \code{\link{plot_cast_point}}, \code{\link{plot_cast_ts}}, and 
#'     \code{\link{read_cast}}. \cr \cr
#'   \code{cast_dates}: must be \code{NULL} or a \code{Date} or 
#'     \code{Date}-conformable vector in \code{\link{plot_cov_RMSE_mod_spp}}, 
#'     \code{\link{read_casts}}, and \code{\link{select_most_ab_spp}}. \cr \cr
#'   \code{cast_to_check}: must be a \code{data.frame} in 
#'     \code{\link{cast_is_valid}}. \cr \cr
#'   \code{cast_type}: must be a length-1 \code{character} vector in
#'     \code{\link{most_recent_cast}}, \code{\link{plot_cast_point}},
#'     \code{\link{plot_cast_ts}}, \code{\link{plot_cov_RMSE_mod_spp}}, 
#'     \code{\link{plot_err_lead_spp_mods}}, \code{\link{read_cast}}, 
#'     \code{\link{read_casts}}, and \code{\link{select_most_ab_spp}}. \cr \cr
#'   \code{class}: must be \code{NULL} or a \code{character} vector in
#'     \code{\link{classy}}. \cr \cr
#'   \code{colname}: must be a length-1 \code{character} vector in
#'     \code{\link{fcast0}}, \code{\link{na_conformer}}, and
#'     \code{\link{remove_incompletes}}. \cr \cr
#'   \code{confidence_level}: must be a length-1 \code{numeric} value between
#'     0 and 1 in \code{\link{all_options}}, \code{\link{data_options}}, 
#'     \code{\link{make_ensemble}}, and \code{\link{metadata_options}}. 
#'     \cr \cr
#'   \code{controls}: must be a \code{list} with elements named 
#'     \code{forecast} and \code{aic} in \code{\link{save_forecast_output}}. 
#'     \cr \cr
#'   \code{covariates}: must be a \code{data.frame} of class \code{covariates}
#'     in \code{\link{forecast_covariates}}, \code{\link{forecast_ndvi}},
#'     and \code{\link{prep_metadata}}. \cr \cr
#'   \code{cov_fcast}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, and
#'     \code{\link{data_options}}. \cr \cr
#'   \code{cov_hist}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, and
#'     \code{\link{data_options}}. \cr \cr
#'   \code{c_filename}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, and 
#'     \code{\link{data_options}}. \cr \cr
#'   \code{c_save}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, and 
#'     \code{\link{data_options}}. \cr \cr
#'   \code{data_name}: must be a length-1 \code{character} vector of value 
#'     \code{"all"}, \code{"controls"}, \code{"covariates"}, \code{"moons"},
#'     or \code{"metadata"} in \code{\link{read_data}}. \cr \cr
#'   \code{df}: must be a \code{data.frame} in 
#'     \code{\link{append_csv}}, \code{\link{dataout}}, and
#'     \code{\link{remove_incompletes}}. \cr \cr
#'   \code{download}: must be a length-1 \code{logical} vector in
#'     \code{\link{download_predictions}}. \cr \cr 
#'   \code{download_existing_predictions}: must be a length-1 \code{logical} 
#'     vector in \code{\link{all_options}} and  
#'     \code{\link{predictions_options}}. \cr \cr
#'   \code{drop_spp}: must be a \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}},
#'     \code{\link{remove_spp}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{end}: must be \code{NULL} or a positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}},
#'     \code{\link{cast_options}}, \code{\link{covariates_options}}, 
#'     \code{\link{data_options}}, and \code{\link{rodents_options}}.
#'     \cr \cr
#'   \code{ensemble}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}} and \code{\link{cast_options}}. \cr \cr
#'   \code{extension}: must be a length-1 \code{character} vector with
#'     a single period in \code{\link{model_paths}}. \cr \cr
#'   \code{fcast_nms}: must be \code{NULL} or a non-negative \code{integer} or
#'     \code{integer}-conformable vector in \code{\link{all_options}},
#'     \code{\link{covariates_options}}, and \code{\link{data_options}} 
#'     \cr \cr
#'   \code{filename}: must be a length-1 \code{character} vector in
#'     \code{\link{append_csv}} and \code{\link{verify_PortalData}}. \cr \cr
#'   \code{from_date}: must be \code{NULL} or a length-1 \code{Date} or 
#'     \code{Date}-conformable vector in \code{\link{plot_cast_point}} and
#'     \code{\link{select_most_ab_spp}}. \cr \cr
#'   \code{from_zenodo}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}} and \code{\link{PortalData_options}}. 
#'     \cr \cr
#'   \code{future_moons}: must be a \code{data.frame} of class \code{moons} 
#'     in \code{\link{add_addl_future_moons}}. \cr \cr
#'   \code{hind_step}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}},
#'     \code{\link{cast_options}}, \code{\link{covariates_options}}, 
#'     \code{\link{data_options}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{hist_cov}: must be a \code{data.frame} of class \code{covariates} 
#'     in \code{\link{prep_fcast_covariates}} and
#'     \code{\link{update_covfcast_options}}. \cr \cr
#'   \code{hist_fcast_file}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, and
#'     \code{\link{data_options}}. \cr \cr
#'   \code{lag}: must be \code{NULL} or a length-1 non-negative \code{integer}
#'     or \code{integer}-conformable vector in \code{\link{lag_covariates}}, 
#'     \code{\link{model_options}}, and \code{\link{pevGARCH}}. \cr \cr
#'   \code{lead}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{plot_cast_point}} and
#'     \code{\link{select_most_ab_spp}}. \cr \cr
#'   \code{lead_time}: must be a length-1 non-negative \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}}, 
#'     \code{\link{covariates_options}}, \code{\link{data_options}}, and
#'     \code{\link{metadata_options}}. \cr \cr
#'   \code{level}: must be \code{NULL} or a length-1 \code{character} vector 
#'     of value \code{"All"} or \code{"Controls"} in 
#'     \code{\link{plot_cast_point}},
#'     \code{\link{plot_cast_ts}}, \code{\link{plot_cast_ts_ylab}},
#'     \code{\link{plot_cov_RMSE_mod_spp}}, 
#'     \code{\link{plot_err_lead_spp_mods}}, \code{\link{select_most_ab_spp}}, 
#'     and \code{\link{select_casts}} or a length-1 \code{character} vector 
#'     of value \code{"Site"} or \code{"Treatment"} in 
#'     \code{\link{AutoArima}}, \code{\link{all_options}}, 
#'     \code{\link{data_options}}, \code{\link{ESSS}}, 
#'     \code{\link{metadata_options}}, \code{\link{nbGARCH}},
#'     \code{\link{pevGARCH}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{local_paths}: must be a \code{character} 
#'     vector in \code{\link{file_paths}}. \cr \cr 
#'   \code{main} must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{cast_options}},
#'     \code{\link{data_options}}, \code{\link{dirtree}}, 
#'     \code{\link{dir_options}}, \code{\link{PortalData_options}},  
#'     \code{\link{models_options}}, and \code{\link{predictions_options}}.
#'     \cr \cr
#'   \code{metadata}: must be of class \code{metadata} in
#'     \code{\link{save_forecast_output}}. \cr \cr
#'   \code{meta_filename}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{metadata_options}}. \cr \cr
#'   \code{meta_save}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{metadata_options}}. \cr \cr
#'   \code{min_lag}: must be a length-1 non-negative \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}},
#'     \code{\link{covariates_options}}, and \code{\link{data_options}}. 
#'     \cr \cr
#'   \code{min_observed}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in 
#'     \code{\link{measure_cast_error}} and 
#'     \code{\link{plot_cov_RMSE_mod_spp}}. \cr \cr
#'   \code{min_plots}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}}, 
#'     \code{\link{cast_options}}, \code{\link{data_options}}, and
#'     \code{\link{rodents_options}}. \cr \cr
#'   \code{min_traps}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}}, 
#'     \code{\link{cast_options}}, \code{\link{data_options}}, and
#'     \code{\link{rodents_options}}. \cr \cr
#'   \code{model}: must be a length-1 \code{character} vector in
#'     \code{\link{model_options}}, \code{\link{plot_cast_point}}, 
#'     \code{\link{plot_cast_ts}}, \code{\link{plot_cast_ts_ylab}},
#'     \code{\link{save_forecast_output}}, and
#'     \code{\link{select_most_ab_spp}}. \cr \cr
#'   \code{models}: must be \code{NULL} or a \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{cast_options}}, 
#'     \code{\link{models_options}}, \code{\link{model_paths}}, and 
#'     \code{\link{select_casts}}, \code{\link{plot_cov_RMSE_mod_spp}}, and
#'     \code{\link{plot_err_lead_spp_mods}}. \cr \cr
#'   \code{model_set}: must be \code{NULL} or a length-1 \code{character} 
#'     vector with value \code{"prefab"} or \code{"wEnsemble"} in 
#'     \code{\link{model_names}}. \cr \cr
#'   \code{mod_covariates}: must be a length-1 \code{logical} vector in 
#'     \code{\link{model_options}}. \cr \cr
#'   \code{mod_type}: must be \code{"pevGARCH"} in
#'     \code{\link{covariate_models}} . \cr \cr
#'   \code{moons}: must be a \code{data.frame} of class \code{moons} in
#'     \code{\link{add_future_moons}}, \code{\link{append_past_moons_to_raw}},
#'     \code{\link{forecast_covariates}}, \code{\link{forecast_ndvi}},
#'     \code{\link{forecast_weather}}, \code{\link{format_moons}},
#'     \code{\link{get_climate_forecasts}}, 
#'     \code{\link{prep_covariates}}, \code{\link{prep_fcast_covariates}},
#'     \code{\link{prep_metadata}}, \code{\link{prep_rodents}}, 
#'     \code{\link{prep_rodents_list}}, \code{\link{trim_moons_fcast}},
#'     \code{\link{update_covariates}}, and
#'     \code{\link{update_covfcast_options}}. \cr \cr
#'   \code{msg}: must be \code{NULL} or a \code{character} vector in
#'     \code{\link{messageq}}. \cr \cr
#'   \code{m_filename}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{moons_options}}. \cr \cr
#'   \code{m_save}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{moons_options}}. \cr \cr
#'   \code{nadot}: must be a length-1 \code{logical} vector in
#'     \code{\link{rodent_spp}}. \cr \cr
#'   \code{ndates}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in 
#'     \code{\link{plot_err_lead_spp_mods}}. \cr \cr
#'   \code{newmoonnumbers}: must be \code{NULL} or a length-1 non-negative 
#'     \code{integer} or \code{integer}-conformable vector in 
#'     \code{\link{select_casts}}. \cr \cr
#'   \code{new_forecast_covariates}: must be a \code{data.frame} of class 
#'     \code{covariates} in \code{\link{append_cov_fcast_csv}}. \cr \cr
#'   \code{nfcnm}: must be a length-1 non-negative \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}},
#'     \code{\link{covariates_options}}, and \code{\link{data_options}}. 
#'     \cr \cr
#'   \code{n_future_moons}: must be a length-1 non-negative \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{moons_options}}.
#'     \cr \cr
#'   \code{options_all}: must be of class \code{all_options} in
#'     \code{\link{casts}}, \code{\link{cleanup_dir}}, 
#'     \code{\link{fill_dir}}, \code{\link{portalcast}},
#'     \code{\link{setup_dir}}, \code{\link{step_casts}},and 
#'     \code{\link{step_hind_forward}}. \cr \cr
#'   \code{options_cast}: must be of class \code{cast_options} in
#'     \code{\link{add_ensemble}}, \code{\link{cast}},
#'     \code{\link{cast_models}}, \code{\link{check_to_skip}}, 
#'     \code{\link{combine_forecasts}}, \code{\link{models_to_cast}}, and
#'     \code{\link{verify_models}}. \cr \cr
#'   \code{options_covariates}: must be of class \code{covariates_options} in
#'     \code{\link{append_cov_fcast_csv}}, 
#'     \code{\link{forecast_covariates}}, \code{\link{forecast_ndvi}},
#'     \code{\link{forecast_weather}}, \code{\link{get_climate_forecasts}}, 
#'     \code{\link{prep_covariates}}, \code{\link{prep_fcast_covariates}},
#'     \code{\link{prep_hist_covariates}}, \code{\link{trim_moons_fcast}}, 
#'     \code{\link{update_covariates}}, and
#'     \code{\link{update_covfcast_options}}. \cr \cr
#'   \code{options_data}: must be of class \code{data_options} in
#'     \code{\link{fill_data}}, \code{\link{prep_data}},
#'     \code{\link{transfer_hist_covariate_forecasts}},
#'     \code{\link{transfer_trapping_table}}, and 
#'     \code{\link{update_data}}. \cr \cr
#'   \code{options_dir}: must be of class \code{dir_options} in
#'     \code{\link{create_dir}}, \code{\link{create_main_dir}}, and
#'     \code{\link{create_sub_dirs}}. \cr \cr
#'   \code{options_metadata}: must be of class \code{metadata_options} in
#'     \code{\link{prep_metadata}}. \cr \cr
#'   \code{options_model}: must be of class \code{model_options} in
#'     \code{\link{model_template}} and \code{\link{write_model}}. \cr \cr
#'   \code{options_models}: must be of class \code{models_options} in
#'     \code{\link{fill_models}}. \cr \cr
#'   \code{options_moons}: must be of class \code{moons_options} in
#'     \code{\link{add_future_moons}}, \code{\link{append_past_moons_to_raw}},
#'     and \code{\link{prep_moons}}. \cr \cr
#'   \code{options_out}: must be an options \code{list} in
#'     \code{\link{dataout}}. \cr \cr
#'   \code{options_PortalData}: must be of class \code{PortalData_options} in
#'     \code{\link{fill_PortalData}}. \cr \cr
#'   \code{options_predictions}: must be of class \code{predictions_options}
#'     in \code{\link{fill_predictions}}. \cr \cr
#'   \code{options_rodents}: must be of class \code{rodents_options} in
#'     \code{\link{enforce_rodents_options}}, \code{\link{prep_rodents}}, 
#'     \code{\link{prep_rodents_list}}, \code{\link{trim_treatment}}, and
#'     \code{\link{update_rodents_list}}. \cr \cr
#'   \code{output}: must be \code{"abundance"} in \code{\link{all_options}}, 
#'     \code{\link{data_options}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{plots}: must be a length-1 \code{character} vector of value
#'     \code{"all"} or \code{"longerm"} in \code{\link{all_options}},
#'     \code{\link{data_options}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{pred_dir}: must be a length-1 \code{character} vector in
#'     \code{\link{compile_aic_weights}} and \code{\link{make_ensemble}}. 
#'     \cr \cr
#'   \code{quiet}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{AutoArima}}, 
#'     \code{\link{cast_options}}, \code{\link{covariates_options}},
#'     \code{\link{create_sub_dir}}, \code{\link{data_options}},
#'     \code{\link{download_predictions}}, \code{\link{dir_options}}, 
#'     \code{\link{ESSS}}, \code{\link{metadata_options}},
#'     \code{\link{models_options}}, \code{\link{model_options}},  
#'     \code{\link{predictions_options}}, \code{\link{PortalData_options}}, 
#'     \code{\link{moons_options}}, \code{\link{messageq}},
#'     \code{\link{nbGARCH}}, \code{\link{pevGARCH}},
#'     \code{\link{rodents_options}}, and \code{\link{verify_PortalData}}.
#'     \cr \cr 
#'   \code{rangex}: must be a length-2 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{plot_cast_ts_xaxis}}.  
#'     \cr \cr
#'   \code{rodents}: must be a \code{data.table} of class \code{rodents} in 
#'     \code{\link{interpolate_abundance}}, \code{\link{is.spcol}},  
#'     \code{\link{remove_spp}}, and \code{\link{trim_treatment}}. \cr \cr 
#'   \code{rodents_list}: must be of class \code{rodents_list} in 
#'     \code{\link{prep_metadata}}. \cr \cr 
#'   \code{r_filename}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{rodents_options}}. \cr \cr
#'   \code{r_save}: must be a length-1 \code{logical} vector in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{rodents_options}}. \cr \cr
#'   \code{save}: must be a length-1 \code{logical} vector in
#'     \code{\link{covariates_options}}, \code{\link{metadata_options}},
#'     \code{\link{moons_options}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{source_name}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}}, \code{\link{covariates_options}},
#'     and \code{\link{data_options}}. \cr \cr
#'   \code{species}: must be \code{NULL} or a \code{character} vector in
#'     \code{\link{plot_cast_point}}, \code{\link{plot_cast_point_yaxis}},
#'     \code{\link{plot_cov_RMSE_mod_spp}}, 
#'     \code{\link{plot_err_lead_spp_mods}}, 
#'     \code{\link{select_casts}}, and \code{\link{select_most_ab_spp}};
#'     specifically length-1 in
#'     \code{\link{plot_cast_ts}} and \code{\link{plot_cast_ts_ylab}}. \cr \cr
#'   \code{species_set}: must be \code{NULL} or a length-1 \code{character} 
#'     vector of value \code{"base"}, \code{"wtotal"}, or \code{"evalplot"} in 
#'     \code{\link{rodent_spp}}. \cr \cr
#'   \code{specific_subs}: must be \code{NULL} or a length-1 \code{character} 
#'     vector in \code{\link{sub_paths}}. \cr \cr 
#'   \code{spp_names}: must be a \code{character} vector in
#'     \code{\link{is.spcol}}. \cr \cr
#'   \code{start}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{all_options}},
#'     \code{\link{cast_options}}, \code{\link{covariates_options}}, 
#'     \code{\link{data_options}}, and \code{\link{rodents_options}}. \cr \cr
#'   \code{start_newmoon}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{plot_cast_ts}}.  
#'     \cr \cr
#'   \code{subs} must be a class-\code{character} vector in
#'     \code{\link{all_options}}, \code{\link{cast_options}}, 
#'     \code{\link{data_options}}, \code{\link{dirtree}}, 
#'     \code{\link{dir_options}}, \code{\link{models_options}}, 
#'     \code{\link{PortalData_options}}, and 
#'     \code{\link{predictions_options}}. \cr \cr
#'   \code{subs_names}: must be \code{NULL} or a \code{character} 
#'     vector in \code{\link{subdirs}}. \cr \cr
#'   \code{subs_type}: must be \code{NULL} or a length-1 \code{character} 
#'     vector in \code{\link{subdirs}}. \cr \cr
#'   \code{sub_path}: must be \code{NULL} or a length-1 \code{character} 
#'     vector in \code{\link{create_sub_dir}}. \cr \cr
#'   \code{tail}: must be a length-1 \code{logical} vector in
#'     \code{\link{lag_covariates}}. \cr \cr
#'   \code{temp_dir}: must be a length-1 \code{character} vector in
#'     \code{\link{save_forecast_output}}. \cr \cr
#'   \code{time}: must be a length-1 \code{logical} vector in
#'     \code{\link{today}}. \cr \cr
#'   \code{tmnt_type}: must be \code{NULL} or a length-1 \code{character} 
#'     vector of value \code{"all"} or \code{"controls"} in 
#'     \code{\link{all_options}}, \code{\link{data_options}}, 
#'     \code{\link{enforce_rodents_options}}, and
#'     \code{\link{rodents_options}}. \cr \cr
#'   \code{to_cleanup}: must be \code{NULL} or a \code{character} vector in
#'     \code{\link{all_options}} and \code{\link{dir_options}}. \cr \cr
#'   \code{topx}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector in \code{\link{select_most_ab_spp}}.
#'     \cr \cr
#'   \code{treatment}: must be \code{NULL} or \code{"control"} in
#'     \code{\link{all_options}}, \code{\link{data_options}}, and
#'     \code{\link{rodents_options}}. \cr \cr
#'   \code{tree}: must be of class \code{dirtree} in
#'     \code{\link{append_observed_to_cast}}, \code{\link{AutoArima}}, 
#'     \code{\link{base_path}}, \code{\link{clear_tmp}},
#'     \code{\link{covariates_options}}, \code{\link{create_tmp}}, 
#'     \code{\link{download_predictions}}, \code{\link{ESSS}}, 
#'     \code{\link{file_paths}}, \code{\link{main_path}}, 
#'     \code{\link{metadata_options}}, \code{\link{model_options}},
#'     \code{\link{model_paths}}, \code{\link{moons_options}},
#'     \code{\link{most_recent_cast}}, \code{\link{most_recent_census}}, 
#'     \code{\link{nbGARCH}}, \code{\link{pevGARCH}}, 
#'     \code{\link{plot_cast_point}},
#'     \code{\link{plot_cast_point_yaxis}}, \code{\link{plot_cast_ts}},
#'     \code{\link{plot_cast_ts_xaxis}}, \code{\link{plot_cast_ts_ylab}},
#'     \code{\link{plot_cov_RMSE_mod_spp}},
#'     \code{\link{plot_err_lead_spp_mods}}, \code{\link{prep_weather_data}},
#'     \code{\link{read_all}}, \code{\link{read_cast}}, 
#'     \code{\link{read_casts}}, \code{\link{read_controls}},
#'     \code{\link{read_covariates}}, \code{\link{read_data}}, 
#'     \code{\link{read_metadata}}, \code{\link{read_moons}},  
#'     \code{\link{rodents_options}}, \code{\link{select_most_ab_spp}},    
#'     \code{\link{sub_paths}}, and \code{\link{verify_PortalData}}. \cr \cr
#'   \code{verbose}: must be a length-1 \code{logical} vector in
#'     \code{\link{cast_is_valid}}. \cr \cr
#'   \code{version}: must be a length-1 \code{character} vector in
#'     \code{\link{all_options}} and \code{\link{PortalData_options}}. \cr \cr
#'   \code{with_census}: must be a length-1 \code{logical} vector in
#'     \code{\link{most_recent_cast}} and \code{\link{plot_cast_point}}. 
#'     \cr \cr
#'   \code{x}: must be a \code{data.frame} or \code{vector} in
#'     \code{\link{na_conformer}} and must simply exist as an object that can
#'      have a class attribute in \code{\link{classy}}. \cr \cr
#'   \code{yr}: must be a length-1 \code{integer} or 
#'     \code{integer}-conformable value after 1970 in 
#'     \code{\link{all_options}}, \code{\link{covariates_options}}, 
#'     and \code{\link{data_options}}. \cr \cr
#'
#' @export
#'
check_arg <- function(arg_name, arg_value, fun_name = NULL){
  if (arg_name == "add"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`add` is not a character")
      }
    }
  }
  if (arg_name == "add_error"){
    if (!("logical" %in% class(arg_value))){
      stop("`add_error` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`add_error` can only be of length = 1")
    }
  }
  if (arg_name == "add_in_window"){
    if (!("logical" %in% class(arg_value))){
      stop("`add_in_window` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`add_in_window` can only be of length = 1")
    }
  }
  if (arg_name == "add_lead"){
    if (!("logical" %in% class(arg_value))){
      stop("`add_lead` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`add_lead` can only be of length = 1")
    }
  }
  if (arg_name == "add_obs"){
    if (!("logical" %in% class(arg_value))){
      stop("`add_obs` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`add_obs` can only be of length = 1")
    }
  }
  if (arg_name == "all"){
    if (!("list" %in% class(arg_value))){
      stop("`all` is not a list")
    }
    if (!all(c("forecast", "aic") %in% names(arg_value))){
      stop("`all` does not have elements named `forecast` and `aic`")
    }
  }
  if (arg_name == "all_forecasts"){
    if (!("data.frame" %in% class(arg_value))){
      stop("`all_forecasts` is not a data frame")
    }
  }
  if (arg_name == "append_fcast_csv"){
    if (!("logical" %in% class(arg_value))){
      stop("`append_fcast_csv` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`append_fcast_csv` can only be of length = 1")
    }
  }
  if (arg_name == "append_missing_to_raw"){
    if (!("logical" %in% class(arg_value))){
      stop("`append_missing_to_raw` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`append_missing_to_raw` can only be of length = 1")
    }
  }
  if (arg_name == "base"){
    if (!("character" %in% class(arg_value))){
      stop("`base` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`base` can only be of length = 1")
    }
  }
  if (arg_name == "cast"){
    if (!("data.frame" %in% class(arg_value))){
      stop("`cast` is not a data frame")
    }
    if(!cast_is_valid(arg_value)){
      stop("`cast` is not valid")
    }
  }
  if (arg_name == "casts"){
    if (!("casts" %in% class(arg_value))){
      stop("`casts` is not of class casts")
    }
  }
  if (arg_name == "cast_date"){
    if (!is.null(arg_value)){
      if (length(arg_value) != 1){
        stop("`cast_date` can only be of length = 1")
      }
      cast_date2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (is.na(cast_date2)){
        stop("`cast_date` is not of class Date or conformable to class Date")
      }
    }
  }
  if (arg_name == "cast_dates"){
    if (!is.null(arg_value)){
      cast_dates2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (is.na(cast_dates2)){
        stop("`cast_dates` is not of class Date or conformable to class Date")
      }
    }
  }
  if (arg_name == "cast_to_check"){
    if (!("data.frame" %in% class(arg_value))){
      stop("`cast_to_check` is not a data frame")
    }
  }
  if (arg_name == "cast_type"){
    if (!("character" %in% class(arg_value))){
      stop("`cast_type` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`cast_type` can only be of length = 1")
    }
    if (!(arg_value %in% c("forecasts", "hindcasts"))){
      stop("`cast_type` can only be `forecasts` or `hindcasts`")
    }
  }
  if (arg_name == "class"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`class` is not a character")
      }
    }
  }
  if (arg_name == "colname"){
    if (!("character" %in% class(arg_value))){
      stop("`colname` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`colname` can only be of length = 1")
    }
  }
  if (arg_name == "confidence_level"){
    if (!is.numeric(arg_value)){
      stop("`confidence_level` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`confidence_level` can only be of length = 1")
    }
    if (arg_value < 0.001 | arg_value > 0.999){
      stop("`confidence_level` is not between 0.001 and 0.999")
    }
  }
  if (arg_name == "controls"){
    if (!("list" %in% class(arg_value))){
      stop("`controls` is not a list")
    }
    if (!all(c("forecast", "aic") %in% names(arg_value))){
      stop("`controls` does not have elements named `forecast` and `aic`")
    }
  }
  if (arg_name == "covariates"){
    if (!("covariates" %in% class(arg_value))){
      stop("`covariates` is not a covariates table")
    }
  }
  if (arg_name == "cov_fcast"){
    if (!("logical" %in% class(arg_value))){
      stop("`cov_fcast` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`cov_fcast` can only be of length = 1")
    }
  }
  if (arg_name == "cov_hist"){
    if (!("logical" %in% class(arg_value))){
      stop("`cov_hist` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`cov_hist` can only be of length = 1")
    }
  }
  if (arg_name == "c_filename"){
    if (!("character" %in% class(arg_value))){
      stop("`c_filename` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`c_filename` can only be of length = 1")
    }
  }
  if (arg_name == "c_save"){
    if (!("logical" %in% class(arg_value))){
      stop("`c_save` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`c_save` can only be of length = 1")
    }
  }
  if (arg_name == "data_name"){
    valid_names <- c("all", "controls", "covariates", "moons", "metadata")
    if (!is.character(arg_value)){
      stop("`data_name` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`data_name` can only be of length = 1")
    }
    if (!(arg_value %in% valid_names)){
      stop("`data_name` is not valid option")
    }
  }
  if (arg_name == "df"){
    if (!("data.frame" %in% class(arg_value))){
      stop("`df` not a data.frame")
    }
  }
  if (arg_name == "download"){
    if (!("logical" %in% class(arg_value))){
      stop("`download` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`download` can only be of length = 1")
    }
  }
  if (arg_name == "download_existing_predictions"){
    if (!("logical" %in% class(arg_value))){
      stop("`download_existing_predictions` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`download_existing_predictions` can only be of length = 1")
    }
  }
  if (arg_name == "drop_spp"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`drop_spp` is not a character")
      }
    }
  }
  if (arg_name == "end"){
    if (!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        stop("`end` is not numeric")
      }
      if (any(arg_value < 1 )| any(arg_value %% 1 != 0)){
        stop("`end` is not a positive integer")
      }
    }
  }
  if (arg_name == "ensemble"){
    if (!("logical" %in% class(arg_value))){
      stop("`ensemble` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`ensemble` can only be of length = 1")
    }
  }
  if (arg_name == "extension"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`extension` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`extension` can only be of length = 1")
      }
      lext <- nchar(arg_value)
      spot <- rep(NA, lext)
      for(i in 1:lext){
        spot[i] <- substr(arg_value, i, i) == "."
      }
      if (sum(spot) != 1){
        stop("`extension` is not an extension")
      }
    }
  }
  if (arg_name == "fcast_nms"){
    if (!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        stop("`fcast_nms` is not numeric")
      }
      if (any(arg_value < 0) | any(arg_value %% 1 != 0)){
        stop("`fcast_nms` is not a non-negative integer")
      }
    }
  }
  if (arg_name == "filename"){
    if (!("character" %in% class(arg_value))){
      stop("`filename` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`filename` can only be of length = 1")
    }
  }
  if (arg_name == "from_date"){
    if (!is.null(arg_value)){
      if (length(arg_value) != 1){
        stop("`from_date` can only be of length = 1")
      }
      from_date2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (is.na(from_date2)){
        stop("`from_date` is not of class Date or conformable to class Date")
      }
    }
  }
  if (arg_name == "from_zenodo"){
    if (!("logical" %in% class(arg_value))){
      stop("`from_zenodo` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`from_zenodo` can only be of length = 1")
    }
  }
  if (arg_name == "future_moons"){
    if (!("moons" %in% class(arg_value))){
      stop("`future_moons` is not an moons table")
    }
  }
  if (arg_name == "hind_step"){
    if (!is.numeric(arg_value)){
      stop("`hind_step` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`hind_step` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`hind_step` is not a positive integer")
    }
  }
  if (arg_name == "hist_cov"){
    if (!("covariates" %in% class(arg_value))){
      stop("`hist_cov` is not a covariates table")
    }
  }
  if (arg_name == "hist_fcast_file"){
    if (!("character" %in% class(arg_value))){
      stop("`hist_fcast_file` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`hist_fcast_file` can only be of length = 1")
    }
  }
  if (arg_name == "lag"){
    if(!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        stop("`lag` is not numeric")
      }
      if (length(arg_value) != 1){
        stop("`lag` can only be of length = 1")
      }
      if (arg_value < 0 | arg_value %% 1 != 0){
        stop("`lag` is not a non-negative integer")
      }
    }
  }
  if (arg_name == "lead"){
    if (!is.numeric(arg_value)){
      stop("`lead` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`lead` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`lead` is not a positive integer")
    }
  }
  if (arg_name == "lead_time"){
    if (!is.numeric(arg_value)){
      stop("`lead_time` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`lead_time` can only be of length = 1")
    }
    if (arg_value < 0 | arg_value %% 1 != 0){
      stop("`lead_time` is not a non-negative integer")
    }
  }
  if (arg_name == "level"){
    if (!is.null(arg_value)){    
      if (!is.character(arg_value)){
        stop("`level` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`level` can only be of length = 1")
      }
      AC_funs <- c("plot_cov_RMSE_mod_spp", "plot_err_lead_spp_mods",
                   "plot_cast_point", "select_most_ab_spp", "select_casts")
      ST_funs <- c("all_options")
      if (fun_name %in% AC_funs){
        if (!(arg_value %in% c("All", "Controls"))){
          stop("`level` must be 'All' or 'Controls'")
        }
      }
      if (fun_name %in% ST_funs){
        if (!(arg_value %in% c("Site", "Treatment"))){
          stop("`level` must be 'Site' or 'Treatment'")
        }
      }
    }
  }
  if (arg_name == "local_paths"){
    if (!("character" %in% class(arg_value))){
      stop("`local_path` is not a character")
    }
  }
  if (arg_name == "main"){
    if (!("character" %in% class(arg_value))){
      stop("`main` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`main` can only be of length = 1")
    }
  }
  if (arg_name == "metadata"){
    if (!("metadata" %in% class(arg_value))){
      stop("`metadata` is not a metadata list")
    }
  }
  if (arg_name == "meta_filename"){
    if (!("character" %in% class(arg_value))){
      stop("`meta_filename` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`meta_filename` can only be of length = 1")
    }
  }
  if (arg_name == "meta_save"){
    if (!("logical" %in% class(arg_value))){
      stop("`meta_save` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`meta_save` can only be of length = 1")
    }
  }
  if (arg_name == "min_lag"){
    if (!is.numeric(arg_value)){
      stop("`min_lag` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`min_lag` can only be of length = 1")
    }
    if (arg_value < 0 | arg_value %% 1 != 0){
      stop("`min_lag` is not a non-negative integer")
    }
  }
  if (arg_name == "min_observed"){
    if (!is.numeric(arg_value)){
      stop("`min_observed` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`min_observed` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`min_observed` is not a positive integer")
    }
  }
  if (arg_name == "min_plots"){
    if (!is.numeric(arg_value)){
      stop("`min_plots` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`min_plots` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`min_plots` is not a positive integer")
    }
  }
  if (arg_name == "min_traps"){
    if (!is.numeric(arg_value)){
      stop("`min_traps` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`min_traps` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`min_traps` is not a positive integer")
    }
  }
  if (arg_name == "model"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`model` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`model` can only be of length = 1")
      }
    }
  }
  if (arg_name == "models"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`models` is not a character")
      }
    }
  }
  if (arg_name == "model_set"){
    if (!is.null(arg_value)){
      if (!is.character(arg_value)){
        stop("`model_set` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`model_set` can only be of length = 1")
      }
      if (!(arg_value %in% c("prefab", "wEnsemble"))){
        stop("`model_set` must be 'prefab' or 'wEnsemble'")
      }
    }
  }
  if (arg_name == "mod_covariates"){
    if (!("logical" %in% class(arg_value))){
      stop("`mod_covariates` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`mod_covariates` can only be of length = 1")
    }
  }
  if (arg_name == "mod_type"){
    if (arg_value != "pevGARCH"){
      stop("only `pevGARCH` supported for `mod_type`")
    }
  }
  if (arg_name == "moons"){
    if (!("moons" %in% class(arg_value))){
      stop("`moons` is not an moons table")
    }
  }
  if (arg_name == "msg"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`msg` is not a character")
      }
    }
  }
  if (arg_name == "m_filename"){
    if (!("character" %in% class(arg_value))){
      stop("`m_filename` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`m_filename` can only be of length = 1")
    }
  }
  if (arg_name == "m_save"){
    if (!("logical" %in% class(arg_value))){
      stop("`m_save` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`m_save` can only be of length = 1")
    }
  }
  if (arg_name == "nadot"){
    if (!("logical" %in% class(arg_value))){
      stop("`nadot` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`nadot` can only be of length = 1")
    }
  }
  if (arg_name == "ndates"){
    if (!is.numeric(arg_value)){
      stop("`ndates` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`ndates` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`ndates` is not a positive integer")
    }
  }
  if (arg_name == "newmoonnumbers"){
    if (!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        stop("`newmoonnumbers` is not numeric")
      }
      if (any(arg_value < 0) | any(arg_value %% 1 != 0)){
        stop("`newmoonnumbers` is not a non-negative integer")
      }
    }
  }
  if (arg_name == "new_forecast_covariates"){
    if (!("covariates" %in% class(arg_value))){
      stop("`new_forecast_covariates` is not a covariates table")
    }
  }
  if (arg_name == "nfcnm"){
    if (!is.numeric(arg_value)){
      stop("`nfcnm` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`nfcnm` can only be of length = 1")
    }
    if (arg_value < 0 | arg_value %% 1 != 0){
      stop("`nfcnm` is not a non-negative integer")
    }
  }
  if (arg_name == "n_future_moons"){
    if (!is.numeric(arg_value)){
      stop("`n_future_moons` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`n_future_moons` can only be of length = 1")
    }
    if (arg_value < 0 | arg_value %% 1 != 0){
      stop("`n_future_moons` is not a non-negative integer")
    }
  }
  if (arg_name == "options_all"){
    if (!("all_options" %in% class(arg_value))){
      stop("`options_all` is not an all_options list")
    }
  }
  if (arg_name == "options_cast"){
    if (!("cast_options" %in% class(arg_value))){
      stop("`options_cast` is not a cast_options list")
    }
  }
  if (arg_name == "options_covariates"){
    if (!("covariates_options" %in% class(arg_value))){
      stop("`options_covariates` is not a covariates_options list")
    }
  }
  if (arg_name == "options_data"){
    if (!("data_options" %in% class(arg_value))){
      stop("`options_data` is not a data_options list")
    }
  }
  if (arg_name == "options_dir"){
    if (!("dir_options" %in% class(arg_value))){
      stop("`options_dir` is not a dir_options list")
    }
  }
  if (arg_name == "options_metadata"){
    if (!("metadata_options" %in% class(arg_value))){
      stop("`options_metadata` is not a metadata_options list")
    }
  }
  if (arg_name == "options_model"){
    if (!("model_options" %in% class(arg_value))){
      stop("`options_model` is not a model_options list")
    }
  }
  if (arg_name == "options_models"){
    if (!("models_options" %in% class(arg_value))){
      stop("`options_models` is not a models_options list")
    }
  }
  if (arg_name == "options_moons"){
    if (!("moons_options" %in% class(arg_value))){
      stop("`options_moons` is not a moons_options list")
    }
  }
  if (arg_name == "options_out"){
    if (!any(grepl("options", class(arg_value)))){
      stop("`options_out` is not an options list")
    }
  }
  if (arg_name == "options_PortalData"){
    if (!("PortalData_options" %in% class(arg_value))){
      stop("`options_PortalData` is not a PortalData_options list")
    }
  }
  if (arg_name == "options_predictions"){
    if (!("predictions_options" %in% class(arg_value))){
      stop("`options_predictions` is not a predictions_options list")
    }
  }
  if (arg_name == "options_rodents"){
    if (!("rodents_options" %in% class(arg_value))){
      stop("`options_rodents` is not a rodents_options list")
    }
  }
  if (arg_name == "output"){
    if (arg_value != "abundance"){
      stop("only `abundance` supported for `output`")
    }
  }
  if (arg_name == "plots"){
    if (!is.character(arg_value)){
      stop("`plots` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`plots` can only be of length = 1")
    }
    if (!(arg_value %in% c("all", "longterm"))){
      stop("`plots` must be 'all' or 'longterm'")
    }
  }
  if (arg_name == "pred_dir"){
    if (!("character" %in% class(arg_value))){
      stop("`pred_dir` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`pred_dir` can only be of length = 1")
    }
  }
  if (arg_name == "quiet"){
    if (!("logical" %in% class(arg_value))){
      stop("`quiet` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`quiet` can only be of length = 1")
    }
  }
  if (arg_name == "rangex"){
    if (!is.numeric(arg_value)){
      stop("`rangex` is not numeric")
    }
    if (length(arg_value) != 2){
      stop("`rangex` can only be of length = 2")
    }
    if (any(arg_value < 1) | any(arg_value %% 1 != 0)){
      stop("`rangex` is not a positive integer")
    }
  }
  if (arg_name == "rodents"){
    if (!("rodents" %in% class(arg_value))){
      stop("`rodents` is not of class rodents")
    }
  }
  if (arg_name == "rodents_list"){
    if (!("rodents_list" %in% class(arg_value))){
      stop("`rodents_list` is not a rodents_list list")
    }
  }
  if (arg_name == "r_filename"){
    if (!("character" %in% class(arg_value))){
      stop("`r_filename` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`r_filename` can only be of length = 1")
    }
  }
  if (arg_name == "r_save"){
    if (!("logical" %in% class(arg_value))){
      stop("`r_save` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`r_save` can only be of length = 1")
    }
  }
  if (arg_name == "save"){
    if (!("logical" %in% class(arg_value))){
      stop("`save` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`save` can only be of length = 1")
    }
  }

  if (arg_name == "source_name"){
    if (!("character" %in% class(arg_value))){
      stop("`source_name` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`source_name` can only be of length = 1")
    }
  }
  if (arg_name == "species"){
    if(!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`species` is not a character")
      }
      if (!all(arg_value %in% rodent_spp("wtotal"))){
        stop("invalid entry in `species`")
      } 
      sp_funs <- c("plot_cast_ts", "plot_cast_ts_ylab")
      if (fun_name %in% sp_funs){
        if (length(arg_value) != 1){
          stop("`species` can only be of length = 1")
        }
      }
    }
  }
  if (arg_name == "species_set"){
    if (!is.null(arg_value)){
      if (!is.character(arg_value)){
        stop("`species_set` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`species_set` can only be of length = 1")
      }
      if (!(arg_value %in% c("base", "wtotal", "evalplot"))){
        stop("`species_set` must be 'base', 'wtotal', or 'evalplot'")
      }
    }
  }
  if (arg_name == "specific_subs"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`specific_subs` is not a character")
      }
    }
  }
  if (arg_name == "spp_names"){
    if (!("character" %in% class(arg_value))){
      stop("`spp_names` is not a character")
    }
  }
  if (arg_name == "start"){
    if (!is.numeric(arg_value)){
      stop("`start` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`start` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`start` is not a positive integer")
    }
  }
  if (arg_name == "start_newmoon"){
    if (!is.numeric(arg_value)){
      stop("`start_newmoon` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`start_newmoon` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`start_newmoon` is not a positive integer")
    }
  }
  if (arg_name == "subs"){
    if (!("character" %in% class(arg_value))){
      stop("`subs` is not a character vector")
    }
  }
  if (arg_name == "subs_names"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`subs_names` is not a character")
      }
    }
  }
  if (arg_name == "subs_type"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`subs_type` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`subs_type` can only be of length = 1")
      }
      if (!(arg_value %in% c("portalcasting"))){
        stop("`subs_type` is not recognized ")
      }
    }
  }
  if (arg_name == "sub_path"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`sub_path` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`sub_path` can only be of length = 1")
      }
    }
  }
  if (arg_name == "tail"){
    if (!("logical" %in% class(arg_value))){
      stop("`tail` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`tail` can only be of length = 1")
    }
  }
  if (arg_name == "temp_dir"){
    if (!("character" %in% class(arg_value))){
      stop("`temp_dir` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`temp_dir` can only be of length = 1")
    }
  }
  if (arg_name == "time"){
    if (!("logical" %in% class(arg_value))){
      stop("`time` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`time` can only be of length = 1")
    }
  }
  if (arg_name == "tmnt_type"){
    if (!is.null(arg_value)){
      if (!is.character(arg_value)){
        stop("`tmnt_type` is not a character")
      }
      if (length(arg_value) != 1){
        stop("`tmnt_type` can only be of length = 1")
      }
      if (!(arg_value %in% c("all", "controls"))){
        stop("`tmnt_type` must be 'all' or 'controls'")
      }
    }
  }
  if (arg_name == "to_cleanup"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        stop("`to_cleanup` is not a character")
      }
    }
  }
  if (arg_name == "topx"){
    if (!is.numeric(arg_value)){
      stop("`topx` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`topx` can only be of length = 1")
    }
    if (arg_value < 1 | arg_value %% 1 != 0){
      stop("`topx` is not a positive integer")
    }
  }
  if (arg_name == "treatment"){
    if (!(is.null(arg_value)) && arg_value != "control"){
      stop("`treatment` must be `NULL` or 'control'")
    }
  }
  if (arg_name == "tree"){
    if (!("dirtree" %in% class(arg_value))){
      stop("`tree` is not a dirtree list")
    }
  }
  if (arg_name == "verbose"){
    if (!("logical" %in% class(arg_value))){
      stop("`verbose` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`verbose` can only be of length = 1")
    }
  }
  if (arg_name == "version"){
    if (!("character" %in% class(arg_value))){
      stop("`version` is not a character")
    }
    if (length(arg_value) != 1){
      stop("`version` can only be of length = 1")
    }
  }
  if (arg_name == "with_census"){
    if (!("logical" %in% class(arg_value))){
      stop("`with_census` is not logical")
    }
    if (length(arg_value) != 1){
      stop("`with_census` can only be of length = 1")
    }
  }
  if (arg_name == "x"){
    if (fun_name == "na_conformer"){
      if (!(is.data.frame(arg_value) | is.vector(arg_value))){
        stop("`x` is not a data.frame or vector")
      }
    }
    if (fun_name == "classy"){
    }
  }
  if (arg_name == "yr"){
    if (!is.numeric(arg_value)){
      stop("`yr` is not numeric")
    }
    if (length(arg_value) != 1){
      stop("`yr` can only be of length = 1")
    }
    if (arg_value < 1970 | arg_value %% 1 != 0){
      stop("`yr` is not an integer after 1970")
    }
  }
}

