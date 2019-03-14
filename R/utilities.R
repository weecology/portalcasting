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
#'   \code{"NA"}
#'
#' @export
#'
na_conformer <- function(x, colname = "species"){

  if (is.vector(x)){
    naentries <- which(is.na(x))
    x[naentries] <- "NA"
  } else if (is.data.frame(x)){
    nasppname <- which(is.na(x[ , colname]))
    if (length(nasppname) > 0){
      x[nasppname, colname] <- "NA"
    }
  } else {
    stop("`x` must be a vector or a data frame")
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
#' @param options_out an options list that includes a save element and 
#'   a filename element, and potentially a class element.
#'
#' @return df (as input)
#'
#' @export
#'
dataout <- function(df, options_out = moons_options()){
  if (! "data.frame" %in% class(df)){
    stop("`df` not a data.frame")
  }
  if (!is.null(options_out$save)){
    if (options_out$save){
      file_path(options_out$tree, paste0("data/", options_out$filename)) %>%
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
  if (length(filename) > 1){
    stop("`filename` can only be of length = 1")
  }
  if (!is.character(filename)){
    stop("`filename` not a character")
  }
  if (! "data.frame" %in% class(df)){
    stop("`df` not a data.frame")
  }
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
#' @param pred_name \code{character} name for the predictor column (to match
#'   variable model output names).
#'
#' @return Two-element \code{list} of means and interval values for a 
#'   0-abundance forecast to be used as a filler when a model fails or there 
#'   is no non-0 historical abundance.
#'
#' @export
#'
fcast0 <- function(nfcnm, pred_name = "pred"){
  if (length(nfcnm) > 1){
    stop("`nfcnm` can only be of length = 1")
  }
  if (nfcnm %% 1 != 0){
    stop("`nfcnm` not an integer")
  }
  if (!is.character(pred_name)){
    stop("`pred_name` not a character")
  }
  if (length(pred_name) > 1){
    stop("`pred_name` can only be of length = 1")
  }
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list(mean_0, interval = int_0)
  names(out)[1] <- pred_name
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
  if (!is.character(class)){
    stop("`class` must be a character vector")
  }
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
#' @param col_to_check A single \code{character} value of the column to use
#'   to remove incomplete entries. 
#'
#' @return df without any incomplete entries. 
#'
#' @export
#'
remove_incompletes <- function(df, col_to_check){
  if (!("data.frame" %in% class(df))){
    stop("`df` not a data.frame class object")
  }
  if (length(col_to_check) > 1){
    stop("`col_to_check` currently only able to length 1")
  }
  if (!("character" %in% class(col_to_check))){
    stop("`col_to_check` not of class character")
  }
  incompletes <- which(is.na(df[ , col_to_check]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}


#' @title Check arguments
#'
#' @description Omnibus function to verify the validity (class, size, etc.) 
#'   of the inputs to the functions.
#'
#' @param base \code{character} name of the base folder where the directory 
#'   should or does exist. Default \code{"."} (working directory). Will be 
#'   created if it doesn't exist.
#'
#' @param main \code{character} name of the main folder within the base that
#'   contains the subdirectories. Default \code{""} (no main level included). 
#'
#' @param subs \code{character} vector naming the specific subdirectories
#'   within the portalcasting directory tree. Default \code{subdirs(type = 
#'   "portalcasting")} sets the subdirectories as \code{"predictions"}, 
#'   \code{"models"}, \code{"PortalData"}, \code{"data"}, and \code{"tmp"}. 
#'   It is generally not advised to change the subdirectories. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param cast_date \code{Date} from which future is defined, typically 
#'   today's date (using \code{\link{today}}).
#'
#' @param cast_dates \code{Date}s the predictions were made. Used to select 
#'   the files in the predictions subdirectory. Can be length 1 or more and if 
#'   \code{NULL} (default), selects all available -casts.
#'
#' @param append_missing_to_raw \code{logical} indicator dictating if the 
#'   missing moon dates should be appended to the raw data file (should be 
#'   \code{TRUE} to allow the imported portalr functions to work properly).
#'
#' @param m_save \code{logical} indicator controlling if the moons data should 
#'   be saved out.
#'
#' @param m_filename \code{character} name of the file for saving moons data.
#'
#' @param tmnt_type Treatment type: \code{"all"} or \code{"controls"}.
#'
#' @param start \code{integer} (or integer \code{numeric}) newmoon number of 
#'   the first sample to be included. Default value is \code{217}, 
#'   corresponding to \code{1995-01-01}.
#'
#' @param end \code{integer} (or integer \code{numeric}) newmoon number of the
#'   last sample to be included. Default value is \code{NULL}, which equates
#'   to the most recently included sample. If \code{cast_type} is 
#'   \code{"hindcasts"} and \code{end} is \code{NULL}, default becomes 
#'   \code{490:403}, corresponding to \code{2017-01-01} back to
#'   \code{2010-01-01}.
#'
#' @param hind_step \code{integer} (or integer \code{numeric}) iteration 
#'   parameter used to work across the input values for \code{end}. Default is
#'   \code{1}, which is likely all it should be run using.
#'
#' @param drop_spp \code{character}-valued vector of species names to drop 
#'   from the forecasting.
#'
#' @param min_plots \code{integer} (or integer \code{numeric}) of the minimum 
#'   number of plots surveyed for a survey to be used.
#'
#' @param min_traps \code{integer} (or integer \code{numeric}) of the minimum 
#'   number of traps trapped for a plot to be used.
#'  
#' @param tail \code{logical} indicator if the data lagged to the tail end 
#'   should be retained.
#'
#' @param level For data collection, \code{character} input of \code{level} 
#'   for \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}. \cr \cr For -casts, \code{character} value of the 
#'   \code{level} of interest  (\code{"All"} or \code{"Controls"}) in the 
#'   -casts. \cr \cr Differentiation by \code{toggle} (\code{NULL} for
#'   data collection and a toggle character value including \code{"plot"}
#'   for -casts).
#'
#' @param treatment \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param plots \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param output \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param r_save \code{logical} value indicating if the rodent data should be 
#'   saved out.
#'
#' @param r_filename \code{character} name of the file to save the rodent data
#'   in.
#'
#' @param cov_hist \code{logical} indicator of whether or not historical 
#'   covariates are to be included.
#'
#' @param cov_fcast \code{logical} indicator whether or not forecasted 
#'   covariates are to be included.
#'
#' @param yr \code{numeric} value of the year of today's date.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) number of moons
#'   into the future the rodents are to be forecast.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'   covariate lag time used in any model.
#'
#' @param fcast_nms \code{integer} (or integer \code{numeric}) vector of 
#'   newmoon numbers to be forecast for covariates.
#'
#' @param nfcnm \code{integer} (or integer \code{numeric}) number of forecast 
#'   newmoons for covariates.
#'
#' @param append_fcast_csv \code{logical} indicator controlling if the new 
#'   forecast should be appended to the historical forecasts for the purposes 
#'   of hindcasting.
#'
#' @param hist_fcast_file \code{character} name of the file where the 
#'   historical covariate forecasts are held.
#'
#' @param source_name \code{character} value for the name to give the 
#'   covariaate forecast. Currently is \code{"current_archive"}. Previous to
#'   \code{"current_archive"}, the data were retroactively filled in and are 
#'   given the source name \code{"retroactive"}.
#'
#' @param c_save \code{logical} indicator for if the covariate data should be 
#'   saved out.
#'
#' @param c_filename \code{character} value for the name of the covariate file 
#'   for the saving if \code{c_save} is \code{TRUE}.
#'
#' @param cast_type -cast type: \code{"forecasts"} or \code{"hindcasts"}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param meta_save \code{logical} indicator for if the metadata should be
#'   saved out
#'
#' @param meta_filename \code{character} value for the name of the metadata 
#'   file for the saving if \code{c_save} is \code{TRUE}.
#'
#' @param download_existing_predictions \code{logical} indicator for if the 
#'   existing predictions files should be retrieved from the 
#'   \href{https://github.com/weecology/portalPredictions}{portalPredictions 
#'   repo}.
#'
#' @param models \code{character} vector of the names of models to include 
#'   in a cast of the pipeline.
#'   
#' @param model A singular \code{character} value for a \code{model} or 
#'   \code{"Ensemble"} in certain situations.
#'
#' @param ensemble \code{logical} indicator of whether to create an ensemble
#'   model.
#'
#' @param version \code{character} values of the version number or 
#'   \code{"latest"} (default) for the Portal Data to be download.
#'
#' @param from_zenodo \code{logical} indicator of whether or not the Portal 
#'   Data should come from Zenodo (come from GitHub if not).
#'
#' @param to_cleanup \code{character} vector of subdirectory names to
#'   cleanup after completion of casting.
#' 
#' @param options_all Class-\code{all_options} list containing all options 
#'   available for controlling the set up and population of the directory (see 
#'   \code{\link{all_options}}). 
#' 
#' @param options_dir Class-\code{dir_options} list containing all options 
#'   available for controlling the set up of the directory (see 
#'   \code{\link{dir_options}}). 
#'
#' @param options_PortalData Class-\code{PortalData_options} list containing 
#'   available for controlling the set up and population of the PortalData
#'   folder in the portalcasting directory (see 
#'   \code{\link{PortalData_options}}). 
#'
#' @param options_data Class-\code{data_options} list containing available
#'   for controlling the set up and population of the data folder in the 
#'   portalcasting directory (see \code{\link{data_options}}).
#'
#' @param options_predictions Class-\code{predictions_options} list containing 
#'   available for controlling the set up and population of the predictions
#'   folder in the portalcasting directory (see 
#'   \code{\link{predictions_options}}). 
#'
#' @param options_models Class-\code{models_options} list containing 
#'   available for controlling the set up and population of the models
#'   folder in the portalcasting directory (see 
#'   \code{\link{models_options}}). 
#'
#' @param options_covariates Class-\code{covariates_options} list of 
#'   options for the covariate data. See \code{\link{covariates_options}}.
#'
#' @param options_rodents A class-\code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation.
#'
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @param options_metadata A class-\code{metadata_options} \code{list} of 
#'   settings controlling the metadata data creation.
#'
#' @param options_moons A class-\code{moons_options} \code{list} of options 
#'   for the moons data. See \code{\link{moons_options}}.
#'
#' @param path The normalized path of the specific subdirectory folder to be 
#'   created in the directory tree as a \code{character} value (see 
#'   \code{\link{normalizePath}}, \code{\link{sub_path}}).
#'
#' @param filename \code{character} name of the file for the saving.
#'
#' @param n_future_moons \code{integer} (or integer \code{numeric}) value for 
#'   the number of future moons to add.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param save \code{logical} indicator if the specific data should be saved 
#'   out.
#'
#' @param species \code{character} vector of the species codes (or 
#'   \code{"total"} for the total across species) to be selected from or 
#'   \code{NULL} to include all species and the total. Default set to 
#'   \code{rodent_spp(set = "evalplot")}, the only setting for which the
#'   plot is reliably coded/ 
#'
#' @param min_observed \code{integer} value for the minimum number of observed
#'   values needed for a -cast to be retained in the output table. Default is
#'   \code{1}, which returns all -casts with any observations. To include all
#'   -casts (even those without any evaluations), set to \code{0}. 
#'
#' @param ndates \code{integer} number of -cast issue dates to include.
#'
#' @param from_date \code{Date} to be used as a reference of when to count
#'   the \code{lead} from. If \code{NULL} (default), for 
#'   \code{cast_type = "forecasts"}, \code{from_date = cast_date} and 
#'   \code{plot_cast_point} is not yet reliable for 
#'   \code{cast_type = "hindcasts"}.
#'
#' @param with_census \code{logical} toggle if the plot should include the
#'   observed data collected during the predicted census.
#'
#' @param rangex \code{integer}-conformable vector of two values corresponding
#'   to the minimum and maximum newmoonnumbers plotted. 
#'
#' @param start_newmoon \code{integer}-conformable newmoon number used as the
#'   the minimum x value for the plot. 
#'
#' @param add_obs \code{logical} indicator if values observed during the 
#'   -cast time should be added (default is \code{TRUE}).
#'
#' @param covariates Class-\code{covariates} \code{data.frame} of 
#'   historical covariate data generated by 
#'   \code{\link{prep_hist_covariates}}.
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}. For input 
#'   into \code{forecast_weather} and \code{forecast_ndvi}, \code{moons} 
#'   should be trimmed by \code{\link{trim_moons_fcast}}.
#'
#' @param future_moons A \code{moons}-class \code{data.frame} forward from 
#'   the historic moons table (produced by \code{\link{prep_moons}}), as
#'   generated by \code{\link[portalr]{get_future_moons}}. 
#' 
#' @param new_forecast_covariates Class-\code{covariates} \code{data.frame} of
#'   forecasted covariate data generated by 
#'   \code{\link{forecast_covariates}}.
#'
#' @param name \code{character}-valued name of the model (MUST match the 
#'   function name).
#'
#' @param mod_covariates \code{logical} indicator for if the model requires 
#'   covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NULL} if \code{covariates} is \code{FALSE}.
#'
#' @param subs_names Either [1] names of additional subdirectories to add to
#'   the vector set up by \code{type} or [2] an optional way to input all 
#'   subdirectory names (requires \code{type = NULL}, which is the default). 
#'
#' @param subs_type \code{character} name for quick generation of subdirectory
#'   vector. Presently only defined for \code{"portalcasting"}.
#'
#' @param specific_sub \code{character}-value name of the specific 
#'   subdirectory/subdirectories of interest.
#'
#' @param extension \code{character} file extension (including the period).
#'
#' @param local_path \code{character} file path within the \code{main} level 
#'   of the portalcasting directory.
#'
#' @param add \code{character} vector of name(s) of model(s) to add to the 
#'   setup by \code{set}.
#'
#' @param set \code{characher} value of the type of model (currently only 
#'   support for \code{"prefab"}). Use \code{NULL} to build a custom set
#'   from scratch via \code{add}.
#'
#' @param options_model A class-\code{model_options} \code{list} of
#'   options used to set up a general model script. See 
#'   \code{\link{model_options}}.
#'
#' @param hist_cov Historical covariate data table as a code{covariates}-class 
#'   \code{data.frame}, returned from \code{\link{prep_hist_covariates}}.
#'
#' @param rodents_list A class-\code{rodents_list} \code{list} of two class-
#'   \code{rodents} \code{data.frame}s, \code{all} (abundances on all plots)
#'   and \code{controls} (abundances on control plots only).
#'
#' @param lead \code{integer}-conformable lead of the newmoon number used to
#'   select the data plotted. 
#'
#' @param download \code{logical} indicator of whether the download should 
#'   actually happen. Should be \code{TRUE} except for testing purposes.
#'
#' @param rodents Class-\code{rodents} \code{data.table} of rodent data.
#'
#' @param data_name \code{character} representation of the data needed.
#'   Current options include \code{"all"}, \code{"controls"},
#'   \code{"covariates"}, \code{"moons"}, and \code{"metadata"}.
#'
#' @param toggle \code{character} value indicating special aspects of 
#'   checking. 
#'
#' @examples
#' \dontrun{
#'
#' check_args()
#' }
#'
#' @export
#'
check_args <- function(toggle = NULL, base = ".", main = "", 
                       subs = NULL, tail = FALSE,
                       quiet = FALSE, 
                       cast_date = today(), append_missing_to_raw = TRUE, 
                       m_save = TRUE, m_filename = "moons.csv", 
                       tmnt_type = NULL, start = 217, end = NULL, 
                       hind_step = 1, drop_spp = "PI", min_plots = 24, 
                       min_traps = 1, level = "Site", treatment = NULL, 
                       plots = "all", output = "abundance", r_save = TRUE, 
                       r_filename = "all.csv", cov_hist = TRUE, 
                       cov_fcast = TRUE, 
                       yr = as.numeric(format(today(), "%Y")), lead_time = 12,
                       min_lag = 6, fcast_nms = NULL, nfcnm = 0,
                       append_fcast_csv = TRUE, 
                       hist_fcast_file = "covariate_forecasts.csv",
                       source_name = "current_archive", c_save = TRUE, 
                       c_filename = "covariates.csv", cast_type = "forecasts",
                       confidence_level = 0.9, meta_save = TRUE, 
                       meta_filename = "metadata.yaml",
                       download_existing_predictions = FALSE, 
                       models = NULL, ensemble = TRUE, download = FALSE,
                       version = "latest", from_zenodo = TRUE,
                       n_future_moons = 12, save = TRUE,
                       filename = "moons.csv", tree = NULL,
                       to_cleanup = c("tmp", "PortalData"), 
                       options_all = NULL, options_moons = NULL,
                       options_dir = NULL,
                       options_PortalData = NULL,
                       options_data = NULL,
                       options_predictions = NULL,
                       options_models = NULL, options_model = NULL, 
                       options_covariates = NULL, options_rodents = NULL,
                       options_cast = NULL, options_metadata = NULL,
                       path = NULL, species = rodent_spp(set = "evalplot"),
                       cast_dates = NULL, min_observed = 1, ndates = 3,
                       from_date = today(), with_census = FALSE,
                       rangex = 2:3, start_newmoon = 300, add_obs = TRUE,
                       moons = NULL, covariates = NULL, hist_cov = NULL,
                       new_forecast_covariates = NULL, name = "AutoArima",
                       lag = NULL, mod_covariates = FALSE,
                       subs_names = NULL, subs_type = "portalcasting",
                       specific_sub = NULL, extension = ".R",
                       local_path = "data/all.csv", rodents = NULL,
                       model = "AutoArima", set = "prefab", add = NULL,
                       lead = 1, rodents_list = NULL, future_moons = NULL,
                       data_name = "all"){
  if(is.null(toggle)){
    toggle <- "null"
  }
  valid_names <- c("all", "controls", "covariates", "moons", "metadata")
  if (length(data_name) > 1){
    stop("`data_name` can only be of length = 1")
  }
  if (!is.character(data_name)){
    stop("`data_name` is not a character")
  }
  if (!any(valid_names %in% data_name)){
    stop("`data_name` is not valid option")
  } 

  if (!("logical" %in% class(download))){
    stop("`download` is not logical")
  }
  if (length(base) > 1){
    stop("`base` can only be of length = 1")
  }
  if (length(main) > 1){
    stop("`main` can only be of length = 1")
  }
  if (!is.character(base)){
    stop("`base` is not a character")
  }
  if (!is.character(main)){
    stop("`main` is not a character")
  }
  if (!is.null(subs) & !("subdirs" %in% class(subs))){
    stop("`subs` is not NULL or a subdirs list")
  }
  if (!("logical" %in% class(quiet))){
    stop("`quiet` is not of class logical")
  }
  if (length(quiet) > 1){
    stop("`quiet` can only be of length = 1")
  }
  if (!("Date" %in% class(cast_date))){
    stop("`cast_date` is not of class Date")
  }
  if (length(cast_date) > 1){
    stop("`cast_date` can only be of length = 1")
  }
  if (!("logical" %in% class(append_missing_to_raw))){
    stop("`append_missing_to_raw` is not of class logical")
  }
  if (length(append_missing_to_raw) > 1){
    stop("`append_missing_to_raw` can only be of length = 1")
  }
  if (!("logical" %in% class(m_save))){
    stop("`m_save` is not of class logical")
  }
  if (length(m_save) > 1){
    stop("`m_save` can only be of length = 1")
  }
  if (!is.null(tmnt_type)){
    if(tmnt_type != "all" & tmnt_type != "controls"){
      stop("`tmnt_type` must be `NULL`, 'all', or 'controls'")
    }
  }
  if (length(start) > 1){
    stop("`start` can only be of length = 1")
  }
  if (!("numeric" %in% class(start)) & !("integer" %in% class(start))){
    stop("`start` is not of class numeric or integer")
  }
  if(start < 0 | start %% 1 != 0){
    stop("`start` is not a non-negative integer")
  }
  if (!is.null(end)){
    if (!("numeric" %in% class(end)) & !("integer" %in% class(end))){
      stop("`end` is not of class numeric or integer")
    }
    if(any(end < 0 | end %% 1 != 0)){
      stop("`end` is not a non-negative integer")
    }
  }
  if (length(hind_step) > 1){
    stop("`hind_step` can only be of length = 1")
  }
  if (!("numeric" %in% class(hind_step)) & 
      !("integer" %in% class(hind_step))){
    stop("`hind_step` is not of class numeric or integer")
  }
  if(hind_step < 0 | hind_step %% 1 != 0){
    stop("`hind_step` is not a non-negative integer")
  }
  if (!is.null(drop_spp) & !("character") %in% class(drop_spp)){
    stop("`drop_spp` is not NULL or a character")
  }  
  if (length(min_plots) > 1){
    stop("`min_plots` can only be of length = 1")
  }
  if (!("numeric" %in% class(min_plots)) & 
      !("integer" %in% class(min_plots))){
    stop("`min_plots` is not of class numeric or integer")
  }
  if(min_plots < 0 | min_plots %% 1 != 0){
    stop("`min_plots` is not a non-negative integer")
  }
  if (length(min_traps) > 1){
    stop("`min_traps` can only be of length = 1")
  }
  if (!("numeric" %in% class(min_traps)) &
     !("integer" %in% class(min_traps))){
    stop("`min_traps` is not of class numeric or integer")
  }
  if(min_traps < 0 | min_traps %% 1 != 0){
    stop("`min_traps` is not a non-negative integer")
  }
  if (!(is.null(treatment)) && treatment != "control"){
    stop("`treatment` must be `NULL` or 'control'")
  }
  if (toggle == "null"){
    if (level != "Site" & level != "Treatment"){
      stop("`level` for portalr must be 'Site' or 'Treatment'")
    }
  } else if (grepl("plot", toggle)){
    if (!is.character(level)){
      stop("`level` for -casts is not a character")
    }
    if (length(level) > 1){
      stop("`level` for -casts can only be of length = 1")
    }
    if (!(level %in% c("All", "Controls"))){
      stop("`level` for -casts must be 'All' or 'Controls'")
    }
  }
  if (output != "abundance"){
    stop("`output` must be 'abundance'")
  }
  if (plots != "all" & plots != "longterm"){
    stop("`plots` must be 'all' or 'longterm'")
  }
  if (!("logical" %in% class(r_save))){
    stop("`r_save` is not of class logical")
  }
  if (length(r_save) > 1){
    stop("`r_save` can only be of length = 1")
  }
  if (!is.character(r_filename)){
    stop("`r_filename` is not a character")
  }
  if (length(r_filename) > 1){
    stop("`r_filename` can only be of length = 1")
  }
  if (!("logical" %in% class(cov_hist))){
    stop("`cov_hist` is not of class logical")
  }
  if (length(cov_hist) > 1){
    stop("`cov_hist` can only be of length = 1")
  }
  if (!("logical" %in% class(cov_fcast))){
    stop("`cov_fcast` is not of class logical")
  }
  if (length(cov_fcast) > 1){
    stop("`cov_fcast` can only be of length = 1")
  }
  if (length(yr) > 1){
    stop("`yr` can only be of length = 1")
  }
  if (!("numeric" %in% class(yr)) & !("integer" %in% class(yr))){
    stop("`yr` is not of class numeric or integer")
  }
  if(yr < 1970 | yr %% 1 != 0){
    stop("`yr` is not an integer after 1970")
  }
  if (length(lead_time) > 1){
    stop("`lead_time` can only be of length = 1")
  }
  if (!("numeric" %in% class(lead_time)) & 
      !("integer" %in% class(lead_time))){
    stop("`lead_time` is not of class numeric or integer")
  }
  if(lead_time < 0 | lead_time %% 1 != 0){
    stop("`lead_time` is not a non-negative integer")
  }

  if (length(lead) > 1){
    stop("`lead` can only be of length = 1")
  }
  if (!("numeric" %in% class(lead)) & 
      !("integer" %in% class(lead))){
    stop("`lead` is not of class numeric or integer")
  }
  if(lead < 1 | lead %% 1 != 0){
    stop("`lead` is not a positive integer")
  }



  if (length(min_lag) > 1){
    stop("`min_lag` can only be of length = 1")
  }
  if (!("numeric" %in% class(min_lag)) & !("integer" %in% class(min_lag))){
    stop("`min_lag` is not of class numeric or integer")
  }
  if(min_lag < 0 | min_lag %% 1 != 0){
    stop("`min_lag` is not a non-negative integer")
  }
  if (!is.null(fcast_nms)){
    if (!("numeric" %in% class(fcast_nms)) & 
        !("integer" %in% class(fcast_nms))){
      stop("`fcast_nms` is not of class numeric or integer")
    }
    if(any(fcast_nms < 0 | fcast_nms %% 1 != 0)){
      stop("`fcast_nms` is not a non-negative integer")
    }
  }
  if (!is.null(nfcnm)){
    if (length(nfcnm) > 1){
      stop("`nfcnm` can only be of length = 1")
    }
    if (!("numeric" %in% class(nfcnm)) & !("integer" %in% class(nfcnm))){
      stop("`nfcnm` is not of class numeric or integer")
    }
    if(any(nfcnm < 0 | nfcnm %% 1 != 0)){
      stop("`nfcnm` is not a non-negative integer")
    }
  }
  if (!("logical" %in% class(append_fcast_csv))){
    stop("`append_fcast_csv` is not of class logical")
  }
  if (length(append_fcast_csv) > 1){
    stop("`append_fcast_csv` can only be of length = 1")
  }
  if (!("logical" %in% class(c_save))){
    stop("`c_save` is not of class logical")
  }
  if (length(c_save) > 1){
    stop("`c_save` can only be of length = 1")
  }
  if (!("logical" %in% class(meta_save))){
    stop("`meta_save` is not of class logical")
  }
  if (length(meta_save) > 1){
    stop("`meta_save` can only be of length = 1")
  }
  if (!("logical" %in% class(download_existing_predictions))){
    stop("`download_existing_predictions` is not of class logical")
  }
  if (length(download_existing_predictions) > 1){
    stop("`download_existing_predictions` can only be of length = 1")
  }
  if (!("logical" %in% class(ensemble))){
    stop("`ensemble` is not of class logical")
  }
  if (length(ensemble) > 1){
    stop("`ensemble` can only be of length = 1")
  }
  if (!("logical" %in% class(from_zenodo))){
    stop("`from_zenodo` is not of class logical")
  }
  if (length(from_zenodo) > 1){
    stop("`from_zenodo` can only be of length = 1")
  }
  if (!is.character(hist_fcast_file)){
    stop("`hist_fcast_file` is not a character")
  }
  if (length(hist_fcast_file) > 1){
    stop("`hist_fcast_file` can only be of length = 1")
  }
  if (!is.character(c_filename)){
    stop("`c_filename` is not a character")
  }
  if (length(c_filename) > 1){
    stop("`c_filename` can only be of length = 1")
  }
  if (!is.character(meta_filename)){
    stop("`meta_filename` is not a character")
  }
  if (length(meta_filename) > 1){
    stop("`meta_filename` can only be of length = 1")
  }
  if (!is.character(m_filename)){
    stop("`m_filename` is not a character")
  }
  if (length(m_filename) > 1){
    stop("`m_filename` can only be of length = 1")
  }
  if (!is.character(source_name)){
    stop("`source_name` is not a character")
  }
  if (length(source_name) > 1){
    stop("`source_name` can only be of length = 1")
  }
  if (!is.character(cast_type)){
    stop("`cast_type` is not a character")
  }
  if (length(cast_type) > 1){
    stop("`cast_type` can only be of length = 1")
  }
  if (cast_type!= "forecasts" & cast_type != "hindcasts"){
    stop("`cast_type` can only be 'forecasts' or 'hindcasts'")
  }
  if (!is.character(version)){
    stop("`version` is not a character")
  }
  if (length(version) > 1){
    stop("`version` can only be of length = 1")
  }
  if (length(confidence_level) > 1){
    stop("`confidence_level` can only be of length = 1")
  }
  if (!("numeric" %in% class(confidence_level))){
    stop("`confidence_level` is not of class numeric")
  }
  if (confidence_level < 0.001 | confidence_level > 0.999){
    stop("`confidence_level` is not between 0.001 and 0.999")
  }
  if (!(is.null(models)) & !("character" %in% class(models))){
    stop("`models` must be NULL or of class character")
  }
  if (length(model) > 1){
      stop("`model` can only be of length = 1 for plotting")
  }
  if (!("character" %in% class(model))){
    stop("`model` must be of class character")
  }
  if (!("logical" %in% class(save))){
    stop("`save` is not of class logical")
  }
  if (length(save) > 1){
    stop("`save` can only be of length = 1")
  }
  if (!is.character(filename)){
    stop("`filename` is not a character")
  }
  if (length(filename) > 1){
    stop("`filename` can only be of length = 1")
  }
  if (!is.null(n_future_moons)){
    if (length(n_future_moons) > 1){
      stop("`n_future_moons` can only be of length = 1")
    }
    if (!("numeric" %in% class(n_future_moons)) &
        !("integer" %in% class(n_future_moons))){
      stop("`n_future_moons` is not of class numeric or integer")
    }
    if(any(n_future_moons < 0 | n_future_moons %% 1 != 0)){
      stop("`n_future_moons` is not a non-negative integer")
    }
  }
  if (!is.null(tree) & 
     !("dirtree" %in% class(tree))){
    stop("`tree` is not NULL or of class dirtree")
  }
  if (!is.null(to_cleanup) & !is.character(to_cleanup)){
    stop("`to_cleanup` is not NULL or a character")
  }
  if (!is.null(options_all) & 
      !("all_options" %in% class(options_all))){
    stop("`options_all` is not NULL or an all_options list")
  }
  if (!is.null(options_dir) & 
      !("dir_options" %in% class(options_dir))){
    stop("`options_dir` is not NULL or a dir_options list")
  }
  if (!is.null(options_PortalData) & 
      !("PortalData_options" %in% class(options_PortalData))){
    stop("`options_PortalData` is not NULL or a PortalData_options list")
  }
  if (!is.null(options_data) & 
      !("data_options" %in% class(options_data))){
    stop("`options_data` is not NULL or a data_options list")
  }
  if (!is.null(options_metadata) & 
      !("metadata_options" %in% class(options_metadata))){
    stop("`options_metadata` is not NULL or a metadata_options list")
  }

  if (!is.null(options_cast) & 
      !("cast_options" %in% class(options_cast))){
    stop("`options_cast` is not NULL or a cast_options list")
  }

  if (!is.null(options_moons) & 
      !("moons_options" %in% class(options_moons))){
    stop("`options_moons` is not NULL or a moons_options list")
  }

  if (!is.null(options_predictions) & 
      !("predictions_options" %in% class(options_predictions))){
    stop("`options_predictions` is not NULL or a predictions_options list")
  }
  if (!is.null(path) & !("character" %in% class(path))){
    stop("`path` is not NULL or a character")
  }
  if (!is.null(options_models) & 
      !("models_options" %in% class(options_models))){
    stop("`options_models` is not NULL or a models_options list")
  }
  if (!is.null(options_model) & 
      !("model_options" %in% class(options_model))){
    stop("`options_model` is not NULL or a model_options list")
  }
  if (!is.null(options_rodents) & 
      !("rodents_options" %in% class(options_rodents))){
    stop("`options_rodents` is not NULL or a rodents_options list")
  }
  if (!is.null(options_covariates) & 
      !("covariates_options" %in% class(options_covariates))){
    stop("`options_covariates` is not NULL or a covariates_options list")
  }
  if (!is.null(species)){
    if (!("character" %in% class(species))){
      stop("`species` is not a character")
    }
    if (!all(species %in% rodent_spp("wtotal"))){
      stop("invalid entry in `species`")
    } 
    if(grepl("1sp", toggle)){
      if (length(species) > 1){
        stop("`species` can only be of length = 1")
      }  
    }
  }
  if (!is.null(cast_dates)){
    cast_dates2 <- tryCatch(as.Date(cast_dates), error = function(x){NA})
    if (is.na(cast_dates2)){
      stop("`cast_dates` is not of class Date or conformable to class Date")
    }
  }
  if (!is.numeric(min_observed)){
    stop("`min_observed` is not numeric")
  }
  if (length(min_observed) > 1){
    stop("`min_observed` can only be of length = 1")
  }
  if (min_observed < 1 | min_observed %% 1 != 0){
    stop("`min_observed` is not a positive integer")
  }

  if (!is.numeric(ndates)){
    stop("`ndates` is not numeric")
  }
  if (length(ndates) > 1){
    stop("`ndates` can only be of length = 1")
  }
  if (ndates < 1 | ndates %% 1 != 0){
    stop("`ndates` is not a positive integer")
  }



  if (!("Date" %in% class(from_date))){
    stop("`from_date` is not of class Date")
  }
  if (length(from_date) > 1){
    stop("`from_date` can only be of length = 1")
  }
  if (length(with_census) > 1){
    stop("`with_census` can only be of length = 1")
  }
  if (!is.logical(with_census)){
    stop("`with_census` is not logical")
  }

  if (!is.numeric(rangex)){
    stop("`rangex` is not numeric")
  }
  if (length(rangex) != 2){
    stop("`start_newmoon` can only be of length = 2")
  }
  if(any(rangex < 1) | any(rangex %% 1 != 0)){
    stop("`rangex` is not a positive integer")
  }
  if (!is.numeric(start_newmoon)){
    stop("`start_newmoon` is not numeric")
  }
  if (length(start_newmoon) > 1){
    stop("`start_newmoon` can only be of length = 1")
  }
  if(start_newmoon < 1 | start_newmoon %% 1 != 0){
    stop("`start_newmoon` is not a positive integer")
  }
  if (!("logical" %in% class(add_obs))){
    stop("`add_obs` is not of class logical")
  }
  if (length(add_obs) > 1){
    stop("`add_obs` can only be of length = 1")
  }
  if (!is.null(moons) & 
      !("moons" %in% class(moons))){
    stop("`moons` is not NULL or of class moons")
  }

  if (!is.null(future_moons) & 
      !("moons" %in% class(future_moons))){
    stop("`future_moons` is not NULL or of class moons")
  }

  if (!is.null(rodents_list) & 
      !("rodents_list" %in% class(rodents_list))){
    stop("`rodents_list` is not of class rodents_list")
  }
  if (!is.null(rodents) & 
      !("rodents" %in% class(rodents))){
    stop("`rodents` is not of class rodents")
  }
  if (!is.null(covariates) & 
      !("covariates" %in% class(covariates))){
    stop("`covariates` is not NULL or of class covariates")
  }
  if (!is.null(hist_cov) & 
      !("covariates" %in% class(hist_cov))){
    stop("`hist_cov` is not NULL or of class covariates")
  }
  if (!is.null(new_forecast_covariates) & 
      !("covariates" %in% class(new_forecast_covariates))){
    stop("`new_forecast_covariates` is not NULL or of class covariates")
  }
  if (!is.null(name) & !is.character(name)){
    stop("`name` is not NULL nor a character")
  }
  if (length(name) > 1){
    stop("`name` can only be of length = 1")
  }
  if (length(lag) > 1){
    stop("`lag` can only be of length = 1")
  }
  if (!is.null(lag)){
    if (!("numeric" %in% class(lag)) & !("integer" %in% class(lag))){
      stop("`lag` is not of class numeric or integer")
    }
    if(lag < 0 | lag %% 1 != 0){
      stop("`lag` is not a non-negative integer")
    }
  }
  if (!("logical" %in% class(mod_covariates))){
    stop("`mod_covariates` is not of class logical")
  }
  if (is.null(subs_type) & is.null(subs_names)){
    stop("both `subs_type` and `subs_names` are NULL")
  }
  if (!("portalcasting" %in% subs_type) & is.null(subs_names)){
    stop("`subs_type` is not recognized and `subs` is NULL")
  }
  if (!is.null(subs_names) && !is.character(subs_names)){
    stop("`subs_names` is not a character")
  }
  if (!is.null(specific_sub)){
    if (is.null(tree)){
      stop("`tree` is NULL, so `specific_sub` cannot be checked")
    }
    if (!all(specific_sub %in% tree$subs)){
      stop("`specific_sub` not in `tree`")
    }
  }
  if (is.null(extension)){
    stop("`extension` needs to be specified")
  }
  if (!is.character(extension)){
    stop("`extension` is not a character")
  }
  if (length(extension) > 1){
    stop("`extension` can only be length 1")
  }
  if (is.null(local_path)){
    stop("`local_path` needs to be specified")
  }
  if (!is.character(local_path)){
    stop("`local_path` is not a character")
  }
  if (is.null(set) & is.null(add)){
    stop("both `set` and `add` are NULL")
  }
  if (!is.null(set) & !is.character(set)){
    stop("`set` is not NULL or a character")
  }
  if (length(set) > 1){
    stop("`set` can only be of length = 1")
  } 

  if (!is.null(set) && !(set %in% c("prefab"))){
    stop("`models` not defined for that `set`")
  }
  if (!is.null(add) & !is.character(add)){
      stop("`add` is not NULL or a character")
  }
  if (!("logical" %in% class(tail))){
    stop("`tail` is not of class logical")
  }
}
