#' @title Process and Save Forecast Output to Files
#'
#' @description Take the model fit and forecast output, process them into savable objects, and save them to the output folders. \cr
#'              The forecast metadata file is updated accordingly to track the saved output. \cr
#'              `add_observations_to_forecast_table` appends a column of observations to a forecast's forecast tab. If a model interpolated a data set, it adds the true (non-interpolated) observations so that model predictions are all compared to the same data. \cr 
#'              `select_forecasts` determines the forecasts that match user specifications. Functionally, it is a wrapper on [`read_forecasts_metadata`] with filtering for specifications that provides a simple user interface to the large set of available forecasts via the metadata. 
#'
#' @param model_fit,model_forecast Output from a model's fit and forecast functions.
#'
#' @param forecast_id,forecast_ids `integer` (or integer `numeric`) value(s) representing the forecast(s) of interest, as indexed within the directory in the `casts` sub folder. See the forecasts metadata file (`forecasts_metadata.csv`) for summary information. If `NULL` (the default), the most recently generated forecast's output is read in. \cr 
#'        `forecast_ids` can be NULL, one value, or more than one values, `forecast_id` can only be NULL or one value.
#'
#' @param model,models `character` values of the name(s) of the model(s) of interest, as indexed within the directory in the `forecasts` sub folder. See the forecasts metadata file (`forecasts_metadata.csv`) for summary information. If `NULL` (the default), the most recently generated forecast's output is read in. \cr 
#'        `models` can be NULL, one value, or more than one values, `model` can only be NULL or one value.
#'
#' @param dataset,datasets `character` vector of the rodent dataset name(s) to include.
#'        `datasets` can be NULL, one value, or more than one values, `dataset` can only be NULL or one value.
#'
#' @param species `character` value of the species codes (or `"total"` for the total across species) to include. Default value is `NULL`, which equates to no selection with respect to `species`.
#'
#' @param historic_end_newmoonnumbers `integer` (or integer `numeric`) newmoon numbers of the forecast origin. Default value is `NULL`, which equates to no selection.
#'
#' @param forecast_groups `integer` (or integer `numeric`) value of the forecast groups to include. Default value is `NULL`, which equates to no selection with respect to `forecast_group`.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @details Four model-specific output components are saved and returned:
#'          * `forecast_metadata`: saved out with [`write_yaml`][yaml::write_yaml].
#'          * `forecast_tab`: saved using [`write_csv_arrow`].
#'          * `model_fit`: saved out as a serialized `JSON` file via [`serializeJSON`][jsonlite::serializeJSON] and [`read_json`][jsonlite::read_json], so quite flexible with respect to specific object structure.
#'          * `model_forecast`: saved out as a serialized `JSON` file via [`serializeJSON`][jsonlite::serializeJSON] and [`read_json`][jsonlite::read_json], so quite flexible with respect to specific object structure.
#'
#' @return `process_model_output`: relevant elements are saved to external files, and returned as a `list`. \cr
#'         `read_forecast_table`: forecast table `data.frame`. \cr
#'         `read_forecast_tables`: `data.frame` of combined forecast tables. \cr
#'         `add_observations_to_forecast_table`: forecast table `data.frame` with an observation column added. \cr
#'         `read_forecast_metadata`: `list` of `forecast_metadata`. \cr
#'         `read_model_fit`: forecast output (typically as a `list`). \cr
#'         `read_model_forecast`: forecast output (typically as a `list`). \cr
#'         `select_forecasts`:  `data.frame` of selected forecasts' metadata. \cr
#'         `read_forecasts_metadata`: `data.frame` of forecasts' metadata. 
#'
#' @name process forecast output
#'
NULL

#' @rdname process-forecast-output
#'
#' @export
#'
process_model_output <- function (main      = ".", 
                                  model_fit = NULL,
                                  model_forecast,
                                  model,
                                  dataset,
                                  species) {

  settings <- read_directory_settings(main = main)

  forecasts_metadata <- read_forecasts_metadata(main = main) 

  metadata <- read_metadata(main = main)

  ids     <- forecasts_metadata$forecast_id
  ids     <- as.numeric(ids)
  next_id <- ceiling(max(c(0, ids), na.rm = TRUE)) + 1

  model_controls <- read_models_controls(main = main)[[model]]

  forecast_metadata <- update_list(metadata, 
                                   forecast_id      = next_id,
                                   model            = model,
                                   dataset          = dataset,
                                   species          = species,
                                   model_controls   = model_controls[[model]],
                                   dataset_controls = metadata$datasets_controls[[dataset]])

  forecast_table <- data.frame(lead_time_newmoons                 = 1:metadata$time$lead_time_newmoons,
                               max_lag                            = metadata$time$max_lag,
                               lag_buffer                         = metadata$time$lag_buffer,
                               origin                             = metadata$time$origin, 
                               forecast_date                      = metadata$time$forecast_date, 
                               forecast_month                     = metadata$time$forecast_months,
                               forecast_year                      = metadata$time$forecast_years, 
                               newmoonnumber                      = metadata$time$forecast_newmoonnumbers,
                               currency                           = metadata$datasets_controls[[dataset]]$args$output,
                               model                              = model, 
                               dataset                            = dataset, 
                               species                            = species, 
                               estimate                           = as.numeric(model_forecast$mean), 
                               lower_pi                           = as.numeric(model_forecast$lower[ , 1]),
                               upper_pi                           = as.numeric(model_forecast$upper[ , 1]), 
                               historic_start_newmoonnumber       = metadata$time$historic_start_newmoonnumber,
                               historic_end_newmoonnumber         = metadata$time$historic_end_newmoonnumber,
                               forecast_start_newmoonnumber       = metadata$time$forecast_start_newmoonnumber,
                               forecast_end_newmoonnumber         = metadata$time$forecast_end_newmoonnumber,
                               confidence_level                   = metadata$confidence_level,
                               forecast_group                     = metadata$forecast_group,
                               old_forecast_id                    = NA,
                               forecast_id                        = forecast_metadata$forecast_id)

  pkg_version   <- metadata$directory_configuration$setup$core_package_version

  new_forecast_metadata <- data.frame(forecast_id                  = forecast_metadata$forecast_id,
                                      old_cast_id                  = NA,
                                      forecast_group               = forecast_metadata$forecast_group,
                                      forecast_date                = forecast_metadata$time$forecast_date,
                                      origin                       = forecast_metadata$time$origin,
                                      historic_start_newmoonnumber = forecast_metadata$time$historic_start_newmoonnumber,
                                      historic_end_newmoonnumber   = forecast_metadata$time$historic_end_newmoonnumber,
                                      forecast_start_newmoonnumber = forecast_metadata$time$forecast_start_newmoonnumber,
                                      forecast_end_newmoonnumber   = forecast_metadata$time$forecast_end_newmoonnumber,
                                      lead_time_newmoons           = forecast_metadata$time$lead_time_newmoons,
                                      model                        = model,
                                      dataset                      = dataset,
                                      species                      = species,
                                      portalcasting_version        = pkg_version,
                                      QAQC                         = TRUE,
                                      notes                        = NA)

  forecasts_metadata <- rbind(forecasts_metadata, new_forecast_metadata)

  if (settings$save) {
 
  # update these to be write_data calls

    forecast_metadata_filename <- paste0("forecast_id_", forecast_metadata$forecast_id, "_metadata.yaml")
    forecast_metadata_path     <- file.path(main, settings$subdirectories$forecasts, forecast_metadata_filename)

    write_yaml(x    = forecast_metadata,
               file = forecast_metadata_path)


    forecast_tab_filename <- paste0("forecast_id_", forecast_metadata$forecast_id, "_forecast_tab.csv") 
    forecast_tab_path     <- file.path(main, settings$subdirectories$forecasts, forecast_tab_filename)

    row.names(forecast_table) <- NULL
    write_csv_arrow(x         = forecast_tab,
                    file      = forecast_tab_path)

    row.names(forecasts_metadata) <- NULL
    write_csv_arrow(x         = forecasts_metadata, 
                    file      = forecasts_metadata_path(main = main))

    model_fit_filename <- paste0("forecast_id_", forecast_metadata$forecast_id, "_model_fit.json") 
    model_fit_path     <- file.path(main, settings$subdirectories$fits, model_fit_filename)
    model_fit_json     <- serializeJSON(x = model_fit)

    write_json(x       = model_fit_json, 
               path    = model_fit_path)

    model_forecast_filename <- paste0("forecast_id_", forecast_metadata$forecast_id, "_model_forecast.json") 
    model_forecast_path     <- file.path(main, settings$subdirectories$forecasts, model_forecast_filename)
    model_forecast_json     <- serializeJSON(x = model_forecast)

    write_json(x    = model_forecast_json, 
               path = model_forecast_path)

  }

  list(forecast_metadata   = forecast_metadata, 
       forecast_table      = forecast_tab, 
       model_fit           = model_fit, 
       model_forecast      = model_forecast)

}

#' @rdname process-forecast-output
#'
#' @export
#'
read_forecast_table <- function (main        = ".", 
                                 forecast_id = NULL) {

  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id) ){

    forecasts_meta <- select_forecasts(main = main)
    forecast_id    <- max(forecasts_meta$forecast_id)

  }

  lpath <- paste0("forecast_id_", forecast_id, "_forecast_tab.csv")
  cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

  if (!file.exists(cpath)) {

    stop("forecast_id does not have a forecast_table")

  }

  out <- as.data.frame(read_csv_arrow(file = cpath))
  out <- na_conformer(out)
  class(out$species) <- "character"
 
  out

}

#' @rdname process-forecast-output
#'
#' @export
#'
read_forecast_tables <- function (main         = ".", 
                                  forecast_ids = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_ids)) {

    forecasts_meta <- select_forecasts(main = main)
    forecast_ids   <- max(forecasts_meta$forecast_id)

  }

  forecast_table <- read_forecast_table(main        = main,
                                        forecast_id = forecast_ids[1])
  ncasts         <- length(forecast_ids)


  if (ncasts > 1) {

    for (i in 2:ncasts) {

      forecast_tab_i <- read_forecast_table(main        = main,
                                            forecast_id = forecast_ids[i])

      forecast_table <- rbind(forecast_tab, forecast_tab_i)

    }

  }

  forecast_tab

}


#' @rdname process-forecast-output
#'
#' @export
#'
add_observations_to_forecast_table <- function (main           = ".", 
                                                forecast_table = NULL) {

  return_if_null(forecast_table)

  dataset <- gsub("dm_", "", forecast_table$dataset[1])
  species <- forecast_table$species[1]

  forecast_table$observation   <- NA

  obs <- read_rodents_dataset(main     = main, 
                              dataset  = dataset)

  forecast_table$observaiton <- obs[match(forecast_table$newmoonnumber, obs$newmoonnumber), species]

  forecast_table 

}

#' @rdname process-forecast-output
#'
#' @export
#'
read_forecast_metadata <- function (main        = ".", 
                                    forecast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id)) {

    forecasts_meta <- select_forecasts(main = main)
    forecast_id    <- max(forecasts_meta$forecast_id)

  }

  lpath <- paste0("forecast_id_", forecast_id, "_metadata.yaml")
  cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

  if (!file.exists(cpath)) {

    stop("forecast_id does not have a forecast_metadata file")

  }

  read_yaml(cpath, eval.expr = TRUE) 

}


#' @rdname process-forecast-output
#'
#' @export
#'
read_model_fit <- function (main    = ".", 
                            forecast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id)) {

    forecasts_meta <- select_forecasts(main = main)
    forecast_id <- max(forecasts_meta$forecast_id)

  }

  cpath <- file.path(main, settings$subdirectories$forecasts, paste0("forecast_id_", forecast_id, "_model_fit.json"))

  if (file.exists(cpath)) {

    read_in_json <- fromJSON(readLines(cpath))
    unserializeJSON(read_in_json)

  } else {

    stop("forecast_id does not have a model_fit file")

  } 

}

#' @rdname process-forecast-output
#'
#' @export
#'
read_model_forecast <- function (main        = ".", 
                                 forecast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id)) {

    forecasts_meta <- select_forecasts(main = main)

    forecast_id    <- max(as.numeric(gsub("-", ".", forecasts_meta$forecast_id)))

  }

  cpath_json  <- file.path(main, settings$subdirectories$forecasts, paste0("forecast_id_", forecast_id, "_model_forecast.json"))
  cpath_RData <- file.path(main, settings$subdirectories$forecasts, paste0("forecast_id_", forecast_id, "_model_forecast.RData"))

  if (file.exists(cpath_json)) {

    read_in_json <- fromJSON(readLines(cpath_json))
    unserializeJSON(read_in_json)

  } else if (file.exists(cpath_RData)) {

      model_forecasts <- NULL
      load(cpath_RData)
      model_forecasts

  } else {

     stop("forecast_id does not have a model_forecast file")

  } 

}


#' @rdname process-forecast-output
#'
#' @export
#'
select_forecasts <- function (main                        = ".", 
                              forecast_ids                = NULL,
                              forecast_groups             = NULL,
                              models                      = NULL, 
                              datasets                    = NULL,
                              species                     = NULL,
                              historic_end_newmoonnumbers = NULL) {

  settings           <- read_directory_settings(main = main)

  forecasts_metadata <- read_forecasts_metadata(main = main)

  uforecast_ids      <- unique(forecasts_metadata$forecast_id[forecasts_metadata$QAQC])
  forecast_ids       <- ifnull(forecast_ids, uforecast_ids)
  match_id           <- forecasts_metadata$forecast_id %in% forecast_ids

  uforecast_groups   <- unique(forecasts_metadata$forecast_group[forecasts_metadata$QAQC])
  forecast_groups    <- ifnull(forecast_groups, uforecast_groups)
  match_group        <- forecasts_metadata$forecast_group %in% forecast_groups

  uend_moons         <- unique(forecasts_metadata$historic_end_newmoonnumber[forecasts_metadata$QAQC])
  end_moons          <- ifnull(historic_end_newmoonnumbers, uend_moons)

  match_end_moon     <- forecasts_metadata$historic_end_newmoonnumber %in% end_moons

  umodels            <- unique(forecasts_metadata$model[forecasts_metadata$QAQC])
  models             <- ifnull(models, umodels)
  match_model        <- forecasts_metadata$model %in% models
  
  udatasets          <- unique(forecasts_metadata$dataset[forecasts_metadata$QAQC])
  datasets           <- ifnull(datasets, udatasets)
  match_dataset      <- forecasts_metadata$dataset %in% datasets

  if ("species" %in% colnames(forecasts_metadata)) {

    uspecies         <- unique(forecasts_metadata$species[forecasts_metadata$QAQC])
    species          <- ifnull(species, uspecies)
    match_species    <- forecasts_metadata$species %in% species

  } else {
  
    match_species <- rep(TRUE, length(match_id))

  }

  forecasts_metadata[match_id & match_end_moon & match_model & match_dataset & match_species & forecasts_metadata$QAQC, ]

}



#' @rdname process-forecast-output
#'
#' @export
#'
read_forecasts_metadata <- function (main = ".") {
  
  settings  <- read_directory_settings(main = main)

  meta_path <- forecasts_metadata_path(main = main)

  if (!file.exists(meta_path)) {

    messageq("  **creating forecast metadata file**", quiet = settings$quiet)

    out <- data.frame(forecast_id                  = NA,
                      old_forecast_id              = NA,
                      forecast_group               = 0,
                      forecast_date                = NA,
                      origin                       = NA,
                      historic_start_newmoonnumber = NA,
                      historic_end_newmoonnumber   = NA,
                      forecast_start_newmoonnumber = NA,
                      forecast_end_newmoonnumber   = NA,
                      lead_time_newmoons           = NA,
                      model                        = NA,
                      dataset                      = NA,
                      species                      = NA,
                      portalcasting_version        = NA,
                      QAQC                         = NA,
                      notes                        = NA)
  
    row.names(out) <- NULL
    write_csv_arrow(x    = out, 
                    file = meta_path)

  }


  out <- as.data.frame(read_csv_arrow(file = meta_path))

  if ("species" %in% colnames(out)) {
    out <- na_conformer(out)
  }
  out[out$forecast_group != 0, ]

}

