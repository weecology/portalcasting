#' @title Process and Save Cast Output to Files
#'
#' @description Take the model fit and cast output, process them into savable objects, and save them to the output folders. The cast metadata file is updated accordingly to track the saved output. \cr
#'
#' @param model_fit,model_cast Output from a model's fit and cast functions.
#'
#' @details Four model-specific output components are saved and returned. 
#'    * `"cast_metadata"`: saved out with [`write_yaml`][yaml::write_yaml].
#'    * `"cast_tab"`: saved using [`write_csv_arrow`].
#'    * `"model_fit"`: saved out as a serialized `JSON` file via [`serializeJSON`][jsonlite::serializeJSON] and [`read_json`][jsonlite::read_json], so quite flexible with respect to specific object structure.
#'    * `"model_cast"`: saved out as a serialized `JSON` file via [`serializeJSON`][jsonlite::serializeJSON] and [`read_json`][jsonlite::read_json], so quite flexible with respect to specific object structure.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param model Length-one `character` vector of model name.
#'
#' @param dataset Length-one `character` vector of dataset name.
#'
#' @param species Length-one`character` vector of species name.
#'
#' @return Relevant elements are saved to external files, and returned as a `list`.
#'
#' @export
#'
process_model_output <- function (main      = ".", 
                                  model_fit = NULL,
                                  model_cast,
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

  cast_metadata <- update_list(metadata, 
                               forecast_id          = next_id,
                               model            = model,
                               dataset          = dataset,
                               species          = species,
                               model_controls   = model_controls[[model]],
                               dataset_controls = metadata$datasets_controls[[dataset]])

  cast_tab <- data.frame(lead_time_newmoons                 = 1:metadata$time$lead_time_newmoons,
                         max_lag                            = metadata$time$max_lag,
                         lag_buffer                         = metadata$time$lag_buffer,
                         origin                             = metadata$time$origin, 
                         cast_date                          = metadata$time$cast_date, 
                         cast_month                         = metadata$time$forecast_months,
                         cast_year                          = metadata$time$forecast_years, 
                         newmoonnumber                      = metadata$time$forecast_newmoonnumbers,
                         currency                           = metadata$datasets_controls[[dataset]]$args$output,
                         model                              = model, 
                         dataset                            = dataset, 
                         species                            = species, 
                         estimate                           = as.numeric(model_cast$mean), 
                         lower_pi                           = as.numeric(model_cast$lower[ , 1]),
                         upper_pi                           = as.numeric(model_cast$upper[ , 1]), 
                         historic_start_newmoonnumber       = metadata$time$historic_start_newmoonnumber,
                         historic_end_newmoonnumber         = metadata$time$historic_end_newmoonnumber,
                         forecast_start_newmoonnumber       = metadata$time$forecast_start_newmoonnumber,
                         forecast_end_newmoonnumber         = metadata$time$forecast_end_newmoonnumber,
                         confidence_level                   = metadata$confidence_level,
                         forecast_group                         = metadata$forecast_group,
                         old_forecast_id                        = NA,
                         forecast_id                            = cast_metadata$forecast_id)

  pkg_version   <- metadata$directory_configuration$setup$core_package_version

  new_cast_metadata <- data.frame(forecast_id                      = cast_metadata$forecast_id,
                                  old_forecast_id                  = NA,
                                  forecast_group                   = cast_metadata$forecast_group,
                                  cast_date                    = cast_metadata$time$cast_date,
                                  origin                       = cast_metadata$time$origin,
                                  historic_start_newmoonnumber = cast_metadata$time$historic_start_newmoonnumber,
                                  historic_end_newmoonnumber   = cast_metadata$time$historic_end_newmoonnumber,
                                  forecast_start_newmoonnumber = cast_metadata$time$forecast_start_newmoonnumber,
                                  forecast_end_newmoonnumber   = cast_metadata$time$forecast_end_newmoonnumber,
                                  lead_time_newmoons           = cast_metadata$time$lead_time_newmoons,
                                  model                        = model,
                                  dataset                      = dataset,
                                  species                      = species,
                                  portalcasting_version        = pkg_version,
                                  QAQC                         = TRUE,
                                  notes                        = NA)

  forecasts_metadata <- rbind(forecasts_metadata, new_cast_metadata)

  if (settings$save) {
 
  # update these to be write_data calls

    cast_metadata_filename <- paste0("forecast_id_", cast_metadata$forecast_id, "_metadata.yaml")
    cast_metadata_path     <- file.path(main, settings$subdirectories$forecasts, cast_metadata_filename)

    write_yaml(x    = cast_metadata,
               file = cast_metadata_path)


    cast_tab_filename <- paste0("forecast_id_", cast_metadata$forecast_id, "_cast_tab.csv") 
    cast_tab_path     <- file.path(main, settings$subdirectories$forecasts, cast_tab_filename)

    row.names(cast_tab) <- NULL
    write_csv_arrow(x         = cast_tab,
                    file      = cast_tab_path)

    row.names(forecasts_metadata) <- NULL
    write_csv_arrow(x         = forecasts_metadata, 
                    file      = forecasts_metadata_path(main = main))

    model_fit_filename <- paste0("forecast_id_", cast_metadata$forecast_id, "_model_fit.json") 
    model_fit_path     <- file.path(main, settings$subdirectories$fits, model_fit_filename)
    model_fit_json     <- serializeJSON(x = model_fit)

    write_json(x       = model_fit_json, 
               path    = model_fit_path)

    model_cast_filename <- paste0("forecast_id_", cast_metadata$forecast_id, "_model_cast.json") 
    model_cast_path     <- file.path(main, settings$subdirectories$forecasts, model_cast_filename)
    model_cast_json     <- serializeJSON(x = model_cast)

    write_json(x    = model_cast_json, 
               path = model_cast_path)

  }

  list(cast_metadata   = cast_metadata, 
       cast_tab        = cast_tab, 
       model_fit       = model_fit, 
       model_cast      = model_cast)

}




#' @title Read in Cast Output From a Given Cast
#'
#' @description Read in the various output files of a cast or casts in the casts sub directory. 
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param forecast_ids,forecast_id `integer` (or integer `numeric`) value(s) representing the cast(s) of interest, as indexed within the directory in the `casts` sub folder. See the casts metadata file (`forecasts_metadata.csv`) for summary information. If `NULL` (the default), the most recently generated cast's output is read in. \cr 
#'  `forecast_ids` can be NULL, one value, or more than one values, `forecast_id` can only be NULL or one value.
#'
#' @return 
#'  `read_cast_tab`: `data.frame` of the `cast_tab`. \cr \cr
#'  `read_cast_tabs`: `data.frame` of the `cast_tab`s with a `forecast_id` column added to distinguish among casts. \cr \cr
#'  `read_cast_metadata`: `list` of `cast_metadata`. \cr \cr
#'  `read_model_fit`: a model fit `list`. \cr \cr
#'  `read_model_cast`: a model cast `list`.
#'
#' @name read cast output
#'
#' @export
#'
read_cast_tab <- function (main    = ".", 
                           forecast_id = NULL) {

  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id) ){

    casts_meta <- select_forecasts(main = main)
    forecast_id    <- max(casts_meta$forecast_id)

  }

  lpath <- paste0("forecast_id_", forecast_id, "_cast_tab.csv")
  cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

  if (!file.exists(cpath)) {

    stop("forecast_id does not have a cast_table")

  }

  out <- as.data.frame(read_csv_arrow(file = cpath))
  out <- na_conformer(out)
  class(out$species) <- "character"
 
  out

}

#' @rdname read-cast-output
#'
#' @export
#'
read_cast_tabs <- function (main     = ".", 
                            forecast_ids = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_ids)) {

    casts_meta <- select_forecasts(main = main)
    forecast_ids   <- max(casts_meta$forecast_id)

  }

  cast_tab <- read_cast_tab(main    = main,
                            forecast_id = forecast_ids[1])
  ncasts   <- length(forecast_ids)


  if (ncasts > 1) {

    for (i in 2:ncasts) {

      cast_tab_i <- read_cast_tab(main    = main,
                                  forecast_id = forecast_ids[i])

      cast_tab   <- rbind(cast_tab, cast_tab_i)

    }

  }

  cast_tab

}

#' @rdname read-cast-output
#'
#' @export
#'
read_cast_metadata <- function (main    = ".", 
                                forecast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id)) {

    casts_meta <- select_forecasts(main = main)
    forecast_id    <- max(casts_meta$forecast_id)

  }

  lpath <- paste0("forecast_id_", forecast_id, "_metadata.yaml")
  cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

  if (!file.exists(cpath)) {

    stop("forecast_id does not have a cast_metadata file")

  }

  read_yaml(cpath, eval.expr = TRUE) 

}


#' @rdname read-cast-output
#'
#' @export
#'
read_model_fit <- function (main    = ".", 
                            forecast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id)) {

    casts_meta <- select_forecasts(main = main)
    forecast_id <- max(casts_meta$forecast_id)

  }

  cpath <- file.path(main, settings$subdirectories$forecasts, paste0("forecast_id_", forecast_id, "_model_fit.json"))

  if (file.exists(cpath)) {

    read_in_json <- fromJSON(readLines(cpath))
    unserializeJSON(read_in_json)

  } else {

    stop("forecast_id does not have a model_fit file")

  } 

}

#' @rdname read-cast-output
#'
#' @export
#'
read_model_cast <- function (main    = ".", 
                             forecast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(forecast_id)) {

    casts_meta <- select_forecasts(main = main)

    forecast_id <- max(as.numeric(gsub("-", ".", casts_meta$forecast_id)))

  }

  cpath_json  <- file.path(main, settings$subdirectories$forecasts, paste0("forecast_id_", forecast_id, "_model_cast.json"))
  cpath_RData <- file.path(main, settings$subdirectories$forecasts, paste0("forecast_id_", forecast_id, "_model_cast.RData"))

  if (file.exists(cpath_json)) {

    read_in_json <- fromJSON(readLines(cpath_json))
    unserializeJSON(read_in_json)

  } else if (file.exists(cpath_RData)) {

      model_casts <- NULL
      load(cpath_RData)
      model_casts

  } else {

     stop("forecast_id does not have a model_cast file")

  } 

}


#' @title Find Casts that Fit Specifications
#'
#' @description Determines the casts that match user specifications. \cr
#'  Functionally, a wrapper on [`read_forecasts_metadata`] with filtering for specifications that provides a simple user interface to the large set of available casts via the metadata. 
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param forecast_ids `integer` (or integer `numeric`) values representing the casts of interest, as indexed within the directory in the `casts` sub folder. See the casts metadata file (`forecasts_metadata.csv`) for summary information.
#'
#' @param historic_end_newmoonnumbers `integer` (or integer `numeric`) newmoon numbers of the forecast origin. Default value is `NULL`, which equates to no selection.
#'
#' @param forecast_groups `integer` (or integer `numeric`) value of the cast groups to include. Default value is `NULL`, which equates to no selection with respect to `forecast_group`.
#'
#' @param models `character` values of the names of the models to include. Default value is `NULL`, which equates to no selection with respect to `model`.
#'
#' @param datasets `character` values of the rodent data sets to include. Default value is `NULL`, which equates to no selection with respect to `dataset`.
#'
#' @param species `character` value of the species codes (or `"total"` for the total across species) to include. Default value is `NULL`, which equates to no selection with respect to `species`.
#'
#' @return `data.frame` of the `cast_tab`.
#'
#' @export
#'
select_forecasts <- function (main                        = ".", 
                          forecast_ids                    = NULL,
                          forecast_groups                 = NULL,
                          models                      = NULL, 
                          datasets                    = NULL,
                          species                     = NULL,
                          historic_end_newmoonnumbers = NULL) {

  settings <- read_directory_settings(main = main)

  forecasts_metadata <- read_forecasts_metadata(main = main)

  uforecast_ids      <- unique(forecasts_metadata$forecast_id[forecasts_metadata$QAQC])
  forecast_ids       <- ifnull(forecast_ids, uforecast_ids)
  match_id       <- forecasts_metadata$forecast_id %in% forecast_ids

  uforecast_groups   <- unique(forecasts_metadata$forecast_group[forecasts_metadata$QAQC])
  forecast_groups    <- ifnull(forecast_groups, uforecast_groups)
  match_group    <- forecasts_metadata$forecast_group %in% forecast_groups

  uend_moons     <- unique(forecasts_metadata$historic_end_newmoonnumber[forecasts_metadata$QAQC])
  end_moons      <- ifnull(historic_end_newmoonnumbers, uend_moons)

  match_end_moon <- forecasts_metadata$historic_end_newmoonnumber %in% end_moons

  umodels        <- unique(forecasts_metadata$model[forecasts_metadata$QAQC])
  models         <- ifnull(models, umodels)
  match_model    <- forecasts_metadata$model %in% models
  
  udatasets      <- unique(forecasts_metadata$dataset[forecasts_metadata$QAQC])
  datasets       <- ifnull(datasets, udatasets)
  match_dataset  <- forecasts_metadata$dataset %in% datasets

  if ("species" %in% colnames(forecasts_metadata)) {

    uspecies       <- unique(forecasts_metadata$species[forecasts_metadata$QAQC])
    species        <- ifnull(species, uspecies)
    match_species  <- forecasts_metadata$species %in% species

  } else {
  
    match_species <- rep(TRUE, length(match_id))

  }

  QAQC <- forecasts_metadata$QAQC

  forecasts_metadata[match_id & match_end_moon & match_model & match_dataset & match_species & QAQC, ]

}



  
#' @title Read in the Casts Metadata File
#'
#' @description Read in the casts metadata file. If the data file does not exist, an effort is made to create the file.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @return Data requested.
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
    write_csv_arrow(x         = out, 
                    file      = meta_path)

  }


  out <- as.data.frame(read_csv_arrow(file = meta_path))

  if ("species" %in% colnames(out)) {
    out <- na_conformer(out)
  }
  out[out$forecast_group != 0, ]

}

