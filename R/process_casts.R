#' @title Process and Save Cast Output to Files
#'
#' @description Take the model fit and cast output, process them into savable objects, and save them to the output folders. The cast metadata file is updated accordingly to track the saved output. \cr
#'
#' @param model_fit,model_cast Output from a model's fit and cast functions.
#'
#' @details Four model-specific output components are saved and returned. 
#'    * `"cast_metadata"`: saved out with [`write_yaml`][yaml::write_yaml].
#'    * `"cast_tab"`: saved using [`write.csv`].
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

  casts_metadata <- read_casts_metadata(main = main) 

  metadata <- read_metadata(main = main)

  ids     <- casts_metadata$cast_id
  ids     <- as.numeric(ids)
  next_id <- ceiling(max(c(0, ids), na.rm = TRUE)) + 1

  model_controls <- read_model_controls(main = main)

  cast_metadata <- update_list(metadata, 
                               cast_id          = next_id,
                               model            = model,
                               dataset          = dataset,
                               species          = species,
                               model_controls   = model_controls[[model]],
                               dataset_controls = metadata$dataset_controls[[dataset]])

  cast_tab <- data.frame(lead_time_newmoons                 = 1:metadata$time$lead_time_newmoons,
                         max_lag                            = metadata$time$max_lag,
                         lag_buffer                         = metadata$time$lag_buffer,
                         origin                             = metadata$time$origin, 
                         cast_date                          = metadata$time$cast_date, 
                         cast_month                         = metadata$time$forecast_months,
                         cast_year                          = metadata$time$forecast_years, 
                         newmoonnumber                      = metadata$time$forecast_newmoonnumbers,
                         currency                           = metadata$dataset_controls[[dataset]]$args$output,
                         model                              = model, 
                         dataset                            = dataset, 
                         species                            = species, 
                         estimate                           = model_cast$mean, 
                         lower_pi                           = model_cast$lower[ , 1],
                         upper_pi                           = model_cast$upper[ , 1], 
                         historic_start_newmoonnumber       = metadata$time$historic_start_newmoonnumber,
                         historic_end_newmoonnumber         = metadata$time$historic_end_newmoonnumber,
                         forecast_start_newmoonnumber       = metadata$time$forecast_start_newmoonnumber,
                         forecast_end_newmoonnumber         = metadata$time$forecast_end_newmoonnumber,
                         confidence_level                   = metadata$confidence_level,
                         cast_group                         = metadata$cast_group,
                         old_cast_id                        = NA,
                         cast_id                            = cast_metadata$cast_id)

  pkg_version   <- metadata$directory_configuration$setup$core_package_version

  new_cast_metadata <- data.frame(cast_id                      = cast_metadata$cast_id,
                                  old_cast_id                  = NA,
                                  cast_group                   = cast_metadata$cast_group,
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

  casts_metadata <- rbind(casts_metadata, new_cast_metadata)

  if (settings$save) {
 
  # update these to be write_data calls

    cast_metadata_filename <- paste0("cast_id_", cast_metadata$cast_id, "_metadata.yaml")
    cast_metadata_path     <- file.path(main, settings$subdirectories$forecasts, cast_metadata_filename)

    write_yaml(x    = cast_metadata,
               file = cast_metadata_path)


    cast_tab_filename <- paste0("cast_id_", cast_metadata$cast_id, "_cast_tab.csv") 
    cast_tab_path     <- file.path(main, settings$subdirectories$forecasts, cast_tab_filename)

    write.csv(x         = cast_tab,
              file      = cast_tab_path, 
              row.names = FALSE)

    casts_metadata_path <- file.path(main, settings$subdirectories$forecasts, settings$files$forecast_metadata)

    write.csv(x         = casts_metadata, 
              file      = casts_metadata_path, 
              row.names = FALSE)

    model_fit_filename <- paste0("cast_id_", cast_metadata$cast_id, "_model_fit.json") 
    model_fit_path     <- file.path(main, settings$subdirectories$fits, model_fit_filename)
    model_fit_json     <- serializeJSON(x = model_fit)

    write_json(x       = model_fit_json, 
               path    = model_fit_path)

    model_cast_filename <- paste0("cast_id_", cast_metadata$cast_id, "_model_cast.json") 
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
#' @param cast_ids,cast_id `integer` (or integer `numeric`) value(s) representing the cast(s) of interest, as indexed within the directory in the `casts` sub folder. See the casts metadata file (`casts_metadata.csv`) for summary information. If `NULL` (the default), the most recently generated cast's output is read in. \cr 
#'  `cast_ids` can be NULL, one value, or more than one values, `cast_id` can only be NULL or one value.
#'
#' @return 
#'  `read_cast_tab`: `data.frame` of the `cast_tab`. \cr \cr
#'  `read_cast_tabs`: `data.frame` of the `cast_tab`s with a `cast_id` column added to distinguish among casts. \cr \cr
#'  `read_cast_metadata`: `list` of `cast_metadata`. \cr \cr
#'  `read_model_fit`: a model fit `list`. \cr \cr
#'  `read_model_cast`: a model cast `list`.
#'
#' @name read cast output
#'
#' @export
#'
read_cast_tab <- function (main    = ".", 
                           cast_id = NULL) {

  settings <- read_directory_settings(main = main)

  if (is.null(cast_id) ){

    casts_meta <- select_casts(main = main)
    cast_id    <- max(casts_meta$cast_id)

  }

  lpath <- paste0("cast_id_", cast_id, "_cast_tab.csv")
  cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

  if (!file.exists(cpath)) {

    stop("cast_id does not have a cast_table")

  }

  out <- read.csv(cpath) 

  na_conformer(out)

}

#' @rdname read-cast-output
#'
#' @export
#'
read_cast_tabs <- function (main     = ".", 
                            cast_ids = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(cast_ids)) {

    casts_meta <- select_casts(main = main)
    cast_ids   <- max(casts_meta$cast_id)

  }

  cast_tab <- read_cast_tab(main    = main,
                            cast_id = cast_ids[1])
  ncasts   <- length(cast_ids)


  if (ncasts > 1) {

    for (i in 2:ncasts) {

      cast_tab_i <- read_cast_tab(main    = main,
                                  cast_id = cast_ids[i])

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
                                cast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(cast_id)) {

    casts_meta <- select_casts(main = main)
    cast_id    <- max(casts_meta$cast_id)

  }

  lpath <- paste0("cast_id_", cast_id, "_metadata.yaml")
  cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

  if (!file.exists(cpath)) {

    stop("cast_id does not have a cast_metadata file")

  }

  read_yaml(cpath, eval.expr = TRUE) 

}


#' @rdname read-cast-output
#'
#' @export
#'
read_model_fit <- function (main    = ".", 
                            cast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(cast_id)) {

    casts_meta <- select_casts(main = main)
    cast_id <- max(casts_meta$cast_id)

  }

  lpath_json1  <- paste0("cast_id_", cast_id, "_model_fit.json")
  lpath_json2  <- paste0("cast_id_", cast_id, "_model_fits.json")
  cpath_json1  <- file.path(main, settings$subdirectories$forecasts, lpath_json1)
  cpath_json2  <- file.path(main, settings$subdirectories$forecasts, lpath_json2)

  if (file.exists(cpath_json1)) {

    read_in_json <- fromJSON(readLines(cpath_json1))
    unserializeJSON(read_in_json)

  } else if (file.exists(cpath_json2)) {

    read_in_json <- fromJSON(readLines(cpath_json2))
    unserializeJSON(read_in_json)

  } else {

    stop("cast_id does not have a model_fit or model_fits file")

  } 

}

#' @rdname read-cast-output
#'
#' @export
#'
read_model_cast <- function (main    = ".", 
                             cast_id = NULL) {
  
  settings <- read_directory_settings(main = main)

  if (is.null(cast_id)) {

    casts_meta <- select_casts(main = main)

    cast_id <- max(as.numeric(gsub("-", ".", casts_meta$cast_id)))

  }


  lpath_json1  <- paste0("cast_id_", cast_id, "_model_cast.json")
  lpath_json2  <- paste0("cast_id_", cast_id, "_model_casts.json")
  cpath_json1  <- file.path(main, settings$subdirectories$forecasts, lpath_json1)
  cpath_json2  <- file.path(main, settings$subdirectories$forecasts, lpath_json2)

  lpath_RData1 <- paste0("cast_id_", cast_id, "_model_cast.RData")
  cpath_RData1 <- file.path(main, settings$subdirectories$forecasts, lpath_RData1)
  lpath_RData2 <- paste0("cast_id_", cast_id, "_model_casts.RData")
  cpath_RData2 <- file.path(main, settings$subdirectories$forecasts, lpath_RData2)

  if (file.exists(cpath_json1)) {

    read_in_json <- fromJSON(readLines(cpath_json1))
    unserializeJSON(read_in_json)

  } else if (file.exists(cpath_json2)) {

    read_in_json <- fromJSON(readLines(cpath_json2))
    unserializeJSON(read_in_json)

  } else if (file.exists(cpath_RData1)) {

      model_casts <- NULL
      load(cpath_RData1)
      model_casts

  } else if (file.exists(cpath_RData2)) {

      model_casts <- NULL
      load(cpath_RData2)
      model_casts

  } else {

     stop("cast_id does not have a model_cast or model_casts file")

  } 



}


#' @title Find Casts that Fit Specifications
#'
#' @description Determines the casts that match user specifications. \cr
#'  Functionally, a wrapper on [`read_casts_metadata`] with filtering for specifications that provides a simple user interface to the large set of available casts via the metadata. 
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param cast_ids `integer` (or integer `numeric`) values representing the casts of interest, as indexed within the directory in the `casts` sub folder. See the casts metadata file (`casts_metadata.csv`) for summary information.
#'
#' @param historic_end_newmoonnumbers `integer` (or integer `numeric`) newmoon numbers of the forecast origin. Default value is `NULL`, which equates to no selection.
#'
#' @param cast_groups `integer` (or integer `numeric`) value of the cast groups to include. Default value is `NULL`, which equates to no selection with respect to `cast_group`.
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
select_casts <- function (main                        = ".", 
                          cast_ids                    = NULL,
                          cast_groups                 = NULL,
                          models                      = NULL, 
                          datasets                    = NULL,
                          species                     = NULL,
                          historic_end_newmoonnumbers = NULL) {

  settings <- read_directory_settings(main = main)

  casts_metadata <- read_casts_metadata(main = main)

  ucast_ids      <- unique(casts_metadata$cast_id[casts_metadata$QAQC])
  cast_ids       <- ifnull(cast_ids, ucast_ids)
  match_id       <- casts_metadata$cast_id %in% cast_ids

  ucast_groups   <- unique(casts_metadata$cast_group[casts_metadata$QAQC])
  cast_groups    <- ifnull(cast_groups, ucast_groups)
  match_group    <- casts_metadata$cast_group %in% cast_groups

  uend_moons     <- unique(casts_metadata$historic_end_newmoonnumber[casts_metadata$QAQC])
  end_moons      <- ifnull(historic_end_newmoonnumbers, uend_moons)

  match_end_moon <- casts_metadata$historic_end_newmoonnumber %in% end_moons

  umodels        <- unique(casts_metadata$model[casts_metadata$QAQC])
  models         <- ifnull(models, umodels)
  match_model    <- casts_metadata$model %in% models
  
  udatasets      <- unique(casts_metadata$dataset[casts_metadata$QAQC])
  datasets       <- ifnull(datasets, udatasets)
  match_dataset  <- casts_metadata$dataset %in% datasets

  if ("species" %in% colnames(casts_metadata)) {

    uspecies       <- unique(casts_metadata$species[casts_metadata$QAQC])
    species        <- ifnull(species, uspecies)
    match_species  <- casts_metadata$species %in% species

  } else {
  
    match_species <- rep(TRUE, length(match_id))

  }

  QAQC <- casts_metadata$QAQC

  casts_metadata[match_id & match_end_moon & match_model & match_dataset & match_species & QAQC, ]

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
read_casts_metadata <- function (main = "."){
  
  settings  <- read_directory_settings(main = main)

  meta_path <- file.path(main, settings$subdirectories$forecasts, settings$files$forecast_metadata)

  if (!file.exists(meta_path)) {

    messageq("  **creating forecast metadata file**", quiet = settings$quiet)

    out <- data.frame(cast_id                      = NA,
                      old_cast_id                  = NA,
                      cast_group                   = 0,
                      cast_date                    = NA,
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
  
    write.csv(out, meta_path, row.names = FALSE)

  }


  out <- read.csv(meta_path)

  if ("species" %in% colnames(out)) {
    out <- na_conformer(out)
  }
  out[out$cast_group != 0, ]

}

