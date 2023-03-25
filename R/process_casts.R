# this will update old forecast folders to the new species-level files and numbering

update_forecasts_folder <- function (main = ".", settings = directory_settings(), quiet = FALSE) {

  casts_metadata <- read_casts_metadata(main = main, settings = settings, quiet = quiet)

  old_colnames <- c("cast_id", "cast_group", "cast_date", "start_moon", "end_moon", "lead_time", "model", "dataset", "portalcasting_version", "QAQC", "notes")

  if (all(colnames(casts_metadata) == old_colnames)) {

    messageq(" --- Updating forecasts folder ---", quiet = quiet)

    out <- data.frame(cast_id                      = NULL,
                      old_cast_id                  = NULL,
                      cast_group                   = NULL,
                      cast_date                    = NULL,
                      origin                       = NULL,
                      historic_start_newmoonnumber = NULL,
                      historic_end_newmoonnumber   = NULL,
                      forecast_start_newmoonnumber = NULL,
                      forecast_end_newmoonnumber   = NULL,
                      lead_time_newmoons           = NULL,
                      model                        = NULL,
                      dataset                      = NULL,
                      species                      = NULL,
                      portalcasting_version        = NULL,
                      QAQC                         = NULL,
                      notes                        = NULL)


    ncasts   <- nrow(casts_metadata)
    for (i in 1:ncasts) {

      cast_tab  <- read_cast_tab(main = main, cast_id = casts_metadata$cast_id[i], settings = settings)
      cast_meta <- read_cast_metadata(main = main, cast_id = casts_metadata$cast_id[i], settings = settings)
      cast_cast <- read_model_cast(main = main, cast_id = casts_metadata$cast_id[i], settings = settings)

      nspecies <- length(unique(cast_tab$species))

      for (j in 1:nspecies) {

        cast_tab_j                  <- cast_tab[cast_tab$species == unique(cast_tab$species)[j], ]
        cast_id_j                   <- paste0(cast_tab_j$cast_id[1], "-", j)
        cast_tab_j$old_cast_id      <- cast_tab_j$cast_id
        cast_tab_j$cast_id          <- cast_id_j
        cast_tab_j$origin           <- NA
        cast_tab_j$newmoonnumber    <- cast_tab_j$moon
        cast_tab_j$moon             <- NULL

        cast_tab_j$historic_start_newmoonnumber <- cast_tab_j$start_moon
        cast_tab_j$historic_end_newmoonnumber   <- cast_tab_j$end_moon
        cast_tab_j$start_moon <- NULL
        cast_tab_j$end_moon   <- NULL

        cast_tab_j$forecast_start_newmoonnumber <- min(cast_tab_j$newmoonnumber)
        cast_tab_j$forecast_end_newmoonnumber   <- max(cast_tab_j$newmoonnumber)
        cast_tab_j$lead_time_newmoons           <- cast_tab_j$newmoonnumber - cast_tab_j$historic_end_newmoonnumber


        cast_tab_j$max_lag <- NA
        cast_tab_j$lag_buffer <- NA

        cast_meta_j            <- update_list(cast_meta, model = cast_meta$models, dataset = cast_meta$datasets, species = unique(cast_tab$species)[j], old_cast_id = casts_metadata$cast_id[i], cast_id = cast_id_j)

        if (class(cast_cast) == "list") {
          cast_cast_j            <- update_list(as.list(cast_cast[[j]]), model = cast_meta$models, dataset = cast_meta$datasets, species = unique(cast_tab$species)[j], old_cast_id = casts_metadata$cast_id[i], cast_id = cast_id_j)
        } else if (class(cast_cast) == "data.frame") {
          cast_cast_j            <- update_list(as.list(cast_cast[unique(cast_tab$species)[j], ]), model = cast_meta$models, dataset = cast_meta$datasets, species = unique(cast_tab$species)[j], old_cast_id = casts_metadata$cast_id[i], cast_id = cast_id_j)
        } 

        lpath <- paste0("cast_id_", cast_id_j, "_cast_tab.csv")
        cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
        write.csv(cast_tab_j, file = cpath, row.names = FALSE)

        lpath <- paste0("cast_id_", cast_id_j, "_metadata.yaml")
        cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
        write_yaml(cast_meta_j, file = cpath)

        lpath <- paste0("cast_id_", cast_id_j, "_model_cast.json")
        cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
        write_json(serializeJSON(cast_cast_j), path = cpath)

        out_row <- data.frame(cast_id                      = cast_id_j,
                              old_cast_id                  = casts_metadata$cast_id[i],
                              cast_group                   = casts_metadata$cast_group[i],
                              cast_date                    = casts_metadata$cast_date[i],
                              origin                       = NA,
                              historic_start_newmoonnumber = casts_metadata$start_moon[i],
                              historic_end_newmoonnumber   = casts_metadata$end_moon[i],
                              forecast_start_newmoonnumber = casts_metadata$end_moon[i] + 1,
                              forecast_end_newmoonnumber   = casts_metadata$end_moon[i] + casts_metadata$lead_time[i],
                              lead_time_newmoons           = casts_metadata$lead_time[i],
                              model                        = casts_metadata$model[i],
                              dataset                      = casts_metadata$dataset[i],
                              species                      = unique(cast_tab$species)[j],
                              portalcasting_version        = casts_metadata$portalcasting_version[i],
                              QAQC                         = casts_metadata$QAQC[i],
                              notes                        = casts_metadata$notes[i])

        out <- rbind(out, out_row)

      }
      lpath <- paste0("cast_id_", casts_metadata$cast_id[i], "_cast_tab.csv")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
      if (file.exists(cpath)) {

        file.remove(cpath)

      } 
      lpath <- paste0("cast_id_", casts_metadata$cast_id[i], "_metadata.yaml")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
      if (file.exists(cpath)) {

        file.remove(cpath)

      } 

      lpath_json  <- paste0("cast_id_", casts_metadata$cast_id[i], "_model_casts.json")
      cpath_json  <- file.path(main, settings$subdirectories$forecasts, lpath_json)

      lpath_RData <- paste0("cast_id_", casts_metadata$cast_id[i], "_model_casts.RData")
      cpath_RData  <- file.path(main, settings$subdirectories$forecasts, lpath_RData)

      if (file.exists(cpath_json)) {

        file.remove(cpath_json)

      } 
      if (file.exists(cpath_RData)) {

        file.remove(cpath_RData)
       
      }

    }

    meta_path <- file.path(main, settings$subdirectories$forecasts, settings$files$forecast_metadata)
    write.csv(x = out, file = meta_path, row.names = FALSE)

    messageq(" --- Done with update ---", quiet = quiet)

  } else {

    return( )

  }

}


#' @title Process and Save Cast Output to Files
#'
#' @description Take the model fit and cast output, process them into savable objects, and save them to the output folders. The cast metadata file is updated accordingly to track the saved output. \cr
#'
#' @param model_fit,model_cast Output from a model's fit and cast functions.
#'
#' @details Four model-specific output components are saved and returned. 
#'  \itemize{
#'   \item \code{"cast_metadata"}: saved out with \code{\link[yaml]{write_yaml}}.
#'   \item \code{"cast_tab"}: saved using \code{\link{write.csv}}.
#'   \item \code{"model_fit"}: saved out as a serialized \code{JSON} file via \code{\link[jsonlite]{serializeJSON}} and \code{\link[jsonlite:read_json]{write_json}}, so quite flexible with respect to specific object structure.
#'   \item \code{"model_cast"}: saved out as a serialized \code{JSON} file via \code{\link[jsonlite]{serializeJSON}} and \code{\link[jsonlite:read_json]{write_json}}, so quite flexible with respect to specific object structure.
#'  }
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not.
#'
#' @param model Length-one \code{character} vector of model name.
#'
#' @param dataset Length-one \code{character} vector of dataset name.
#'
#' @param species Length-one\code{character} vector of species name.
#'
#' @return Relevant elements are saved to external files, and returned as a \code{list}.
#'
#' @export
#'
process_model_output <- function (main      = ".", 
                                  model_fit = NULL,
                                  model_cast,
                                  model,
                                  dataset,
                                  species,
                                  settings = directory_settings( ), 
                                  quiet    = FALSE, 
                                  verbose  = FALSE) {


  casts_metadata <- read_casts_metadata(main     = main, 
                                        settings = settings,
                                        quiet    = quiet) 

  metadata <- read_metadata(main     = main,
                            settings = settings)

  ids     <- casts_metadata$cast_id
  ids     <- gsub("-", ".", ids)
  ids     <- as.numeric(ids)
  next_id <- ceiling(max(c(0, ids), na.rm = TRUE)) + 1


  cast_metadata <- update_list(metadata, 
                               cast_id          = next_id,
                               model            = model,
                               dataset          = dataset,
                               species          = species,
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


#' @title Measure Error and Fit Metrics for Forecasts
#' 
#' @description Summarize the cast-level errors or fits to the observations. \cr 
#'  Presently included are root mean square error and coverage.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See \code{\link{read_cast_tab}}.
#'
#' @return \code{data.frame} of metrics for each cast for each species. 
#'
#' @export
#'
measure_cast_level_error <- function (cast_tab = NULL) {


  return_if_null(cast_tab)

  ucast_ids <- unique(cast_tab$cast_id)
  ncast_ids <- length(ucast_ids)
  uspecies  <- unique(cast_tab$species)
  nspecies  <- length(uspecies)
  RMSE      <- rep(NA, ncast_ids * nspecies)
  coverage  <- rep(NA, ncast_ids * nspecies)
  model     <- rep(NA, ncast_ids * nspecies)
  counter   <- 1

  for (i in 1:ncast_ids) {

    for (j in 1:nspecies) {

      ij          <- cast_tab$cast_id == ucast_ids[i] & cast_tab$species == uspecies[j]
      cast_tab_ij <- cast_tab[ij, ]
      err         <- cast_tab_ij$err

      if (!is.null(err)) {

        RMSE[counter] <- sqrt(mean(err^2, na.rm = TRUE))

      }

      covered <- cast_tab_ij$covered

      if (!is.null(covered)) {

        coverage[counter] <- mean(covered, na.rm = TRUE)            

      }

      if (length(cast_tab_ij$model) > 0) {

        model[counter] <- unique(cast_tab_ij$model)

      }

      counter <- counter + 1

    }

  }

  ids <- rep(ucast_ids, each = nspecies)
  spp <- rep(uspecies, ncast_ids)

  data.frame(cast_id  = ids, 
             species  = spp, 
             model    = model, 
             RMSE     = RMSE, 
             coverage = coverage)
}

#' @title Add the Associated Values to a Cast Tab
#' 
#' @description Add values to a cast's cast tab. If necessary components are missing (such as no observations added yet and the user requests errors), the missing components are added. \cr \cr
#'  \code{add_obs_to_cast_tab} appends a column of observations. \cr \cr
#'  \code{add_err_to_cast_tab} adds a column of raw error values. \cr \cr
#'  \code{add_covered_to_cast_tab} appends a \code{logical} column indicating if the observation was within the prediction interval. 
#'
#' @details If a model interpolated a data set, \code{add_obs_to_cast_tab} adds the true (non-interpolated) observations so that model predictions are all compared to the same data.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See \code{\link{read_cast_tab}}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{data.frame} of \code{cast_tab} with an additional column or columns if needed. 
#'
#' @name add to cast tab
#'
#' @export
#'
add_err_to_cast_tab <- function (main     = ".", 
                                 settings = directory_settings( ), 
                                 cast_tab = NULL) {


  return_if_null(cast_tab)
  if (is.null(cast_tab$obs)) {

    cast_tab <- add_obs_to_cast_tab(main     = main,
                                    settings = settings,
                                    cast_tab = cast_tab)
  }

  cast_tab$error <- cast_tab$estimate - cast_tab$obs
  cast_tab

}

#' @rdname add-to-cast-tab
#'
#' @export
#'
add_covered_to_cast_tab <- function (main     = ".", 
                                     settings = directory_settings( ), 
                                     cast_tab = NULL) {


  return_if_null(cast_tab)
  if (is.null(cast_tab$obs)) {

    cast_tab <- add_obs_to_cast_tab(main     = main,
                                    settings = settings,
                                    cast_tab = cast_tab)

  }

  cast_tab$covered <- cast_tab$obs >= cast_tab$lower_pi & cast_tab$obs <= cast_tab$upper_pi 
  cast_tab$covered[is.na(cast_tab$obs)] <- NA
  cast_tab

}

#' @rdname add-to-cast-tab
#'
#' @export
#'
add_obs_to_cast_tab <- function (main     = ".", 
                                 settings = directory_settings( ),
                                 cast_tab = NULL) {

  return_if_null(cast_tab)

  cast_tab$obs   <- NA
  cast_dataset   <- gsub("dm_", "", gsub("_interp", "", cast_tab$dataset))
  ucast_dataset  <- unique(cast_dataset)
  ncast_datasets <- length(ucast_dataset)

  for (j in 1:ncast_datasets) {

    obs <- read_rodents_table(main           = main, 
                              settings       = settings,
                              dataset = ucast_dataset[j])


    matches <- which(cast_dataset == ucast_dataset[j])
    nmatches <- length(matches)
    obs_cols <- gsub("NA.", "NA", colnames(obs))

    for (i in 1:nmatches) {

      spot        <- matches[i]
      obs_moon    <- which(obs$newmoonnumber == cast_tab$newmoonnumber[spot])
      obs_species <- which(obs_cols == cast_tab$species[spot])

      if (length(obs_moon) == 1 & length(obs_species) == 1) {

        cast_tab$obs[spot] <- obs[obs_moon, obs_species]

      }

    }

  }

  cast_tab 

}



#' @title Read in Cast Output From a Given Cast
#'
#' @description Read in the various output files of a cast or casts in the casts sub directory. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param cast_ids,cast_id \code{integer} (or integer \code{numeric}) value(s) representing the cast(s) of interest, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information. If \code{NULL} (the default), the most recently generated cast's output is read in. \cr 
#'  \code{cast_ids} can be NULL, one value, or more than one values, \code{cast_id} can only be NULL or one value.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return 
#'  \code{read_cast_tab}: \code{data.frame} of the \code{cast_tab}. \cr \cr
#'  \code{read_cast_tabs}: \code{data.frame} of the \code{cast_tab}s with a \code{cast_id} column added to distinguish among casts. \cr \cr
#'  \code{read_cast_metadata}: \code{list} of \code{cast_metadata}. \cr \cr
#'  \code{read_model_fit}: a model fit \code{list}. \cr \cr
#'  \code{read_model_cast}: a model cast \code{list}.
#'
#' @name read cast output
#'
#' @export
#'
read_cast_tab <- function (main     = ".", 
                           cast_id  = NULL, 
                           settings = directory_settings( )) {

  if (is.null(cast_id) ){

    casts_meta <- select_casts(main     = main,
                               settings = settings)
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
                            cast_ids  = NULL, 
                            settings = directory_settings( )) {
  
  if (is.null(cast_ids)) {

    casts_meta <- select_casts(main     = main,
                               settings = settings)
    cast_ids   <- max(casts_meta$cast_id)

  }

  cast_tab <- read_cast_tab(main     = main,
                            cast_id  = cast_ids[1],
                            settings = settings)
  ncasts   <- length(cast_ids)


  if (ncasts > 1) {

    for (i in 2:ncasts) {

      cast_tab_i <- read_cast_tab(main     = main,
                                  cast_id  = cast_ids[i], 
                                  settings = settings)

      cast_tab   <- rbind(cast_tab, cast_tab_i)

    }

  }

  cast_tab

}

#' @rdname read-cast-output
#'
#' @export
#'
read_cast_metadata <- function (main     = ".", 
                                cast_id  = NULL, 
                                settings = directory_settings( )) {
  
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
read_model_fit <- function (main     = ".", 
                            cast_id  = NULL, 
                            settings = directory_settings( )) {
  
  if (is.null(cast_id)) {

    casts_meta <- select_casts(main     = main,
                               settings = settings)
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
read_model_cast <- function (main     = ".", 
                             cast_id  = NULL, 
                             settings = directory_settings( )) {
  
  if (is.null(cast_id)) {

    casts_meta <- select_casts(main     = main,
                               settings = settings)

    cast_id <- gsub("\\.", "-", max(as.numeric(gsub("-", ".", casts_meta$cast_id))))

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

eval_models <- function() {

  c("ESSS", "AutoArima", "nbsGARCH")

}

#' @title Find Casts that Fit Specifications
#'
#' @description Determines the casts that match user specifications. \cr
#'  Functionally, a wrapper on \code{\link{read_casts_metadata}} with filtering for specifications that provides a simple user interface to the large set of available casts via the metadata. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param historic_end_newmoonnumbers \code{integer} (or integer \code{numeric}) newmoon numbers of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast groups to include. Default value is \code{NULL}, which equates to no selection with respect to \code{cast_group}.
#'
#' @param models \code{character} values of the names of the models to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}.
#'
#' @param datasets \code{character} values of the rodent data sets to include. Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param species \code{character} value of the species codes (or \code{"total"} for the total across species) to include. Default value is \code{NULL}, which equates to no selection with respect to \code{species}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{data.frame} of the \code{cast_tab}.
#'
#' @export
#'
select_casts <- function (main                        = ".", 
                          settings                    = directory_settings( ), 
                          cast_ids                    = NULL,
                          cast_groups                 = NULL,
                          models                      = NULL, 
                          datasets                    = NULL,
                          species                     = NULL,
                          historic_end_newmoonnumbers = NULL, 
                          quiet                       = FALSE) {


  casts_metadata <- read_casts_metadata(main     = main,
                                        settings = settings,
                                        quiet    = quiet)

  ucast_ids      <- unique(casts_metadata$cast_id[casts_metadata$QAQC])
  cast_ids       <- ifnull(cast_ids, ucast_ids)
  match_id       <- casts_metadata$cast_id %in% cast_ids

  ucast_groups   <- unique(casts_metadata$cast_group[casts_metadata$QAQC])
  cast_groups    <- ifnull(cast_groups, ucast_groups)
  match_group    <- casts_metadata$cast_group %in% cast_groups

  uend_moons     <- c(unique(casts_metadata$end_moon[casts_metadata$QAQC]),
                      unique(casts_metadata$historic_end_newmoonnumber[casts_metadata$QAQC]))
  end_moons      <- ifnull(historic_end_newmoonnumbers, uend_moons)

  if ("end_moon" %in% colnames(casts_metadata)) {

    match_end_moon <- casts_metadata$end_moon %in% end_moons

  } else if ("historic_end_newmoonnumber" %in% colnames(casts_metadata)) {

    match_end_moon <- casts_metadata$historic_end_newmoonnumber %in% end_moons

  } 

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
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return Data requested.
#'
#' @export
#'
read_casts_metadata <- function (main     = ".",
                                 settings = directory_settings( ), 
                                 quiet    = FALSE){
  
  meta_path <- file.path(main, settings$subdirectories$forecasts, settings$files$forecast_metadata)

  if (!file.exists(meta_path)) {

    messageq("  **creating forecast metadata file**", quiet = quiet)

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

