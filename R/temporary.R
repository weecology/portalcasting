# this will update old forecast folders to the new species-level files and numbering

update_forecasts_folder <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  forecasts_metadata <- read_forecasts_metadata(main = main)

  if (file.exists(file.path(main, "forecasts", "casts_metadata.csv"))) {
    forecasts_metadata <- read.csv(file.path(main, "forecasts", "casts_metadata.csv"))
  }

  old_colnames <- c("cast_id", "cast_group", "cast_date", "start_moon", "end_moon", "lead_time", "model", "dataset", "portalcasting_version", "QAQC", "notes")

  if (all(colnames(forecasts_metadata) %in% old_colnames)) {

    messageq(" --- Updating forecasts folder ---", quiet = settings$quiet)

    out <- data.frame(forecast_id                  = NULL,
                      old_cast_id                  = NULL,
                      forecast_group               = NULL,
                      forecast_date                = NULL,
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


    ncasts   <- nrow(forecasts_metadata)
    for (i in 1:ncasts) {

      lpath <- paste0("cast_id_", forecasts_metadata$cast_id[i], "_cast_tab.csv")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

      if (!file.exists(cpath)) {

        stop("forecast_id does not have a forecast_table")

      }

      forecast_table <- as.data.frame(read_csv_arrow(file = cpath))
      forecast_table <- na_conformer(forecast_table)

      lpath <- paste0("cast_id_", forecasts_metadata$cast_id[i], "_metadata.yaml")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

      if (!file.exists(cpath)) {

        stop("forecast_id does not have a forecast_metadata file")

      }

      forecast_meta <- read_yaml(cpath, eval.expr = TRUE) 

      cpath_json  <- file.path(main, settings$subdirectories$forecasts, paste0("cast_id_", forecasts_metadata$cast_id[i], "_model_casts.json"))
      cpath_RData <- file.path(main, settings$subdirectories$forecasts, paste0("cast_id_", forecasts_metadata$cast_id[i], "_model_casts.RData"))

      if (file.exists(cpath_json)) {

        read_in_json <- fromJSON(readLines(cpath_json))
        forecast_forecast <- unserializeJSON(read_in_json)

      } else if (file.exists(cpath_RData)) {

          model_casts <- NULL
          load(cpath_RData)
          forecast_forecasts <- model_casts

      } else {

         stop("forecast_id does not have a model_forecast file")

      } 

      nspecies <- length(unique(forecast_table$species))

      js <- 1:(nspecies + 5)
      js <- js[substr(js, 2, 2) != "0"]
      # avoids the xyz.10 turning into xyz.1 when read in as a numeric

      for (j in 1:nspecies) {

        forecast_tab_j                              <- forecast_table[forecast_table$species == unique(forecast_table$species)[j], ]
        forecast_id_j                               <- paste0(forecast_tab_j$cast_id[1], ".", ifelse(nchar(j) == 1, paste0(0, j), js[j]))
        forecast_tab_j$old_cast_id                  <- forecast_tab_j$cast_id
        forecast_tab_j$forecast_id                  <- forecast_id_j
        forecast_tab_j$cast_id                      <- NULL

        forecast_tab_j$origin                       <- NA
        forecast_tab_j$newmoonnumber                <- forecast_tab_j$moon
        forecast_tab_j$moon                         <- NULL

        forecast_tab_j$forecast_date                <- forecast_tab_j$cast_date
        forecast_tab_j$cast_date                    <- NULL

        forecast_tab_j$forecast_month               <- forecast_tab_j$cast_month
        forecast_tab_j$cast_month                   <- NULL

        forecast_tab_j$forecast_year                <- forecast_tab_j$cast_year
        forecast_tab_j$cast_year                    <- NULL

        forecast_tab_j$forecast_group               <- forecast_tab_j$cast_group
        forecast_tab_j$cast_group                   <- NULL



        forecast_tab_j$historic_start_newmoonnumber <- forecast_tab_j$start_moon
        forecast_tab_j$historic_end_newmoonnumber   <- forecast_tab_j$end_moon
        forecast_tab_j$start_moon                   <- NULL
        forecast_tab_j$end_moon                     <- NULL

        forecast_tab_j$forecast_start_newmoonnumber <- min(forecast_tab_j$newmoonnumber)
        forecast_tab_j$forecast_end_newmoonnumber   <- max(forecast_tab_j$newmoonnumber)
        forecast_tab_j$lead_time_newmoons           <- forecast_tab_j$newmoonnumber - forecast_tab_j$historic_end_newmoonnumber


        forecast_tab_j$max_lag     <- NA
        forecast_tab_j$lag_buffer  <- NA

        forecast_meta_j            <- update_list(forecast_meta, model = forecast_meta$models, dataset = forecast_meta$datasets, species = unique(forecast_table$species)[j], old_forecast_id = forecasts_metadata$forecast_id[i], forecast_id = forecast_id_j)

        if (class(forecast_forecasts) == "list") {
          forecast_forecasts_j      <- update_list(as.list(forecast_forecasts[[j]]), model = forecast_meta$models, dataset = forecast_meta$datasets, species = unique(forecast_table$species)[j], old_forecast_id = forecasts_metadata$forecast_id[i], forecast_id = forecast_id_j)
        } else if (class(forecast_forecasts) == "data.frame") {
          forecast_forecasts_j      <- update_list(as.list(forecast_forecasts[unique(forecast_table$species)[j], ]), model = forecast_meta$models, dataset = forecast_meta$datasets, species = unique(forecast_table$species)[j], old_forecast_id = forecasts_metadata$forecast_id[i], forecast_id = forecast_id_j)
        } 

        lpath <- paste0("forecast_id_", forecast_id_j, "_forecast_table.csv")
        cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
        row.names(forecast_tab_j) <- NULL
        write_csv_arrow(x = forecast_tab_j, file = cpath)

        lpath <- paste0("forecast_id_", forecast_id_j, "_metadata.yaml")
        cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
        write_yaml(forecast_meta_j, file = cpath)

        lpath <- paste0("forecast_id_", forecast_id_j, "_model_forecast.json")
        cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
        write_json(serializeJSON(forecast_forecasts_j), path = cpath)

        out_row <- data.frame(forecast_id                  = forecast_id_j,
                              old_cast_id                  = forecasts_metadata$cast_id[i],
                              forecast_group               = forecasts_metadata$cast_group[i],
                              forecast_date                = forecasts_metadata$cast_date[i],
                              origin                       = NA,
                              historic_start_newmoonnumber = forecasts_metadata$start_moon[i],
                              historic_end_newmoonnumber   = forecasts_metadata$end_moon[i],
                              forecast_start_newmoonnumber = min(forecast_table$moon),
                              forecast_end_newmoonnumber   = max(forecast_table$moon),
                              lead_time_newmoons           = forecasts_metadata$lead_time[i],
                              model                        = forecasts_metadata$model[i],
                              dataset                      = forecasts_metadata$dataset[i],
                              species                      = unique(forecast_table$species)[j],
                              portalcasting_version        = forecasts_metadata$portalcasting_version[i],
                              QAQC                         = forecasts_metadata$QAQC[i],
                              notes                        = forecasts_metadata$notes[i])

        out <- rbind(out, out_row)

      }
      lpath <- paste0("cast_id_", forecasts_metadata$cast_id[i], "_cast_tab.csv")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
      if (file.exists(cpath)) {

        file.remove(cpath)

      } 
      lpath <- paste0("cast_id_", forecasts_metadata$cast_id[i], "_metadata.yaml")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)
      if (file.exists(cpath)) {

        file.remove(cpath)

      } 

      lpath_json  <- paste0("cast_id_", forecasts_metadata$cast_id[i], "_model_casts.json")
      cpath_json  <- file.path(main, settings$subdirectories$forecasts, lpath_json)

      lpath_RData <- paste0("cast_id_", forecasts_metadata$cast_id[i], "_model_casts.RData")
      cpath_RData  <- file.path(main, settings$subdirectories$forecasts, lpath_RData)

      if (file.exists(cpath_json)) {

        file.remove(cpath_json)

      } 
      if (file.exists(cpath_RData)) {

        file.remove(cpath_RData)
       
      }

    }

    meta_path <- file.path(main, settings$subdirectories$forecasts, settings$files$forecasts_metadata)
    row.names(out) <- NULL    
    write_csv_arrow(x = out, file = meta_path)

    messageq(" --- Done with update ---", quiet = settings$quiet)

  } else {

    return( )

  }

}
