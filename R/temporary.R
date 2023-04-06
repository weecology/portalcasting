# this will update old forecast folders to the new species-level files and numbering

update_forecasts_folder <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  casts_metadata <- read_casts_metadata(main = main)

  old_colnames <- c("cast_id", "cast_group", "cast_date", "start_moon", "end_moon", "lead_time", "model", "dataset", "portalcasting_version", "QAQC", "notes")

  if (all(colnames(casts_metadata) %in% old_colnames)) {

    messageq(" --- Updating forecasts folder ---", quiet = settings$quiet)

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

      lpath <- paste0("cast_id_", casts_metadata$cast_id[i], "_cast_tab.csv")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

      if (!file.exists(cpath)) {

        stop("cast_id does not have a cast_table")

      }

      cast_tab <- as.data.frame(read_csv_arrow(file = cpath))
      cast_tab <- na_conformer(out)

      lpath <- paste0("cast_id_", cast_id, "_metadata.yaml")
      cpath <- file.path(main, settings$subdirectories$forecasts, lpath)

      if (!file.exists(cpath)) {

        stop("cast_id does not have a cast_metadata file")

      }

      cast_meta <- read_yaml(cpath, eval.expr = TRUE) 



      cpath_json  <- file.path(main, settings$subdirectories$forecasts, paste0("cast_id_", cast_id, "_model_casts.json"))
      cpath_RData <- file.path(main, settings$subdirectories$forecasts, paste0("cast_id_", cast_id, "_model_casts.RData"))

      if (file.exists(cpath_json)) {

        read_in_json <- fromJSON(readLines(cpath_json))
        cast_cast <- unserializeJSON(read_in_json)

      } else if (file.exists(cpath_RData)) {

          model_casts <- NULL
          load(cpath_RData)
          cast_cast <- model_casts

      } else {

         stop("cast_id does not have a model_cast file")

      } 



      


      nspecies <- length(unique(cast_tab$species))

      js <- 1:(nspecies + 5)
      js <- js[substr(js, 2, 2) != "0"]
      # avoids the xyz.10 turning into xyz.1 when read in as a numeric

      for (j in 1:nspecies) {

        cast_tab_j                  <- cast_tab[cast_tab$species == unique(cast_tab$species)[j], ]
        cast_id_j                   <- paste0(cast_tab_j$cast_id[1], ".", ifelse(nchar(j) == 1, paste0(0, j), js[j]))
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
        row.names(cast_tab_j) <- NULL
        write_csv_arrow(x = cast_tab_j, file = cpath)

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
                              forecast_start_newmoonnumber = min(cast_tab$moon),
                              forecast_end_newmoonnumber   = max(cast_tab$moon),
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
    row_names(out) <- NULL    
    write_csv_arrow(x = out, file = meta_path)

    messageq(" --- Done with update ---", quiet = settings$quiet)

  } else {

    return( )

  }

}
