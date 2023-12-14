#' Zip and unzip forecasts by forecast date
#'
#' @param type either zip or unzip
#' @param forecast_path location of forecast directory
#' @param date forecast date to use while zipping
#'
#' @export
#'
zip_unzip <- function(type = NULL,
                      forecast_path = "forecasts/",
                      date = NULL) {
  # Function to zip a list of files
  zip_files <- function(zipfile, files) {
    zipfile <- normalizePath(zipfile, mustWork = FALSE)
    if (file.exists(zipfile)) {
      # First remove old zip file if exists
      unlink(zipfile)
    }
    # Zip all files in the list
    utils::zip(zipfile, files)
    unlink(files)
  }
  
  messageq("Preparing forecasts files")
  proj_path <- forecast_path
  forecasts_metadata <- paste0(proj_path, "/forecasts_metadata.csv")
  
  proj_path <- normalizePath(proj_path, mustWork = FALSE)
  forecasts_metadata <-
    normalizePath(forecasts_metadata, mustWork = FALSE)
  metadata <- read.csv(forecasts_metadata)
  unique_dates <- sort(unique(metadata$"forecast_date"))
  
  if (type == "zip") {
    csv_file <- "_forecast_table.csv"
    yaml_file <- "_metadata.yaml"
    json_file <- "_model_forecast.json"
    
    if (!is.null(date)) {
      unique_dates <- c(date)
      messageq(paste0("Zipping forecasts files for ", date))
    }
    
    for (forecast_day in unique_dates) {
      id_date_files <- c()
      zipfile <-
        paste0(proj_path, "/forecast_id_", forecast_day, ".zip")
      
      # Get all the values of that particular day in a data frame
      newdata <- subset(
        metadata,
        forecast_date == forecast_day,
        select = c("forecast_id", "forecast_date")
      )
      
      # For each forecast_id get 3 files
      All_ids <- newdata$"forecast_id"
      for (id in All_ids) {
        csv_file_path <- paste0(proj_path, "/forecast_id_", id, csv_file)
        if (file.exists(csv_file_path)) {
          id_date_files <- c(id_date_files, csv_file_path)
        }
        yaml_file_path <-
          paste0(proj_path, "/forecast_id_", id, yaml_file)
        if (file.exists(yaml_file_path)) {
          id_date_files <- c(id_date_files, yaml_file_path)
        }
        json_file_path <-
          paste0(proj_path, "/forecast_id_", id, json_file)
        if (file.exists(json_file_path)) {
          id_date_files <- c(id_date_files, json_file_path)
        }
      }
      
      if (length(id_date_files)) {
        # Zip forecast files for the current date
        zip_files(zipfile, id_date_files)
      }
    }
    
    # Zip forecasts_evaluations.csv
    eval_file <- paste0(proj_path, "/forecasts_evaluations.csv")
    zipfile <- paste0(proj_path, "/forecasts_evaluations.zip")
    zip_files(zipfile, eval_file)
  }
  
  if (type == "unzip") {
    messageq("Unzipping forecasts files")
    
    # Unzip files based on unique_dates
    for (forecast_day in unique_dates) {
      zipfile <- paste0(proj_path, "/forecast_id_", forecast_day, ".zip")
      zipfile <- normalizePath(zipfile, mustWork = FALSE)
      if (file.exists(zipfile)) {
        unzip(zipfile, exdir = proj_path, junkpaths = TRUE)
        unlink(zipfile)
      }
    }
    
    # Unzip forecasts_evaluations.csv
    eval_zip <- paste0(proj_path, "/forecasts_evaluations.zip")
    eval_zip <- normalizePath(eval_zip, mustWork = FALSE)
    if (file.exists(eval_zip)) {
      unzip(eval_zip, exdir = proj_path, junkpaths = TRUE)
      unlink(eval_zip)
    }
  }
}
