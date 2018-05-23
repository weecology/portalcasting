#' @title Combine Forecasts and Add Ensembles
#' 
#' @description Combine all new forecasts (from the tmp directory), add 
#'   ensembles
#' 
#' @param model_metadata model metadata list
#' @return list of [1] the forecasts and [2] the model AIC values
#' @export
#'
forecastall <- function(model_metadata){
  
  forecast_date <- model_metadata$forecast_date
  filename_suffix <- model_metadata$filename_suffix
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files("tmp", pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  fcasts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))

  file_ptn <- paste(filename_suffix, "_model_aic.csv", sep = "")
  files <- list.files("tmp", pattern = file_ptn, full.names = TRUE)

  aics <- do.call(rbind, lapply(files, read.csv, na.strings = ""))
  
  fcast_date <- as.character(forecast_date)
  fcast_fname <- paste(fcast_date, filename_suffix, ".csv", sep = "")
  forecast_filename <- file.path("predictions", fcast_fname)
  aic_fname <- paste(fcast_date, filename_suffix, "_model_aic.csv", sep = "")
  model_aic_filename <- file.path("predictions", aic_fname)
  append_csv(fcasts, forecast_filename)
  append_csv(aics, model_aic_filename)
  
  ensemble <- make_ensemble(fcasts) %>% 
              subset(select = colnames(fcasts))
  append_csv(ensemble, forecast_filename)
  
  return(list(forecasts = fcasts, all_model_aic = aics))
}