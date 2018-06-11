#' @title Forecast Portal rodents
#'
#' @description Main function for controlling the running of potentially 
#'   multiple models for either a forecast or a hindcast
#'
#' @param type "forecast" or "hindcast"
#'
#' @param model_dir directory name where the model files reside
#'
#' @param models names of models to run or "all" to run them all
#'
#' @param ensemble logical indicator of whether to create an ensemble model
#'
#' @return Nothing
#'
#' @export
#'
portalcast <- function(type = "forecast", model_dir = "models", 
                       models = c("autoarima", "esss", "nbgarch", 
                                  "pevgarch"),
                       ensemble = TRUE){

  dir.create("tmp")

  cat("Preparing data", "\n")
  if (type == "forecast"){
    rodents <- prep_rodent_data()
    covariates <- prep_covariate_data()
    metadata <- prep_metadata(rodents, covariates)
  }

  forecast_date <- as.Date(metadata$forecast_date)
  if (!(forecast_date == Sys.Date())){
    stop("Data not updated")
  }

  available <- list.files("models")
  if (models[1] == "all"){
    runnames <- list.files("models", full.names = TRUE)
  } else{
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      stop(paste0("Requested model ", models[-torun], " not in directory"))
    }
    runnames <- list.files("models", full.names = TRUE)[torun]
  }

  cat("Running models", "\n")
  sapply(runnames, source)

  cat("Compiling forecasts", "\n")
  combine_forecasts(metadata)

  if (ensemble){
    cat("Creating ensemble model", "\n")
    add_ensemble(metadata)
  }

  unlink("tmp/*")
}