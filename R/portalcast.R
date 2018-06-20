#' @title Forecast Portal rodents
#'
#' @description Main function for controlling the running of potentially 
#'   multiple models for either a forecast or a hindcast
#'
#' @param type "forecast" or "hindcast"
#'
#' @param model_dir directory name where the model files reside
#'
#' @param temp_dir directory name for the temporary housing of predictions
#'
#' @param pred_dir directory name where the saved model predictions reside
#'
#' @param data_dir directory name where the in-use data reside
#'
#' @param models names of models to run or "all" to run them all
#'
#' @param ensemble logical indicator of whether to create an ensemble model
#'
#' @return Nothing
#'
#' @export
#'
portalcast <- function(type = "forecast", model_dir,
                       temp_dir,
                       pred_dir,
                       data_dir,
                       models = c("autoarima", "esss", "nbgarch", "pevgarch"),
                       ensemble = TRUE){

  dir.create(temp_dir)

  cat("Preparing data", "\n")
  if (type == "forecast"){
    #rodents <- prep_rodents(data_dir = data_dir)
    #covariates <- prep_covariates(data_dir = data_dir)
    #metadata <- prep_metadata(rodents, covariates, data_dir = data_dir)
  }

  forecast_date <- as.Date(metadata$forecast_date)
  if (!(forecast_date == Sys.Date())){
    stop("Data not updated")
  }

  cat("Checking model availability", "\n")
  #runnames <- select_models(model_dir, models)

  cat("Running models", "\n")
  sapply(runnames, source)

  cat("Compiling forecasts", "\n")
  #combined <- combine_forecasts(metadata)

  if (ensemble){
    cat("Creating ensemble model", "\n")
    #ensemble <- add_ensemble(metadata)
  }

  unlink(temp_dir)
}

#' @title Select models to forecast with
#'
#' @description Verify that models requested are available and select them
#'
#' @param model_dir directory name where the model files reside
#'
#' @param models names of models to run or "all" to run them all
#'
#' @return names of files to be run
#'
#' @export
#'
select_models <- function(model_dir, models){
  available <- list.files(model_dir)
  if (models[1] == "all"){
    runnames <- list.files(model_dir, full.names = TRUE)
  } else{
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      stop(paste0("Requested model ", models[-torun], " not in directory"))
    }
    runnames <- list.files(model_dir, full.names = TRUE)[torun]
  }
  return(runnames)
}