#' @title Prepare data subdirectory for a forecast run or hindcast runs
#'
#' @description Prepare the data directory for a forecasting run or a set
#'   of hindcasting runs
#'
#' @param options_data the data options list
#'
#' @return nothing
#'
#' @export
#'
prep_data <- function(options_data = data_options()){
  if (!options_data$quiet){
    cat("Preparing data", "\n")
  }
  metadata_path <- file_path(options_data$tree, "data/metadata.yaml")
  if (!file.exists(metadata_path)){
    fill_data(options_data)
  }
  metadata <- yaml.load_file(metadata_path)    
  if (metadata$forecast_date != today()){
    fill_data(options_data)
  }
}

