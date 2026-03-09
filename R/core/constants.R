#' @title Package-Wide Constants
#'
#' @description Constants used throughout the portalcasting package for file extensions,
#'              default paths, and operation types.
#'
#' @name constants
#'
#' @family core
#'
NULL

# File extensions for forecasts
FORECAST_FILE_CSV <- "_forecast_table.csv"
FORECAST_FILE_YAML <- "_metadata.yaml"
FORECAST_FILE_JSON <- "_model_forecast.json"

# Default paths
DEFAULT_FORECAST_PATH <- "forecasts/"

# Zip operation types
ZIP_OPERATION_ZIP <- "zip"
ZIP_OPERATION_UNZIP <- "unzip"
