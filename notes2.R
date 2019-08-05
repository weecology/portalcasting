
#' @importFrom curl curl new_handle
#' @importFrom DesignLibrary match.call.defaults
#' @importFrom digest digest
#' @importFrom dplyr arrange bind_rows filter full_join group_by inner_join 
#'   left_join mutate n rename right_join select summarise summarize ungroup 
#' @importFrom forecast auto.arima ets forecast na.interp
#' @importFrom graphics abline axis mtext par plot points polygon rect text
#' @importFrom grDevices rgb
#' @importFrom httr authenticate content headers http_type GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_date is.Date month year
#' @importFrom magrittr %>% extract2
#' @importFrom portalr download_observations fcast_ndvi 
#'   find_incomplete_censuses get_future_moons ndvi summarize_rodent_data 
#'   weather
#' @importFrom purrr map
#' @importFrom RCurl basicTextGatherer getURL curlOptions curlPerform 
#'   getCurlHandle
#' @importFrom readr read_csv
#' @importFrom rlang !! !!! .data quo quos
#' @importFrom stats AIC lm na.omit predict qnorm quantile runif setNames
#' @importFrom tidyselect one_of
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom tscount tsglm
#' @importFrom usethis use_data
#' @importFrom utils download.file packageDescription read.csv read.table 
#'   tail unzip write.csv write.table
#' @importFrom viridis viridis
#' @importFrom yaml as.yaml yaml.load_file



# To quiet concerns of R CMD check re: variables used in non-standard eval
if (getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(".", "aic", "censusdate", "currency", "date", "date_made", "delta_aic", 
      "ensemble_estimate", "ensemble_var", 
      "estimate", "fit_end_newmoon", "fit_start_newmoon", 
      "forecast_newmoon", "forecastmonth", "forecastyear", "initial_newmoon",
      "level", "LowerPI", "maxtemp", "meantemp", "mintemp", "model_var",  
      "newmoondate", "newmoonnumber", "period", "PI", "precipitation", 
      "species", "sum_weight", "treatment", "locally_measured", "battery_low",
      "UpperPI", "weight", "weighted_ss", ".mapUnicode")
  )
}
there should basically be a way then to give name and cri and thats it!


down<-zenodo_url("1215988", "latest")


dest <- paste0(".\\data.zip")
download.file(down, dest)

the datasets are held in a list

each dataset is an element
type
source
name

download(name = "PortalData", type = "zenodo", 
                 concept_rec_id = "1215988")

download(name = "PortalPredictions", type = "zenodo", 
                 concept_rec_id = "833438")


downloads <- list(PD = download, PP = download2)

?download.file
