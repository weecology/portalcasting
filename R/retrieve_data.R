#' @title Normalize Path
#' @description Return normalized path for all operating systems
#' @param reference a path to join with current working directory
#' @param base Current working directory else path given
#'
#' @return normalized path
#' @export
#' @examples
#' full_path("PortalData/Rodents/Portal_rodent.csv")
#' full_path("PortalData/Rodents/Portal_rodent.csv", "~")
#'
full_path <- function(reference, base = getwd()){
  base <- normalizePath(base)
  path <- normalizePath(file.path(base, reference), mustWork = FALSE)
  return(path)
}

#' @title Collect Moon Data
#' @description Get dates and data for newmoonnumbers and related trapping 
#'   periods
#' @param future logical value indicating if future moons (for forecasts) are
#'   to be added to the table or not
#' @param forecast_date date from which forecasting is happening, typically
#'   today's date. Required if \code{future = TRUE}
#' @return newmoons table
#' @export
#' 
get_moon_data <- function(future = FALSE, forecast_date = Sys.Date()){
  path <- full_path("PortalData/Rodents/moon_dates.csv", "~")
  moons <- read.csv(path, header = TRUE)
  
  moons$year <- year(moons$newmoondate)
  moons$month <- month(moons$newmoondate)
  moons$newmoondate <- as.Date(as.character(moons$newmoondate))

  if (future == TRUE){
    if (!is.Date(forecast_date)){
      stop("forecast_date must be a date.")
    }
    curr_moons <- moons %>%
                  select(newmoonnumber, newmoondate, period, censusdate)
    future_moons <- get_future_moons(moons)
    total_moons <- rbind(curr_moons, future_moons)

    not_future_moons <- which(future_moons$newmoondate < forecast_date)
    nnfm <- length(not_future_moons)
    if (nnfm > 0){
      add_nfm <- future_moons[not_future_moons, ]
      add_nfm$year <- as.numeric(format(add_nfm$newmoondate, "%Y"))
      add_nfm$month <- as.numeric(format(add_nfm$newmoondate, "%m"))
      moons <- rbind(moons, add_nfm)
      curr_moons <- moons %>% 
                    select(newmoonnumber, newmoondate, period, censusdate)
      future_moons <- get_future_moons(moons)
      total_moons <- rbind(curr_moons, future_moons)
    }
    moons <- total_moons
  }
  return(moons)
}

#' @title Collect Historic Weather Data
#' @description Gather the downloaded historical weather data
#' @return weather data table 
#' @export
#'
get_weather_data <- function(){
  cols <- c("mintemp", "maxtemp", "meantemp", "precipitation", 
            "newmoonnumber")
  weather_data <- weather("newmoon", fill = TRUE) %>% 
                  ungroup() %>%
                  select(cols)
  incompletes <- which(is.na(weather_data$newmoonnumber))
  if (length(incompletes) > 0){
    weather_data <- weather_data[-incompletes, ]
  }
  return(weather_data)
}

#' @title Collect Historic Weather and NDVI Data
#' @description Gather the downloaded historical weather and ndvi data
#' @return covariate data table 
#' @export
#'
get_covariate_data <- function(){
  weather_data <- get_weather_data()
  ndvi_data <- ndvi("newmoon", fill = TRUE)
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  return(out)
}

#' @title Collect Historic Rodent Data
#' @description Get rodent data, tailored for forecasting (all plots and 
#'   controls only)
#' @param moons current newmoonnumber table
#' @param forecast_date date the forecast is run
#' 
#' @return a list of two dataframes, all plots and control plots
#' @export
#'
get_rodent_data <- function(moons, forecast_date){

  # Corresponding to Jan 1995
  historic_start_period <- 203
  historic_start_newmoon <- 217
  
  # Control plot data
  controls <- portalr::abundance(clean = FALSE, level = "Treatment",
                                 type = "Rodents", length = "Longterm",
                                 min_plots = 24)
  # Drop PI
  controls <- controls[ , -which(colnames(controls) == "PI")]
  # The total rodent count in each treatment
  controls$total = rowSums(controls[,-(1:2)])
  # Drop non-control treatments and add in newmoonnumber
  controls <- controls %>%
              filter(treatment == 'control') %>%
              select(-treatment) %>%
              inner_join(moons, by = c("period" = "period")) %>%
              subset(newmoonnumber >= historic_start_newmoon) %>%
              select(-newmoondate, -censusdate)
  
  # All plot data
  all <- abundance(clean = FALSE, level = "Site", type = "Rodents",
                   length = "all", min_plots = 24)
  # Drop PI
  all <- all[ , -which(colnames(all) == "PI")]
  # The total rodent count across the entire site
  all$total <- rowSums(all[,-(1)])
  all <- all %>% 
         inner_join(moons, by = c("period" = "period")) %>%
         subset(period >= historic_start_period) %>%
         select(-newmoondate, -censusdate)
    
  rodent_data <- list()
  rodent_data$controls <- controls
  rodent_data$all <- all
  return(rodent_data)
}
