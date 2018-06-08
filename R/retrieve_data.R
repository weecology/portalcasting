#' @title Prepare historic rodent data
#'
#' @description Prepare the rodent dataset, tailored for forecasting. 
#'   Specifically, return data for all plots and control plots only, for each 
#'   species except PI and the total
#'
#' @param moons current newmoonnumber table
#'
#' @param start_newmoonnumber newmoon number of the first sample to be 
#'   included. Default value is \code{217}, corresponding to 1995-01-01.
#' 
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
prep_rodent_data <- function(moons = prep_moon_data(), 
                             start_newmoonnumber = 217){

  controls <- abundance(clean = FALSE, level = "Treatment", type = "Rodents", 
                        length = "Longterm", min_plots = 24) %>%
              select(-PI)
  controls <- controls %>%
              mutate(total = rowSums(controls[ , -(1:2)])) %>%
              filter(treatment == "control") %>%
              select(-treatment) %>%
              inner_join(moons, by = c("period" = "period")) %>%
              subset(newmoonnumber >= start_newmoonnumber) %>%
              select(-newmoondate, -censusdate)

  all <- abundance(clean = FALSE, level = "Site", type = "Rodents", 
                        length = "all", min_plots = 24) %>%
         select(-PI)
  all <- all %>%
         mutate(total = rowSums(all[ , -1])) %>%
         inner_join(moons, by = c("period" = "period")) %>%
         subset(newmoonnumber >= start_newmoonnumber) %>%
         select(-newmoondate, -censusdate)

  out <- list("controls" = controls, "all" = all)
  return(out)
}

#' @title Normalize path
#'
#' @description Return normalized path for all operating systems
#'
#' @param reference a path to join with current working directory
#'
#' @param base Current working directory else path given
#'
#' @return normalized path
#'
#' @examples
#' full_path("PortalData/Rodents/Portal_rodent.csv")
#'
#' full_path("PortalData/Rodents/Portal_rodent.csv", "~")
#'
#' @export
#'
full_path <- function(reference, base = getwd()){
  base <- normalizePath(base)
  path <- normalizePath(file.path(base, reference), mustWork = FALSE)
  return(path)
}

#' @title Prepare temporal (lunar) data
#'
#' @description Get time information (calendar dates, census periods, 
#'   newmoon numbers) associated with trapping events (achieved and missed)
#'   based on a lunar survey schedule.
#'
#' @param future logical value indicating if future moons (for forecasts) are
#'   to be added to the table or not
#'
#' @param forecast_date date from which forecasting is happening, typically
#'   today's date. Required if \code{future = TRUE}
#'
#' @return data table of time variables for all surveys
#'
#' @export
#' 
prep_moon_data <- function(future = FALSE, forecast_date = Sys.Date()){

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
#'
#' @description Gather the downloaded historical weather data, select the 
#'   columns needed, and remove the incomplete data
#'
#' @return weather data table 
#'
#' @export
#'
prep_weather_data <- function(){
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
#'
#' @description Gather the downloaded historical weather and ndvi data and
#'   join them into a single data table
#;
#' @return covariate data table 
#'
#' @export
#'
prep_hist_covariate_data <- function(){
  weather_data <- prep_weather_data()
  ndvi_data <- ndvi("newmoon", fill = TRUE)
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  return(out)
}
