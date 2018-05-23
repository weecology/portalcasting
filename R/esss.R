#' @title Exponential Smoothing State Space model for Portal Predictions
#'
#' @description Model "ESSS" is a flexible exponential smoothing state space 
#'  model fit using the forecast and ets functions with the possibility of 
#'  multiplicative trends. Unfortunately since the seasonality and 
#'  sampling occurring with different frequencies, which the ets function 
#'  cannot accommodate, seasonal models are not included.
#'
#' @param abundances table of rodent abundances and time measures
#' @param forecast_date the dates to be forecast from
#' @param forecast_months the months of the dates to be forecast
#' @param forecast_years the years of the dates to be forecast
#' @param forecast_newmoons the numbers of the new moons to be forecast
#' @param level name of the type of plots included ("All" or "Controls")
#' @param num_forecast_newmoons number of new moons to forecast
#' @param CI_level confidence interval level used for forecast envelope
#' @return list of forecast and aic tables
#'
#' @export
#'
forecast_esss <- function(abundances, forecast_date, forecast_months, 
                          forecast_years, forecast_newmoons, level,
                          num_forecast_newmoons, CI_level = 0.9){

  cat("Fitting ESSS model for total \n")
  interpolated_abundances <- interpolate_abundance(abundances)

  ets_model <- ets(interpolated_abundances$total)
  ets_forecast <- forecast(ets_model, h = num_forecast_newmoons,
                           level = CI_level, 
                           allow.multiplicative.trend = TRUE)

  fit_start_newmoon <- min(abundances$newmoonnumber)
  fit_end_newmoon <- max(abundances$newmoonnumber)
  initial_newmoon <- max(abundances$newmoonnumber)
  CI_match <- which(ets_forecast$level == CI_level * 100)
  estimate <- as.numeric(ets_forecast$mean)
  LowerPI <- as.numeric(ets_forecast$lower[ , CI_match]) 
  UpperPI <- as.numeric(ets_forecast$upper[ , CI_match])

  output_fcast <- data.frame(date = forecast_date, 
                             forecastmonth = forecast_months,
                             forecastyear = forecast_years, 
                             newmoonnumber = forecast_newmoons,
                             currency = "abundance",
                             model = "ESSS", level = level, 
                             species = "total", estimate = estimate,
                             LowerPI = LowerPI, UpperPI = UpperPI,
                             fit_start_newmoon = fit_start_newmoon,
                             fit_end_newmoon = fit_end_newmoon,
                             initial_newmoon = initial_newmoon,
                             stringsAsFactors = FALSE)

  output_aic <- data.frame(date = as.Date(forecast_date), 
                           currency = "abundance", model = "ESSS", 
                           level = level, species = "total", 
                           aic = as.numeric(ets_model$aic), 
                           fit_start_newmoon = fit_start_newmoon,
                           fit_end_newmoon = fit_end_newmoon,
                           initial_newmoon = initial_newmoon,
                           stringsAsFactors = FALSE)

  output <- list(output_fcast, output_aic)
  names(output) <- c("forecast", "aic")

  return(output)
}
