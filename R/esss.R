#' @title Exponential Smoothing State Space model for Portal Predictions
#'
#' @description Model "ESSS" is a flexible exponential smoothing state space 
#'  model fit using \code{ets} in \code{forecast} package (Hyndman et al.
#'  2018) with the possibility of multiplicative trends.  
#'
#'  Because the seasonality and sampling occurring with different frequencies,
#'  which \code{ets} cannot accommodate, seasonal models are not included.
#'
#' @param abundances table of rodent abundances and time measures
#'
#' @param metadata model metadata list
#'
#' @param level name of the type of plots included ("All" or "Controls")
#'
#' @param CL confidence interval level used for forecast envelope
#'
#' @return list of forecast and aic tables
#'
#' @references 
#'  Hyndman R., Bergmeir C., Caceres G., Chhay L., O'Hara-Wild M., Petropoulos
#'  F., Razbash S., Wang E., and Yasmeen F. 2018. forecast: Forecasting 
#'  functions for time series and linear models. 
#'  \href{http://pkg.robjhyndman.com/forecast}{R package version 8.3}. 
#'
#' @export
#'
esss <- function(abundances, metadata, level, CL = 0.9){

  nfcnm <- length(metadata$rodent_forecast_newmoons)
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  fcast <- data.frame()
  aic <- data.frame()

  for (s in species){

    ss <- gsub("NA.", "NA", s)
    cat("Fitting ESSS model for", ss, "\n")

    sp_abundance <- extract2(abundances, s)
  
    if (sum(sp_abundance) == 0){
      fcast_s <- fcast0(nfcnm)
      aic_s <- 1e6
    } else{
      model <- ets(sp_abundance)
      fcast_s <- forecast(model, h = nfcnm, level = CL, 
                   allow.multiplicative.trend = TRUE)
      aic_s <- model$aic
    }    

    estimate <- as.numeric(fcast_s$mean)
    CI_match <- which(fcast_s$level == CL * 100)
    LowerPI <- as.numeric(fcast_s$lower[ , CI_match]) 
    UpperPI <- as.numeric(fcast_s$upper[ , CI_match])

    fcast_s <- data.frame(date = metadata$forecast_date, 
                 forecastmonth = metadata$rodent_forecast_months,
                 forecastyear = metadata$rodent_forecast_years, 
                 newmoonnumber = metadata$rodent_forecast_newmoons,
                 currency = "abundance", model = "ESSS", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "ESSS", level = level, species = ss, aic = aic_s, 
               fit_start_newmoon = min(abundances$moons),
               fit_end_newmoon = max(abundances$moons),
               initial_newmoon = max(abundances$moons),
               stringsAsFactors = FALSE)

    fcast <- rbind(fcast, fcast_s)
    aic <- rbind(aic, aic_s)
  }
  output <- list("forecast" = fcast, "aic" = aic)
  return(output)
}
