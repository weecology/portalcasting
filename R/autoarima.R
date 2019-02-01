#' @title Flexible Auto-Regressive Integrated Moving Average model for Portal 
#'  Predictions
#'
#' @description Fit an AutoARIMA model in the portalcasting pipeline. 
#'
#' @details Model "AutoARIMA" is a flexible auto-regressive integrated 
#'  moving average model fit to the data using 
#'  \code{\link[forecast]{auto.arima}} in the
#'  \href{http://pkg.robjhyndman.com/forecast/}{\code{forecast} package}
#'  (Hyndman \emph{et al}. 2018).
#'
#'  Because the seasonality and sampling occurring with different frequencies,
#'  which \code{\link[forecast]{auto.arima}} cannot accommodate, seasonal 
#'  models are not included.
#'
#'  Although \code{\link[forecast]{auto.arima}} can handle missing data, the
#'  other models used currently cannot, so we interpolate missing data here 
#'  for comparison.
#'
#' @param abundances Class-\code{rodents} \code{data.frame} table of rodent 
#'   abundances and time measures.
#'
#' @param metadata Class-\code{metadata} model metadata \code{list}.
#'
#' @param level \code{character} value name of the type of plots included 
#'   (\code{"All"} or \code{"Controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'   quiet.
#'
#' @return \code{list} of [1] \code{"forecast"} (the forecasted abundances)
#'   and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references 
#'  Hyndman R., Bergmeir C., Caceres G., Chhay L., O'Hara-Wild M., Petropoulos
#'  F., Razbash S., Wang E., and Yasmeen F. 2018. forecast: Forecasting 
#'  functions for time series and linear models. 
#'  \href{http://pkg.robjhyndman.com/forecast}{R package version 8.3}. 
#'
#' @export
#'
AutoArima <- function(abundances, metadata, level = "All", quiet = FALSE){
  if (!("rodents" %in% class(abundances))){
    stop("`abundances` is not of class rodents")
  }
  if (!("logical" %in% class(quiet))){
    stop("`quiet` is not of class logical")
  }
  if (length(level) > 1){
    stop("`level` can only be of length = 1")
  }
  if (!is.character(level)){
    stop("`level` is not a character")
  }
  if (!any(c("All", "Controls") %in% level)){
    stop("`level` is not valid option")
  } 
  if (!("metadata" %in% class(metadata))){
    stop("`metadata` is not a metadata list")
  } 
  nfcnm <- length(metadata$rodent_forecast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  fcast <- data.frame()
  aic <- data.frame()

  for (s in species){

    ss <- gsub("NA.", "NA", s)
    if (!quiet){
      message(paste0("Fitting AutoArima model for ", ss))
    }
    abund_s <- extract2(abundances, s)
  
    if (sum(abund_s) == 0){
      model_fcast <- fcast0(nfcnm, "mean")
      model_aic <- 1e6
      estimate <- as.numeric(model_fcast$mean)
      LowerPI <- rep(0, nfcnm)
      UpperPI <- rep(0, nfcnm)

    } else{
      model <- auto.arima(abund_s)
      model_fcast <- forecast(model, h = nfcnm, level = CL, fan = TRUE)
      model_aic <- model$aic
      estimate <- as.numeric(model_fcast$mean)
      CI_match <- which(model_fcast$level == CL * 100)
      LowerPI <- as.numeric(model_fcast$lower[ , CI_match]) 
      UpperPI <- as.numeric(model_fcast$upper[ , CI_match])
    }    


    fcast_s <- data.frame(date = metadata$forecast_date, 
                 forecastmonth = metadata$rodent_forecast_months,
                 forecastyear = metadata$rodent_forecast_years, 
                 newmoonnumber = metadata$rodent_forecast_newmoons,
                 currency = "abundance", model = "AutoArima", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "AutoArima", level = level, species = ss, 
               aic = model_aic, fit_start_newmoon = min(abundances$moons),
               fit_end_newmoon = max(abundances$moons),
               initial_newmoon = max(abundances$moons),
               stringsAsFactors = FALSE)

    fcast <- rbind(fcast, fcast_s)
    aic <- rbind(aic, aic_s)
  }
  list("forecast" = fcast, "aic" = aic)
}