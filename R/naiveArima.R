#' @title Naive Arima model for Portal Predictions
#'
#' @description Fit a Naive Arima model in the portalcasting pipeline.
#'
#' @details Model "naiveArima" is a naive baseline model commonly used for  
#' forecasting. It is sometimes referred to as a random walk forecast with 'no  
#' drift'. Operationally, it is implemented as an ARIMA(0,1,0) using 
#' \code{\link[forecast]{Arima}} in the \href{http://pkg.robjhyndman.com/forecast/}{\code{forecast} package}
#'  (Hyndman \emph{et al}. 2018).
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
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
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' naiveArima()
#' }
#' 
#' @export
#'
naiveArima <- function(tree = dirtree(), level = "All", quiet = FALSE){
  check_args() 
  messageq(paste0("### Fitting naiveArima model for ", level, " ###"), quiet)
  
  abundances <- read_data(tree, tolower(level))
  metadata <- read_metadata(tree)
  
  nfcnm <- length(metadata$rodent_forecast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  fcast <- data.frame()
  aic <- data.frame()
  
  for (s in species){
    
    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting naiveArima model for ", ss), quiet)
    
    abund_s <- extract2(abundances, s)
    model <- Arima(abund_s, order=c(0,1,0))
    model_fcast = forecast(model, h=nfcnm, level= CL, fan=TRUE)
    
    model_aic <- model$aic
    estimate <- as.numeric(model_fcast$mean)
    CI_match <- which(model_fcast$level == CL * 100)
    LowerPI <- as.numeric(model_fcast$lower[ , CI_match])
    UpperPI <- as.numeric(model_fcast$upper[ , CI_match])
    
    fcast_s <- data.frame(date = metadata$forecast_date, 
                          forecastmonth = metadata$rodent_forecast_months,
                          forecastyear = metadata$rodent_forecast_years, 
                          newmoonnumber = metadata$rodent_forecast_newmoons,
                          currency = "abundance", model = "naiveArima", level = level, 
                          species = ss, estimate = estimate, LowerPI = LowerPI, 
                          UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                          fit_end_newmoon = max(abundances$moons),
                          initial_newmoon = max(abundances$moons),
                          stringsAsFactors = FALSE)
    
    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance",
                        model = "naiveArima", level = level, species = ss, aic = model_aic,
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

