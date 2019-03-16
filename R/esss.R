#' @title Exponential Smoothing State Space model for Portal Predictions
#'
#' @description Fit an ESSS model in the portalcasting pipeline.
#'
#' @details Model "ESSS" is a flexible exponential smoothing state space 
#'  model with the possibility of multiplicative trends fit using 
#'  \code{\link[forecast]{ets}} in the
#'  \href{http://pkg.robjhyndman.com/forecast/}{\code{forecast} package}
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
#' @export
#'
ESSS <- function(tree = dirtree(), level = "All", quiet = FALSE){
  check_args()
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
    messageq(paste0("Fitting ESSS model for ", ss), quiet)

    abund_s <- extract2(abundances, s)
  
    if (sum(abund_s) == 0){
      model_fcast <- fcast0(nfcnm, "mean")
      model_aic <- 1e6
      estimate <- as.numeric(model_fcast$mean)
      LowerPI <- rep(0, nfcnm)
      UpperPI <- rep(0, nfcnm)
    } else{
      model <- ets(abund_s)
      model_fcast <- forecast(model, h = nfcnm, level = CL, 
                       allow.multiplicative.trend = TRUE)
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
                 currency = "abundance", model = "ESSS", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "ESSS", level = level, species = ss, aic = model_aic, 
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
