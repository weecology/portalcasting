#' @title negative binomial Generalized AutoRegressive Conditional
#'   Heteroscedasticity model for Portal Predictions
#'
#' @description Model "nbGARCH" is a generalized autoregresive conditional 
#'   heteroscedasticity model with overdispersion (\emph{i.e.}, a negative 
#'   binomial response variable) fit to the data using \code{tsglm} in the
#'   \code{tscount} package (Liboschik \emph{et al}. 2017). 
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
#'   Liboschik T., Fokianos K., and Fried R. 2017. tscount: An R Package for 
#'   Analysis of Count Time Series Following Generalized Linear Models. 
#'   \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'   \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'
#' @export
#'
nbgarch <- function(abundances, metadata, level = "All", CL = 0.9){

  nfcnm <- length(metadata$rodent_forecast_newmoons)
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  fcast <- data.frame()
  aic <- data.frame()

  for (s in species){

    ss <- gsub("NA.", "NA", s)
    cat("Fitting nbGARCH model for", ss, "\n")

    sp_abund <- extract2(abundances, s)
  
    if (sum(sp_abund) == 0){
      model_fcast <- fcast0(nfcnm)
      model_aic <- 1e6
    } else{
      model <- tryCatch(
                 tsglm(sp_abund, model = list(past_obs = 1, past_mean = 12),
                   distr = "nbinom", link = "log"),
                 warning = function(x){NULL}, error = function(x){NULL})
      if(is.null(model)){
          model <- tsglm(sp_abund, 
                     model = list(past_obs = 1, past_mean = 12),
                     distr = "poisson", link = "log")
      }
      model_fcast <- predict(model, nfcnm, level = CL)
      model_aic <- AIC(model)
    }    

    estimate <- as.numeric(model_fcast$pred)
    LowerPI <- as.numeric(model_fcast$interval[ , 1]) 
    UpperPI <- as.numeric(model_fcast$interval[ , 2])

    fcast_s <- data.frame(date = metadata$forecast_date, 
                 forecastmonth = metadata$rodent_forecast_months,
                 forecastyear = metadata$rodent_forecast_years, 
                 newmoonnumber = metadata$rodent_forecast_newmoons,
                 currency = "abundance", model = "nbGARCH", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "nbGARCH", level = level, species = ss, 
               aic = model_aic, fit_start_newmoon = min(abundances$moons),
               fit_end_newmoon = max(abundances$moons),
               initial_newmoon = max(abundances$moons),
               stringsAsFactors = FALSE)

    fcast <- rbind(fcast, fcast_s)
    aic <- rbind(aic, aic_s)
  }
  output <- list("forecast" = fcast, "aic" = aic)
  return(output)
}
