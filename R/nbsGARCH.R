#' @title negative binomial seasonal Generalized AutoRegressive Conditional
#'   Heteroscedasticity model for Portal Predictions
#'
#' @description Fit an nbsGARCH model in the portalcasting pipeline.
#'
#' @details Model "nbsGARCH" is a seasonal generalized autoregresive 
#'   conditional heteroscedasticity model with overdispersion 
#'   (\emph{i.e.}, a negative binomial response variable) fit to the data
#'   using \code{\link[tscount]{tsglm}} in the
#'   \href{http://tscount.r-forge.r-project.org/}{\code{tscount} package}
#'   (Liboschik \emph{et al}. 2017). Seasonal dynamics are modeled using
#'   a two-term Fourier transformation.
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
#'   Liboschik T., Fokianos K., and Fried R. 2017. tscount: An R Package for 
#'   Analysis of Count Time Series Following Generalized Linear Models. 
#'   \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'   \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' nbsGARCH()
#' }
#'
#' @export
#'
nbsGARCH <- function(tree = dirtree(), level = "All", quiet = FALSE){
  check_args()
  messageq(paste0("### Fitting nbsGARCH model for ", level, " ###"), quiet)
  abundances <- read_data(tree, tolower(level))
  metadata <- read_metadata(tree)
  moons <- read_moons(tree)
  moon_foys <- foy(moons$newmoondate)
  sin2pifoy <- sin(2 * pi * moon_foys)
  cos2pifoy <- cos(2 * pi * moon_foys)
  fouriers <- data.frame(sin2pifoy, cos2pifoy)
  fcnm <- metadata$rodent_forecast_newmoons
  nfcnm <- length(metadata$rodent_forecast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  for_hist <- which(moons$newmoonnumber %in% abundances$moons)
  for_fcast <- which(moons$newmoonnumber %in% fcnm) 
  predictors <- fouriers[for_hist, ]
  fcast_predictors <- fouriers[for_fcast, ]

  fcast <- data.frame()
  aic <- data.frame()
  
  for (s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting nbsGARCH model for ", ss), quiet)

    abund_s <- extract2(abundances, s)
    past <- list(past_obs = 1, past_mean = 12)
    if (sum(abund_s) == 0){
      model_fcast <- fcast0(nfcnm)
      model_aic <- 1e6
    } else{
      model <- tryCatch(
                 tsglm(abund_s, model = past, distr = "nbinom", 
                       xreg = predictors, link = "log"),
                 warning = function(x){NULL}, error = function(x){NULL})
      if(is.null(model) || AIC(model) == Inf){
          model <- tryCatch(
                     tsglm(abund_s, model = past, distr = "poisson", 
                           xreg = predictors, link = "log"),
                     warning = function(x){NULL}, error = function(x){NULL})
      }
      if(is.null(model) || AIC(model) == Inf){
        model_fcast <- fcast0(nfcnm)
        model_aic <- 1e6
      } else {
        model_fcast <- predict(model, nfcnm, level = CL, 
                              newxreg = fcast_predictors)
        model_aic <- AIC(model)
      }
    }    

    estimate <- as.numeric(model_fcast$pred)
    LowerPI <- as.numeric(model_fcast$interval[ , 1]) 
    UpperPI <- as.numeric(model_fcast$interval[ , 2])

    fcast_s <- data.frame(date = metadata$forecast_date, 
                 forecastmonth = metadata$rodent_forecast_months,
                 forecastyear = metadata$rodent_forecast_years, 
                 newmoonnumber = metadata$rodent_forecast_newmoons,
                 currency = "abundance", model = "nbsGARCH", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "nbsGARCH", level = level, species = ss, 
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
