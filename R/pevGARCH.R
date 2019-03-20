#' @title poisson environmental variable Generalized Auto-Regressive 
#'   Conditional Heteroscedasticity model for Portal Predictions
#'
#' @description Fit a pevGARCH model in the portalcasting pipeline.
#'
#' @details Model "pevGARCH" is a generalized autoregresive conditional 
#'   heteroscedasticity model with a Poisson response variable  fit to the 
#'   data using \code{\link[tscount]{tsglm}} in the
#'   \href{http://tscount.r-forge.r-project.org/}{\code{tscount} package}
#'   (Liboschik \emph{et al}. 2017). 
#'
#'   For this model, environmental data are included as predictors of 
#'   abundance with an optional \code{lag} between the covariate values and 
#'   abundances (defaults to 6 newmoons).
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param level \code{character} value name of the type of plots included 
#'   (\code{"All"} or \code{"Controls"}).
#'
#' @param lag \code{integer} (or integer \code{numeric}) of the lag time to
#'   use for the covariates.
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
#' pevGARCH()
#' }
#' 
#' @export
#'
pevGARCH <- function(tree = dirtree(), level = "All", lag = 6, quiet = FALSE){
  check_args()
  messageq(paste0("### Fitting pevGARCH model for ", level, " ###"), quiet)

  abundances <- read_data(tree, tolower(level))
  metadata <- read_metadata(tree)
  covariates <- read_covariates(tree)

  fcnm <- metadata$rodent_forecast_newmoons
  nfcnm <- length(fcnm)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  covar_lag <- lag_covariates(covariates, lag, tail = TRUE)
  for_hist <- which(covar_lag$newmoonnumber %in% abundances$moons)
  for_fcast <- which(covar_lag$newmoonnumber %in% fcnm) 
  covar_hist <- covar_lag[for_hist, ]
  covar_fcast <- covar_lag[for_fcast, ]
        
  fcast <- data.frame()
  aic <- data.frame()

  models <- covariate_models("pevGARCH")

  for(s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting pevGARCH models for ", ss), quiet)

    abund_s <- extract2(abundances, s)
  
    if (sum(abund_s) == 0){
      spec_fcast <- fcast0(nfcnm)
      spec_aic <- 1e6
    } else{

      best_aic <- Inf
      best_model <- NA
      model_count <- 1

      for(m in models){
        model_name <- paste(m, collapse = ", ")
        msg <- paste0("Fitting Model ", model_count, ": ", model_name)
        messageq(msg, quiet)
        predictors <- NULL
        fcast_predictors <- NULL
        if (!(is.null(unlist(m)))){
          predictors <- select(covar_hist, unlist(m))
          fcast_predictors <- select(covar_fcast, unlist(m))
        }
        setup <- list(past_obs = 1, past_mean = 12)
        proposed_model <- tryCatch(
                            tsglm(abund_s, model = setup, distr = "poisson",
                              xreg = predictors, link = "log"), 
                            error = function(x) {NA})
        proposed_aic <- tryCatch(
                          AIC(proposed_model), 
                          error = function(x) {Inf})


        proposed_fcast <- tryCatch(
                            predict(proposed_model, nfcnm, level = CL, 
                              newxreg = fcast_predictors),
                            error = function(x) {NA})

        if(proposed_aic < best_aic){
          best_model <- proposed_model
          best_aic <- proposed_aic

          spec_forecast <- proposed_fcast
          spec_aic <- proposed_aic
        }

        model_count <- model_count + 1
      }

      fit_fails <- length(which(is.na(best_model) == TRUE))    
      if(fit_fails > 0){
        spec_forecast <- fcast0(nfcnm)
        spec_aic <- 1e6
      } 

    }

    estimate <- as.numeric(spec_forecast$pred)
    LowerPI <- as.numeric(spec_forecast$interval[, 1]) 
    UpperPI <- as.numeric(spec_forecast$interval[, 2])

    fcast_s <- data.frame(date = metadata$forecast_date, 
                 forecastmonth = metadata$rodent_forecast_months,
                 forecastyear = metadata$rodent_forecast_years, 
                 newmoonnumber = metadata$rodent_forecast_newmoons,
                 currency = "abundance", model = "pevGARCH", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "pevGARCH", level = level, species = ss, 
               aic = spec_aic, fit_start_newmoon = min(abundances$moons),
               fit_end_newmoon = max(abundances$moons),
               initial_newmoon = max(abundances$moons),
               stringsAsFactors = FALSE)

    fcast <- rbind(fcast, fcast_s)
    aic <- rbind(aic, aic_s)

  }
  output <- list("forecast" = fcast, "aic" = aic)
  return(output) 
}
