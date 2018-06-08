#' @title poisson environmental variable Generalized Auto-Regressive 
#'   Conditional Heteroscedasticity model for Portal Predictions
#'
#' @description Model "pevGARCH" is a generalized autoregresive conditional 
#'   heteroscedasticity model with a Poisson response variable  fit to the 
#'   data using \code{tsglm} in the \code{tscount} package (Liboschik 
#'   \emph{et al}. 2017). 
#'
#'   For this model, environmental data are included as predictors of ,
#'   abundance but with a 6 month lag between the covariate values and the 
#'   abundances.
#'
#' @param abundances table of rodent abundances and time measures
#'
#' @param covariates table of covariate data and time measures
#'
#' @param covariates_fcast table of forecast covariate data and time measures
#'
#' @param metadata model metadata list
#'
#' @param level name of the type of plots included ("All" or "Controls")
#'
#' @param lag the lag used for the covariate data
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
pevgarch <- function(abundances, covariates, covariates_fcast, metadata,  
                     level = "All", lag = 6){

  nfcnm <- length(metadata$rodent_forecast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]

        
#
# i'm actively working here and need to make sure things flow with the 
#  analysis. right now the historical covariate data need to be trimmed
#  i think the "lag_covariates" function might need to be re-done 

  covar_lag <- lag_covariates(covariates, covariates_fcast, metadata, lag)
  covar_hist <- covar_lag$hist_lag
  covar_fcast <- covar_lag$fcast_lag

  fcast <- data.frame()
  aic <- data.frame()

  models <- covariate_models("pevgarch")

  for(s in species){

    ss <- gsub("NA.", "NA", s)
    cat("Fitting Poisson environmental GARCH models for", ss, "\n")

    abund_s <- extract2(abundances, s)
  
    if (sum(abund_s) == 0){
      model_fcast <- fcast0(nfcnm)
      model_aic <- 1e6
    } else{

      best_aic <- Inf
      best_model <- NA
      model_count <- 1

      for(m in models){
        model_name <- paste(m, collapse = ", ")
        cat("Fitting Model ", model_count, ": ", model_name, "\n", sep = "")

        predictors <- select(covar_hist, unlist(m))
        fcast_predictors <- select(covar_fcast, unlist(m))
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

        if(prop_model_aic < best_model_aic){
          best_mod <- proposed_model
          best_model_aic <- proposed_aic

          forecast_s <- prop_forecast
          spec_aic <- prop_model_aic
        }

        model_count <- model_count + 1
      }

      fit_fails <- length(which(is.na(best_model) == T))
        
      if(fit_fails > 0){
        spec_forecast <- zero_abund_forecast
        spec_aic <- 1e6
      } 

    }

    estimate <- as.numeric(spec_forecast$pred)
    LowerPI <- as.numeric(spec_forecast$interval[, 1]) 
    UpperPI <- as.numeric(spec_forecast$interval[, 2])
    spec_output_fcast <- data.frame(date = forecast_date, 
                                    forecastmonth = forecast_months, 
                                    forecastyear = forecast_years,
                                    newmoonnumber = forecast_newmoons, 
                                    currency = "abundance", 
                                    model = "pevGARCH", level = level, 
                                    species = ss, estimate = estimate,
                                    LowerPI = LowerPI, UpperPI = UpperPI,
                                    fit_start_newmoon = fit_start_newmoon,
                                    fit_end_newmoon = fit_end_newmoon,
                                    initial_newmoon = initial_newmoon,
                                    stringsAsFactors = FALSE)
    output_fcast <- rbind(output_fcast, spec_output_fcast)

    spec_output_aic <- data.frame(date = forecast_date, 
                                  currency = "abundance", model = "pevGARCH", 
                                  level = level, species = ss, 
                                  aic = as.numeric(spec_aic), 
                                  fit_start_newmoon = fit_start_newmoon,
                                  fit_end_newmoon = fit_end_newmoon,
                                  initial_newmoon = initial_newmoon,
                                  stringsAsFactors = FALSE)

    output_aic <- rbind(output_aic, spec_output_aic)

  }
  output <- list(output_fcast, output_aic)
  names(output) <- c("forecast", "aic")

  return(output) 
}
