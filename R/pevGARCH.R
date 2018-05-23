#' @title poisson environmental variable Generalized Auto-Regressive 
#'   Conditional Heteroscedasticity model for Portal Predictions
#'
#' @description Model "pevGARCH" is a generalized autoregresive conditional 
#'   heteroscedasticity model with a Poisson response variable
#'   For this model, environmental data are included as predictors of ,
#'   abundance but with a 6 month lag between the covariate values and the 
#'   abundances.
#'
#' @param abundances table of rodent abundances and time measures
#' @param forecast_date the dates to be forecast from
#' @param forecast_months the months of the dates to be forecast
#' @param forecast_years the years of the dates to be forecast
#' @param forecast_newmoons the numbers of the new moons to be forecast
#' @param level name of the type of plots included ("All" or "Controls")
#' @param num_forecast_newmoons number of new moons to forecast
#' @param CI_level confidence interval level used for forecast envelope
#' @param weather_data table of already lagged weather data
#' @param weather_forecast table of forecasted weather
#' @param ndvi_data table of already lagged NDVI data
#' @param ndvi_forecast table of forecasted NDVI
#' @return list of forecast and aic tables
#'
#' @export
#'
forecast_pevgarch <- function(abundances, forecast_date, forecast_months, 
                              forecast_years, forecast_newmoons, level,
                              num_forecast_newmoons, CI_level = 0.9,
                              weather_data, weather_fcast,
                              ndvi_data, ndvi_fcast){

  species <- colnames(abundances)[2:(ncol(abundances) - 3)]

  interpolated_abundances <- interpolate_abundance(abundances)

  fit_start_newmoon <- min(abundances$newmoonnumber)
  fit_end_newmoon <- max(abundances$newmoonnumber)
  initial_newmoon <- max(abundances$newmoonnumber)

  zero_abund_mean <- rep(0, num_forecast_newmoons)
  zero_abund_interval <- matrix(0, ncol = 2, nrow = num_forecast_newmoons)
  colnames(zero_abund_interval) <- c("lower", "upper")

  zero_abund_forecast <- list(zero_abund_mean, zero_abund_interval)
  names(zero_abund_forecast) <- c("pred", "interval")

  cov_data <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  cov_nm <- cov_data$newmoonnumber
  abund_nm <- interpolated_abundances$moons
  included_nm <- which(cov_nm %in% abund_nm)
  cov_data <- cov_data[included_nm, ]
  fcast_data <- right_join(weather_fcast, ndvi_fcast, by = "newmoonnumber")

  covariates <- list(c("maxtemp", "meantemp", "precipitation", "ndvi"),
                     c("maxtemp", "mintemp", "precipitation", "ndvi"),
                     c("mintemp", "maxtemp", "meantemp", "precipitation"),
                     c("precipitation", "ndvi"),
                     c("mintemp", "ndvi"),
                     c("mintemp"),
                     c("maxtemp"),
                     c("meantemp"),
                     c("precipitation"),
                     c("ndvi"),
                     c(NULL))
  output_fcast <- data.frame()
  output_aic <- data.frame()

  for(s in species){

    ss <- s
    if(ss == "NA."){
      ss <- "NA"
    }
    cat("Fitting Poisson environmental GARCH models for", ss, "\n")

    species_abundance <- interpolated_abundances %>% 
                         extract2(s)
  
    if(sum(species_abundance) == 0){

      spec_forecast <- zero_abund_forecast
      spec_aic <- 1e6

    } else{

      best_model_aic <- Inf
      best_model <- NA
      model_count <- 1

      for(proposed_covariates in covariates){
        cat("Fitting Model", model_count, "\n")
        predictors <- cov_data[ , unlist(proposed_covariates)]

        if(length(proposed_covariates) > 0){
          fcast_predictors <- fcast_data[ , unlist(proposed_covariates)]
        } else{
          fcast_predictors <- NULL
        }
        model_setup <- list(past_obs = 1, past_mean = 12)
        prop_model <- tryCatch(tsglm(species_abundance, 
                                              model = model_setup, 
                                              distr = "poisson",
                                              xreg = predictors, 
                                              link = "log"),
                               error = function(x) {NA})
        prop_model_aic <- tryCatch(AIC(prop_model), 
                                   error = function(x) {Inf})
        prop_forecast <- tryCatch(predict(prop_model, 
                                          num_forecast_newmoons, 
                                          level = CI_level, 
                                          newxreg = fcast_predictors),
                                  error = function(x) {NA})

        if(prop_model_aic < best_model_aic){
          best_model <- prop_model
          best_model_aic <- prop_model_aic

          spec_forecast <- prop_forecast
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
