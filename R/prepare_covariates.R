#' @title Prepare Covariate Data for Casting
#'
#' @description Prepare and combine the historical and cast covariate data for a model run. \cr \cr 
#'              \code{prepare_historical_covariates} and \code{prepare_forecast_covariates} ready the historical and -casted covariates, respectively. \cr \cr
#'              \code{prepare_covariates} combines \code{prepare_historical_covariates} and \code{prepare_forecast_covariates}.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param origin \code{Date} for the forecast origin. Defaults to today's date.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be printed.
#'
#' @return \code{data.frame} of historical and forecasted covariates that is also saved out to \code{settings$files$covariates} if indicated by \code{settings$save}.
#'  
#' @name prepare covariates
#'
#' @export
#'
prepare_covariates <- function (main     = ".", 
                                settings = directory_settings( ), 
                                quiet    = FALSE, 
                                verbose  = FALSE) {

  messageq("  - covariates", quiet = quiet)

  weather_data <- weather(level   = "daily", 
                          horizon = 1, 
                          path    = file.path(main, settings$subdirectories$resources))
  ndvi_data    <- ndvi(level      = "daily", 
                       path       = file.path(main, settings$subdirectories$resources))

  climate_forecasts <- read_climate_forecasts(main     = main,
                                              settings = settings)
  newmoons          <- read_newmoons(main              = main,
                                     settings          = settings)
  control_rodents   <- read_rodents_table(main         = main,
                                          dataset      = "controls", 
                                          settings     = settings) 

  historic_weather               <- data.frame(date = seq(settings$time$timeseries_start_lagged, settings$time$origin, 1))
  weather_rows                   <- match(historic_weather$date, weather_data$date)
  historic_weather$mintemp       <- weather_data$mintemp[weather_rows]
  historic_weather$maxtemp       <- weather_data$maxtemp[weather_rows]
  historic_weather$meantemp      <- weather_data$meantemp[weather_rows]
  historic_weather$precipitation <- weather_data$precipitation[weather_rows]
  historic_weather$source        <- "historic"             

  forecast_weather               <- data.frame(date = seq(settings$time$forecast_start, settings$time$forecast_end, 1))

  forecast_weather_rows          <- match(forecast_weather$date, climate_forecasts$date)
  forecast_weather$mintemp       <- climate_forecasts$mintemp[forecast_weather_rows]
  forecast_weather$maxtemp       <- climate_forecasts$maxtemp[forecast_weather_rows]
  forecast_weather$meantemp      <- climate_forecasts$meantemp[forecast_weather_rows]
  forecast_weather$precipitation <- climate_forecasts$precipitation[forecast_weather_rows]
  dayin                          <- forecast_weather$date %in% climate_forecasts$date
  forecast_weather$source[dayin] <- "nmme_forecast"

  weather_together <- rbind(historic_weather, forecast_weather)

  # fill in the missing values in the time series with a seasonal auto arima model
  # this accounts for needing a climate forecast a year out now, whereas we did not before
  # the models we get don't always go out that far, so we extend with a seasonal autoarima on each covariate

  in_fit         <- !is.na(weather_together$source)
  date_fit       <- weather_together$date[in_fit]
  foy_fit        <- foy(date_fit)
  cos_fit        <- cos(2 * pi * foy_fit)
  sin_fit        <- sin(2 * pi * foy_fit)
  xreg_fit       <- data.frame(cos_seas = cos_fit, sin_seas = sin_fit)
  xreg_fit       <- as.matrix(xreg_fit)

  mintemp_model  <- auto.arima(weather_together$mintemp[in_fit], xreg = xreg_fit)
  maxtemp_model  <- auto.arima(weather_together$maxtemp[in_fit], xreg = xreg_fit)
  meantemp_model <- auto.arima(weather_together$meantemp[in_fit], xreg = xreg_fit)
  precip_model   <- auto.arima(weather_together$precipitation[in_fit], xreg = xreg_fit)

  in_cast        <- is.na(weather_together$source)
  date_cast      <- weather_together$date[in_cast]
  foy_cast       <- foy(date_cast)
  cos_cast       <- cos(2 * pi * foy_cast)
  sin_cast       <- sin(2 * pi * foy_cast)
  xreg_cast      <- data.frame(cos_seas = cos_cast, sin_seas = sin_cast)
  xreg_cast      <- as.matrix(xreg_cast)

  mintemp_forecast  <- forecast(mintemp_model, xreg = xreg_cast)$mean
  maxtemp_forecast  <- forecast(maxtemp_model, xreg = xreg_cast)$mean
  meantemp_forecast <- forecast(meantemp_model, xreg = xreg_cast)$mean
  precip_forecast   <- pmax(0, forecast(precip_model, xreg = xreg_cast)$mean)
 
  weather_together$mintemp[in_cast]       <- mintemp_forecast
  weather_together$maxtemp[in_cast]       <- maxtemp_forecast
  weather_together$meantemp[in_cast]      <- meantemp_forecast
  weather_together$precipitation[in_cast] <- precip_forecast
  weather_together$source[in_cast]        <- "seasonal_autoarima_forecast"


  control_rodents$censusdate   <- as.Date(newmoons$censusdate[match(control_rodents$newmoonnumber, newmoons$newmoonnumber)])
  control_rodents$newmoondate  <- as.Date(newmoons$newmoondate[match(control_rodents$newmoonnumber, newmoons$newmoonnumber)]) 
  control_rodents$source       <- "historic"

  historic_ordii               <- control_rodents[ , c("newmoonnumber", "DO", "newmoondate", "source")]
  moonin                       <- newmoons$newmoondate > settings$time$origin
  forecast_ordii               <- data.frame(newmoonnumber = newmoons$newmoonnumber[moonin], 
                                             DO            = NA,
                                             newmoondate   = newmoons$newmoondate[moonin],
                                             source        = "psGARCH_forecast")

  past                         <- list(past_obs   = 1,
                                       past_mean  = 13)
  ordii_model                  <- tsglm(ts        = historic_ordii$DO, 
                                        model     = past, 
                                        distr     = "poisson", 
                                        link      = "log")
  forecast_ordii$DO            <- predict(object  = ordii_model, 
                                          n.ahead = nrow(forecast_ordii))$pred

  ordii_together               <- rbind(historic_ordii, 
                                        forecast_ordii)
  

  # ndvi is a bit of a challenge because we have multiple sources now

  monitor <- c("mu", "sensor_observation_sigma", "sensor_offset_sigma", "seasonal_cos_slope", "seasonal_sin_slope", "mu_year_offset_sigma", "seasonal_cos_year_offset_sigma", "seasonal_sin_year_offset_sigma",
               "mu_year_offset", "seasonal_cos_year_offset", "seasonal_sin_year_offset")

  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      list(.RNG.name                 = sample(rngs, 1),
           .RNG.seed                 = sample(1:1e+06, 1),
            mu                       = rnorm(1, data$mean_observed_value, 0.01),
            seasonal_cos_slope       = rnorm(1, 0, 0.1),
            seasonal_sin_slope       = rnorm(1, 0, 0.1),
            mu_year_offset_sigma        = runif(1, 0.001, 0.01),
            seasonal_sin_year_offset_sigma        = runif(1, 0.001, 0.01),
            seasonal_cos_year_offset_sigma        = runif(1, 0.001, 0.01),
            sensor_observation_sigma = runif(data$nsensors, 0.001, 0.01),
            sensor_offset_sigma      = runif(1, 0.001, 0.01))



    }

  }

  jags_model <- "model { 
 
    # priors

    mu                   ~  dnorm(mean_observed_value, 100)

    sensor_offset_sigma  ~  dunif(0, 1) 
    sensor_offset_tau    <- pow(sensor_offset_sigma, -2)

    seasonal_cos_slope   ~ dnorm(0, 1)
    seasonal_sin_slope   ~ dnorm(0, 1)

    seasonal_cos_year_offset_sigma  ~  dunif(0, 1) 
    seasonal_cos_year_offset_tau    <- pow(seasonal_cos_year_offset_sigma, -2)
    seasonal_sin_year_offset_sigma  ~  dunif(0, 1) 
    seasonal_sin_year_offset_tau    <- pow(seasonal_sin_year_offset_sigma, -2)

    mu_year_offset_sigma  ~  dunif(0, 1) 
    mu_year_offset_tau    <- pow(mu_year_offset_sigma, -2)

    for (i in 1:nsensors) {

      sensor_offset[i]            ~  dnorm(0, sensor_offset_tau)
      sensor_observation_sigma[i] ~  dunif(0, 1) 
      sensor_observation_tau[i]   <- pow(sensor_observation_sigma[i], -2)

    }

    for (i in 1:nyears) {

      mu_year_offset[i]            ~  dnorm(0, mu_year_offset_tau)
      seasonal_cos_year_offset[i]            ~  dnorm(0, seasonal_cos_year_offset_tau)
      seasonal_sin_year_offset[i]            ~  dnorm(0, seasonal_sin_year_offset_tau)

    }

    for (i in 1:npossible_dates) {
 
      latent_value[i]   <-  mu + mu_year_offset[date_year[i]] + 
                           (seasonal_cos_slope + seasonal_cos_year_offset[date_year[i]]) * seasonal_cos_value[i] + 
                           (seasonal_sin_slope + seasonal_sin_year_offset[date_year[i]]) * seasonal_sin_value[i]

    }



    for (i in 1:nobservations) {

      observed_value[i] ~ dnorm(latent_value[observation_date[i]] + sensor_offset[observation_sensor[i]], sensor_observation_tau[observation_sensor[i]])

    }



  }"


  ndvi_data$date      <- as.Date(ndvi_data$date)
  possible_dates      <- seq(min(ndvi_data$date), settings$time$forecast_end, 1)
  npossible_dates     <- length(possible_dates)
  seasonal_cos_value  <- cos(2 * pi * foy(possible_dates))
  seasonal_sin_value  <- sin(2 * pi * foy(possible_dates))
  date_year           <- as.numeric(as.factor(format(possible_dates, "%Y")))
  nyears              <- max(date_year)

  observed_value      <- ndvi_data$ndvi[ndvi_data$date %in% possible_dates]
  mean_observed_value <- mean(observed_value, na.rm = TRUE)
  nobservations       <- nrow(ndvi_data[ndvi_data$date %in% possible_dates, ])
  observation_sensor  <- as.numeric(as.factor(ndvi_data$sensor[ndvi_data$date %in% possible_dates]))
  nsensors            <- length(unique(observation_sensor))
  observation_date    <- match(as.Date(ndvi_data$date[ndvi_data$date %in% possible_dates]), possible_dates)
  possible_sensors    <- levels(as.factor(ndvi_data$sensor[ndvi_data$date %in% possible_dates]))
  
  data <- list(observed_value      = observed_value,
               mean_observed_value = mean_observed_value,
               seasonal_cos_value  = seasonal_cos_value,
               seasonal_sin_value  = seasonal_sin_value,
               nobservations       = nobservations,
               npossible_dates     = npossible_dates,
               observation_date    = observation_date,
               nsensors            = nsensors,
               observation_sensor  = observation_sensor,
               nyears              = nyears,
               date_year           = date_year)

  model_fit <- run.jags(model = jags_model, 
                   monitor   = monitor, 
                   inits     = inits(data), 
                   data      = data, 
                   n.chains  = 4,
                   adapt     = 1000,
                   burnin    = 1000,
                   sample    = 1000,
                   thin      = 1,
                   modules   = "glm",
                   method    = "interruptible", 
                   factories = "", 
                   mutate    = NA, 
                   summarise = TRUE, 
                   plots     = FALSE)

  mcmc <- combine.mcmc(model_fit$mcmc)
  model_fit_summary <- summary(model_fit)

  y <-  model_fit_summary["mu", "Mean"] + model_fit_summary[paste0("mu_year_offset[", date_year, "]"), "Mean"] + 
       (model_fit_summary["seasonal_cos_slope", "Mean"] + model_fit_summary[paste0("seasonal_cos_year_offset[", date_year, "]"), "Mean"]) * seasonal_cos_value + 
       (model_fit_summary["seasonal_sin_slope", "Mean"] + model_fit_summary[paste0("seasonal_sin_year_offset[", date_year, "]"), "Mean"]) * seasonal_sin_value
  names(y) <- NULL

  covariates_together <- data.frame(newmoon       = ordii_together$newmoonnumber,
                                    newmoondate   = ordii_together$newmoondate,
                                    ordii         = ordii_together$DO,
                                    ndvi          = round(y[match(ordii_together$newmoondate,  possible_dates)], 3),
                                    mintemp       = NA,
                                    meantemp      = NA,
                                    maxtemp       = NA,
                                    precipitation = NA,
                                    warm_precip   = NA)

  newmoon_number <- newmoons$newmoonnumber[-1]
  newmoon_start <- as.Date(newmoons$newmoondate[-nrow(newmoons)])
  newmoon_end <- as.Date(newmoons$newmoondate[-1])
  newmoon_match_number <- NULL
  newmoon_match_date <- NULL
  for (i in seq(newmoon_number)) {
    temp_dates <- as.character(seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1))
    temp_numbers <- rep(newmoon_number[i], length(temp_dates))
    newmoon_match_date <- c(newmoon_match_date, temp_dates)
    newmoon_match_number <- c(newmoon_match_number, temp_numbers)
  }
  newmoon_match_date <- as.Date(newmoon_match_date)
  
  for (i in 1:nrow(covariates_together)) {
    rowsin <- weather_together$date %in% newmoon_match_date[newmoon_match_number == covariates_together$newmoon[i]]
    covariates_together$mintemp[i]       <- round(mean(weather_together$mintemp[rowsin], na.rm = TRUE), 3)
    covariates_together$meantemp[i]      <- round(mean(weather_together$meantemp[rowsin], na.rm = TRUE), 3)
    covariates_together$maxtemp[i]       <- round(mean(weather_together$maxtemp[rowsin], na.rm = TRUE), 3)
    covariates_together$precipitation[i] <- round(sum(weather_together$precipitation[rowsin], na.rm = TRUE), 3)
    covariates_together$warm_precip[i]   <- round(sum(weather_together$precipitation[rowsin] * weather_together$mintemp[rowsin] >= 4, na.rm = TRUE), 3)

  }

  covariates_together$cos2pifoy <- round(cos(2 * pi * foy(covariates_together$newmoondate)), 3)
  covariates_together$sin2pifoy <- round(sin(2 * pi * foy(covariates_together$newmoondate)), 3)
  covariates_together  

  write_data(x         = covariates_together, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$covariates, 
             quiet     = !verbose)

}

