
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



  possible_dates      <- seq(as.Date(min(ndvi_data$date)), forecast_end, 1)
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
plot(ndvi_data$date, ndvi_data$ndvi, col = as.numeric(observation_sensor), ylim = c(-0.1, 0.5), xlim = as.Date(c("1990-01-01", "2023-05-05")))
  points(possible_dates, y, type = 'l', lwd = 3, col =1)

nits <- 100

ii <- sample(1:nrow(mcmc), nits)
for (i in 1:nits){
y <- mcmc[ii[i], "mu"] + mcmc[ii[i], paste0("mu_year_offset[", date_year, "]")] + 
     (mcmc[ii[i], "seasonal_cos_slope"] + mcmc[ii[i], paste0("seasonal_cos_year_offset[", date_year, "]")]) * seasonal_cos_value + 
     (mcmc[ii[i], "seasonal_sin_slope"] + mcmc[ii[i], paste0("seasonal_sin_year_offset[", date_year, "]")]) * seasonal_sin_value
  points(possible_dates, y, type = 'l', lwd = 1, col = grey(0.5, 0.5))
}

ii <- sample(1:nrow(mcmc), nits)
Y <- matrix(NA, nrow = nits, ncol = length(seasonal_cos_value))
for (i in 1:nits){
Y[i,] <- mcmc[ii[i], "mu"] + 
     (mcmc[ii[i], "seasonal_cos_slope"] + mcmc[ii[i], paste0("seasonal_cos_year_offset[", date_year, "]")]) * seasonal_cos_value + 
     (mcmc[ii[i], "seasonal_sin_slope"] + mcmc[ii[i], paste0("seasonal_sin_year_offset[", date_year, "]")]) * seasonal_sin_value
  points(possible_dates, y, type = 'l', lwd = 1, col = grey(0.5, 0.5))
}


points(ndvi_data$date, ndvi_data$ndvi, col = as.numeric(observation_sensor), ylim = c(-0.1, 0.5), xlim = as.Date(c("1990-01-01", "2023-05-05")))
  points(possible_dates, y, type = 'l', lwd = 3, col =1)
  points(possible_dates, apply(Y, 2, median), type = 'l', lwd = 3, col = 4)



yt <- model_fit_summary["mu", "Median"] + 
     (model_fit_summary["seasonal_cos_slope", "Median"] + model_fit_summary[paste0("seasonal_cos_year_offset[", date_year, "]"), "Median"]) * seasonal_cos_value + 
     (model_fit_summary["seasonal_sin_slope", "Median"] + model_fit_summary[paste0("seasonal_sin_year_offset[", date_year, "]"), "Median"]) * seasonal_sin_value



