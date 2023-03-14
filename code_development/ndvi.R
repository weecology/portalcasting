



#  monitor <- c("mu", "latent_sigma", "observation_sigma", "sensor_offset_sigma", "seasonal_cos_slope", "seasonal_sin_slope")

  monitor <- c("mu", "latent_sigma", "observation_sigma")


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

#      list(.RNG.name            = sample(rngs, 1),
#           .RNG.seed            = sample(1:1e+06, 1),
#            mu                  = rnorm(1, data$mean_observed_value, 0.01),
#            seasonal_cos_slope  = rnorm(1, 0, 0.005),
#            seasonal_sin_slope  = rnorm(1, 0, 0.005),
#            latent_sigma        = runif(1, 0.001, 0.01),
#            observation_sigma   = runif(1, 0.001, 0.01),
#            sensor_offset_sigma = runif(1, 0.001, 0.01))

      list(.RNG.name            = sample(rngs, 1),
           .RNG.seed            = sample(1:1e+06, 1),
            mu                  = rnorm(1, data$mean_observed_value, 0.01),
            latent_sigma        = runif(1, 0.001, 0.01),
            observation_sigma   = runif(1, 0.001, 0.01))


    }

  }

  jags_model <- "model { 
 
    # priors

    mu                   ~  dnorm(mean_observed_value, 100)

    latent_sigma         ~  dunif(0, 1) 
    latent_tau           <- pow(latent_sigma, -2)

#    seasonal_cos_slope   ~ dnorm(0, 1000)
#    seasonal_sin_slope   ~ dnorm(0, 1000)

    observation_sigma    ~  dunif(0, 1) 
    observation_tau      <- pow(observation_sigma, -2)

#    sensor_offset_sigma  ~  dunif(0, 1) 
#    sensor_offset_tau    <- pow(sensor_offset_sigma, -2)


#    for (i in 1:nsensors) {
#
#      sensor_offset[i] ~ dnorm(0, sensor_offset_tau)
#
#    }


#    latent_value[1] ~ dnorm(mu + seasonal_cos_slope * seasonal_cos_value[1] + seasonal_sin_slope * seasonal_sin_value[1], latent_tau)
#    for (i in 2:npossible_dates) {
# 
#      latent_value[i]   ~ dnorm(latent_value[i - 1] + seasonal_cos_slope * seasonal_cos_value[i] + seasonal_sin_slope * seasonal_sin_value[i], latent_tau)
#
#    }

    for (i in 1:npossible_dates) {

      latent_value[i]   ~ dnorm(mu, latent_tau)

    }


#    for (i in 1:nobservations) {
#
#      observed_value[i] ~ dnorm(latent_value[observation_date[i]] + sensor_offset[observation_sensor[i]], observation_tau)
#
#    }

    for (i in 1:nobservations) {

      observed_value[i] ~ dnorm(latent_value[observation_date[i]], observation_tau)

    }

  }"



  possible_dates      <- seq(as.Date(min(ndvi_data$date)), as.Date(max(ndvi_data$date)), 1)
  npossible_dates     <- length(possible_dates)
  seasonal_cos_value  <- cos(2 * pi * foy(possible_dates))
  seasonal_sin_value  <- sin(2 * pi * foy(possible_dates))

  observed_value      <- ndvi_data$ndvi
  mean_observed_value <- mean(observed_value, na.rm = TRUE)
  nobservations       <- nrow(ndvi_data)
  observation_sensor  <- as.numeric(as.factor(ndvi_data$sensor))
  nsensors            <- length(unique(observation_sensor))
  observation_date    <- match(as.Date(ndvi_data$date), possible_dates)
  possible_sensors    <- levels(as.factor(ndvi_data$sensor))


#  data <- list(observed_value      = observed_value,
#               mean_observed_value = mean_observed_value,
#               seasonal_cos_value  = seasonal_cos_value,
#               seasonal_sin_value  = seasonal_sin_value,
#               nobservations       = nobservations,
#               npossible_dates     = npossible_dates,
#               observation_date    = observation_date,
#               nsensors            = nsensors,
#               observation_sensor  = observation_sensor)

  data <- list(observed_value      = observed_value,
               mean_observed_value = mean_observed_value,
               nobservations       = nobservations,
               npossible_dates     = npossible_dates,
               observation_date    = observation_date)


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


model_fit