s <- mgcv::s
# one at a time?
m <- mgcv::gam(ndvi ~ s(foy_fit * sensor, bs = "cc"))
i <- 5
y <- predict(m, newdata = data.frame(foy_fit = foy_cast, sensor = sensor_levels[i]))
sensor_index_col <- sensor_index
sensor_index_col[sensor_index_col != i] <- 0
plot(ndvi ~ ndvi_data$date, col = sensor_index_col, ylim = c(0, 0.5))
points(y ~ dates_cast, type = 'l', lwd = 3)





  possible_sensors    <- unique(ndvi_data$sensor)
  npossible_sensors   <- length(possible_sensors)
model_fit <- named_null_list(possible_sensors)
for (j in 1:npossible_sensors) {
  monitor <- c("mu", "observation_sigma", "seasonal_cos_slope", "seasonal_sin_slope")


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      list(.RNG.name            = sample(rngs, 1),
           .RNG.seed            = sample(1:1e+06, 1),
            mu                  = rnorm(1, data$mean_observed_value, 0.01),
            seasonal_cos_slope  = rnorm(1, 0, 0.005),
            seasonal_sin_slope  = rnorm(1, 0, 0.005),
            observation_sigma   = runif(1, 0.001, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu                   ~  dnorm(mean_observed_value, 100)

    seasonal_cos_slope   ~ dnorm(0, 100)
    seasonal_sin_slope   ~ dnorm(0, 100)

    observation_sigma    ~  dunif(0, 0.1) 
    observation_tau      <- pow(observation_sigma, -2)


    latent_value[1] <- mu + seasonal_cos_slope * seasonal_cos_value[1] + seasonal_sin_slope * seasonal_sin_value[1]
    for (i in 2:npossible_dates) {
 
      latent_value[i]   <- latent_value[i - 1] + seasonal_cos_slope * seasonal_cos_value[i] + seasonal_sin_slope * seasonal_sin_value[i]

    }


    for (i in 1:nobservations) {

      observed_value[i] ~ dnorm(latent_value[observation_date[i]], observation_tau)

    }

  }"
 
  ndvi_data_sensor    <- ndvi_data[ndvi_data$sensor == possible_sensors[j], ]

  possible_dates      <- seq(as.Date(min(ndvi_data_sensor$date)), as.Date(max(ndvi_data_sensor$date)), 1)
  npossible_dates     <- length(possible_dates)
  seasonal_cos_value  <- cos(2 * pi * foy(possible_dates))
  seasonal_sin_value  <- sin(2 * pi * foy(possible_dates))

  observed_value      <- ndvi_data_sensor$ndvi
  mean_observed_value <- mean(observed_value, na.rm = TRUE)
  nobservations       <- nrow(ndvi_data_sensor)
  observation_date    <- match(as.Date(ndvi_data_sensor$date), possible_dates)


  data <- list(observed_value      = observed_value,
               mean_observed_value = mean_observed_value,
               seasonal_cos_value  = seasonal_cos_value,
               seasonal_sin_value  = seasonal_sin_value,
               nobservations       = nobservations,
               npossible_dates     = npossible_dates,
               observation_date    = observation_date)


  model_fit[[j]] <- run.jags(model = jags_model, 
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
}


mapply(summary, model_fit, vars = "mu")
