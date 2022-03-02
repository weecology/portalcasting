# working here! getting a sim up and running to verify!
#  looking good so far. next thing is to get the K parameter off of the integer poisson
#

    r      <- 0.6
    K      <- 40
    nmoons <- 335
    seed   <- 1312

    set.seed(seed)


    log_X0_mean      <- log(10)
    log_X0_precision <- 4.5
    X0               <- exp(rnorm(1, log_X0_mean, sqrt(1 / log_X0_precision)))
    Y0               <- rpois(1, X0)


    X      <- rep(NA, nmoons)
    Y      <- rep(NA, nmoons)

    X[1]   <- X0 * exp(r * (1 - (X0 / K)))
    Y[1]   <- rpois(1, X[1])

    for (i in 2:nmoons) {

      X[i] <- X[i-1] * exp(r * (1 - (X[i - 1] / K)))

      Y[i] <- rpois(1, X[i])

    }


    plot(X, type = "l", ylim = c(0, max(c(X, Y))))

    points(Y)


    bonus_max_count <- max(Y) * 1.2
    log_bonus_max_count <- log(bonus_max_count)

  data <- list(Y                = Y, 
               Y0               = Y0,
               nmoons           = nmoons,
               log_bonus_max_count  = log_bonus_max_count,
               log_X0_mean      = log_X0_mean,
               log_X0_precision = log_X0_precision)


  jags_model <- "model {

    r  ~ dnorm(0, 10)

    log_K   ~ dnorm(log_bonus_max_count, 1 / log_bonus_max_count)
    K       <- exp(log_K)

    log_X0  ~ dnorm(log_X0_mean, log_X0_precision)
    X0      <- exp(log_X0)
    Y0      ~ dpois(X0)
  
    X[1]   <- max(c(X0 * exp(r * (1 - (X0 / K))), 0.000001))
    Y[1]   ~  dpois(X[1])

    for (i in 2:nmoons) {

      X[i] <- max(c(X[i-1] * exp(r * (1 - (X[i - 1] / K))), 0.000001))

      Y[i] ~  dpois(X[i])

    }

   }"


  inits <- function (data = NULL) {

    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")


    function (chain = chain) {

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
           log_X0    = log(rnorm(1, data$log_X0_mean, sqrt(1 / data$log_X0_precision))),
           r         = rnorm(1, 0, 1),
           log_K     = log(rnorm(1, data$log_bonus_max_count, 0.1 * sqrt( data$log_bonus_max_count) )))

    }

  }

  monitor <- c("r", "K", "X0")

  model_fit <- run.jags(model     = jags_model, 
                        monitor   = monitor, 
                        inits     = inits(data),
                        data      = data, 
                        n.chains  = 3,
                        adapt     = 1000,
                        burnin    = 1000,
                        sample    = 1000,
                        thin      = 1,
                        modules   = "glm", 
                        method    = "parallel")

  round(summary(model_fit), 4)
  plot(model_fit)
