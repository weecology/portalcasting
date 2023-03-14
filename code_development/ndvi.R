



  monitor <- c("mu", "sigma")
  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      list(.RNG.name    = sample(rngs, 1),
           .RNG.seed    = sample(1:1e+06, 1),
            mu          = rnorm(1, 0, 1),
            sigma       = runif(1, 0.01, 1))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu          ~  dnorm(0, 1)
    sigma       ~  dunif(0, 1) 
    tau         <- pow(sigma, -2)

    # through time

    for(i in 1:N) {

      x[i] ~ dnorm(mu, tau)
      y[i] ~ dnorm(x[i], 1)

    }

  }"


  data <- list(y              = ndvi_data$ndvi,
               N              = nrow(ndvi_data))

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