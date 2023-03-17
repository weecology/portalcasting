
  control_runjags <- runjags_control(nchains = 2, thin = 1, sample = 100, burnin = 100, adapt = 100)

  monitor <- c("mu", "sigma", paste0("X[", metadata$time$forecast_newmoons, "]"))

  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")
 
    function (chain = chain) {

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = rnorm(1, mean = log_mean_count, sd = 0.1),  
            sigma    = runif(1, min = 0.01, max = 0.5))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu    ~  dnorm(log_mean_count, 5)
    sigma ~  dunif(0, 1) 
    tau   <- pow(sigma, -2)

    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 


    # through time

    for(i in 2:N) {

      # Process model

      pred_log_X[i] <- log_X[i-1]
      log_X[i]      ~  dnorm(pred_log_X[i], tau)
      X[i]          <- exp(log_X[i])
   

      # observation model

      count[i] ~ dpois(X[i])

    }


  }"

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)


  true_count_lead <- metadata$time$rodent_lead_time
  cast_count      <- rep(NA, metadata$time$lead_time_newmoons)
  count           <- c(abundance, cast_count)
  log_mean_count  <- log(mean(abundance, na.rm = TRUE))

  data <- list(count          = count, 
               N              = length(count),
               log_mean_count = log_mean_count)

  model_fit <- run.jags(model     = jags_model, 
                   monitor   = monitor, 
                   inits     = inits(data), 
                   data      = data, 
                   n.chains  = control_runjags$nchains, 
                   adapt     = control_runjags$adapt, 
                   burnin    = control_runjags$burnin, 
                   sample    = control_runjags$sample, 
                   thin      = control_runjags$thin, 
                   modules   = control_runjags$modules, 
                   method    = control_runjags$method, 
                   factories = control_runjags$factories, 
                   mutate    = control_runjags$mutate, 
                   summarise = FALSE, 
                   plots     = FALSE)

  nchains <- control_runjags$nchains
  vals    <- model_fit$mcmc[[1]]

  if (nchains > 1) {

    for (j in 2:nchains) {

      vals <- rbind(vals, model_fit$mcmc[[j]])

    }

  }

  pred_cols       <- grep("X", colnames(vals))
  vals            <- vals[ , pred_cols]
  HPD             <- HPDinterval(obj = as.mcmc(vals), prob = metadata$confidence_level)
  model_cast      <- data.frame(pred  = round(apply(vals, 2, mean), 3),
                                lower = round(HPD[ , "lower"], 3), 
                                upper = round(HPD[ , "upper"], 3))
  rownames(model_cast) <- NULL

  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 

