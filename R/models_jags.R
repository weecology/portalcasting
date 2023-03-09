#' @rdname prefab_model_functions
#'
#' @export
#'
jags_logistic_competition <- function (main            = ".", 
                                       dataset         = NULL,
                                       species         = NULL,
                                       settings        = directory_settings( ), 
                                       control_runjags = runjags_control( ), 
                                       quiet           = FALSE, 
                                       verbose         = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  dataset     <- tolower(dataset)
  model       <- "jags_logistic_competition"


  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)


  abundance <- prepare_rodent_abundance(main     = main,
                                        dataset  = dataset,
                                        species  = species,
                                        model    = model,
                                        settings = settings,
                                        quiet    = quiet,
                                        verbose  = verbose)



  monitor <- c("mu", "sigma", "r_int", "log_K_int", "log_K_slope", paste0("X[", metadata$time$rodent_cast_moons - metadata$time$start_moon, "]"))

  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      mu        <- rnorm(1, data$log_mean_count, 0.1)
      log_K_int <- rnorm(1, data$log_max_count, 0.01)

      log_K_int <- max(c(mu, log_K_int)) + 0.1

      list(.RNG.name    = sample(rngs, 1),
           .RNG.seed    = sample(1:1e+06, 1),
            mu          = mu, 
            sigma       = runif(1, 0.00001, 0.005),
            r_int       = rnorm(1, 0, 0.01),
            log_K_int   = log_K_int,
            log_K_slope = rnorm(1, 0, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu          ~  dnorm(log_mean_count, 5)
    sigma       ~  dunif(0, 1) 
    tau         <- pow(sigma, -2)
    r_int       ~  dnorm(0, 5)
    log_K_int   ~  dnorm(log_max_count, 5)
    log_K_slope ~  dnorm(0, 1)
 
    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 

    # exampand parameters

    for (i in 1:N) {

      r[i] <- r_int
      K[i] <- exp(log_K_int + log_K_slope * ordii[i]) 

    }


    # through time

    for(i in 2:N) {

      # Process model

      pred_X[i]     <- X[i-1] * exp(r[i] * (1 - (X[i - 1] / K[i])))
      pred_log_X[i] <- log(pred_X[i])
      log_X[i]      ~  dnorm(pred_log_X[i], tau)
      X[i]          <- exp(log_X[i])

      # observation model

      count[i] ~ dpois(X[i])

    }

  }"


  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)


  covariates    <- read_covariates(main     = main,
                                   settings = settings)

    true_count_lead   <- length(metadata$time$rodent_cast_moons)

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(abundance, cast_count)
   
    log_mean_count <- log(mean(abundance, na.rm = TRUE))
    log_max_count  <- log(max(abundance, na.rm = TRUE))

    ordii_controls <- scale(covariates$ordii_controls[covariates$newmoonnumber >= metadata$time$start_moon &
                                                      covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])

    data <- list(count          = count, 
                 N              = length(count),
                 log_mean_count = log_mean_count,
                 log_max_count  = log_max_count,
                 ordii          = ordii_controls[ , 1])



    mods <- run.jags(model     = jags_model, 
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
      vals    <- mods$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods$mcmc[[j]])

        }

      }

      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      HPD            <- HPDinterval(as.mcmc(vals))
      casts        <- data.frame(pred = round(apply(vals, 2, mean), 3),
                                 lower       = round(HPD[ , "lower"], 3), 
                                 upper       = round(HPD[ , "upper"], 3),
                                 moon           = metadata$time$rodent_cast_moons)

      rownames(casts)      <- NULL

      cast_tab <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = metadata$dataset_controls[[dataset]]$args$output,
                               model            = model, 
                               dataset          = dataset, 
                               species          = species, 
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               estimate         = casts$pred, 
                               lower_pi         = casts$lower,
                               upper_pi         = casts$upper, 
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)


  metadata <- update_list(metadata,
                          models           = model,
                          datasets         = dataset,
                          dataset_controls = metadata$dataset_controls[[dataset]])

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts) 
 

}


#' @rdname prefab_model_functions
#'
#' @export
#'
jags_logistic_competition_covariates <- function (main            = ".", 
                                                  dataset         = NULL,
                                                  species         = NULL,
                                                  settings        = directory_settings(), 
                                                  control_runjags = runjags_control(), 
                                                  quiet           = FALSE, 
                                                  verbose         = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  dataset     <- tolower(dataset)
  model       <- "jags_logistic_competition_covariates"


  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)


  abundance <- prepare_rodent_abundance(main     = main,
                                        dataset  = dataset,
                                        species  = species,
                                        model    = model,
                                        settings = settings,
                                        quiet    = quiet,
                                        verbose  = verbose)


  monitor <- c("mu", "sigma", "r_int", "r_slope", "log_K_int", "log_K_slope_NDVI", "log_K_slope_DO", paste0("X[", metadata$time$rodent_cast_moons - metadata$time$start_moon, "]"))


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      mu        <- rnorm(1, log_mean_count, 0.1)
      log_K_int <- rnorm(1, log_max_count, 0.01)

      log_K_int <- max(c(mu, log_K_int)) + 0.1

      list(.RNG.name         = sample(rngs, 1),
           .RNG.seed         = sample(1:1e+06, 1),
            mu               = mu, 
            sigma            = runif(1, 0.01, 0.5),
            r_int            = rnorm(1, 0, 0.01),
            r_slope          = rnorm(1, 0, 0.1),
            log_K_int        = log_K_int,
            log_K_slope_NDVI = rnorm(1, 0, 0.01),
            log_K_slope_DO   = rnorm(1, 0, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu               ~  dnorm(log_mean_count, 5)
    sigma            ~  dunif(0, 1) 
    tau              <- pow(sigma, -1/2)
    r_int            ~  dnorm(0, 5)
    r_slope          ~  dnorm(0, 1)
    log_K_int        ~  dnorm(log_max_count, 5)
    log_K_slope_NDVI ~  dnorm(0, 4)
    log_K_slope_DO   ~  dnorm(0, 4)
 
    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 

    # exampand parameters

    for (i in 1:N) {

      r[i] <- r_int + r_slope * warm_rain_three_newmoons[i]
      K[i] <- exp(log_K_int + log_K_slope_DO * ordii[i] + log_K_slope_NDVI * ndvi_thirteen_newmoons[i]) 

    }


    # through time

    for(i in 2:N) {

      # Process model

      pred_X[i]     <- X[i-1] * exp(r[i] * (1 - (X[i - 1] / K[i])))
      pred_log_X[i] <- log(pred_X[i])
      log_X[i]      ~  dnorm(pred_log_X[i], tau)
      X[i]          <- exp(log_X[i])

      # observation model

      count[i] ~ dpois(X[i])

    }

  }"

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  covariates    <- read_covariates(main     = main,
                                   settings = settings)



    true_count_lead   <- length(metadata$time$rodent_cast_moons)

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(abundance, cast_count)
   
    log_mean_count <- log(mean(abundance, na.rm = TRUE))
    log_max_count  <- log(max(abundance, na.rm = TRUE))

    warm_rain_three_newmoons <- scale(covariates$warm_precip_3_moon[covariates$newmoonnumber >= metadata$time$start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])
    ndvi_thirteen_newmoons     <- scale(covariates$ndvi_13_moon[covariates$newmoonnumber >= metadata$time$start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])
    ordii_controls <- scale(covariates$ordii_controls[covariates$newmoonnumber >= metadata$time$start_moon &
                                                      covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])

    data <- list(count          = count, 
                 N              = length(count),
                 log_mean_count = log_mean_count,
                 log_max_count  = log_max_count,
                 warm_rain_three_newmoons = warm_rain_three_newmoons[ , 1],
                 ndvi_thirteen_newmoons     = ndvi_thirteen_newmoons[ , 1],
                 ordii          = ordii_controls[ , 1])


    mods <- run.jags(model     = jags_model, 
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
      vals    <- mods$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods$mcmc[[j]])

        }

      }

      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      HPD            <- HPDinterval(as.mcmc(vals))
      casts        <- data.frame(pred = round(apply(vals, 2, mean), 3),
                                 lower       = round(HPD[ , "lower"], 3), 
                                 upper       = round(HPD[ , "upper"], 3),
                                 moon           = metadata$time$rodent_cast_moons)

      rownames(casts)      <- NULL

      cast_tab <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = metadata$dataset_controls[[dataset]]$args$output,
                               model            = model, 
                               dataset          = dataset, 
                               species          = species, 
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               estimate         = casts$pred, 
                               lower_pi         = casts$lower,
                               upper_pi         = casts$upper, 
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)


  metadata <- update_list(metadata,
                          models           = model,
                          datasets         = dataset,
                          dataset_controls = metadata$dataset_controls[[dataset]])

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts) 
 

}

#' @rdname prefab_model_functions
#'
#' @export
#'
jags_logistic_covariates <- function (main            = ".", 
                                      dataset         = NULL,
                                      species         = NULL,
                                      settings        = directory_settings(), 
                                      control_runjags = runjags_control(), 
                                      quiet           = FALSE, 
                                      verbose         = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  dataset     <- tolower(dataset)
  model       <- "jags_logistic_covariates"


  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)


  abundance <- prepare_rodent_abundance(main     = main,
                                        dataset  = dataset,
                                        species  = species,
                                        model    = model,
                                        settings = settings,
                                        quiet    = quiet,
                                        verbose  = verbose)


  monitor <- c("mu", "sigma", "r_int", "r_slope", "log_K_int", "log_K_slope_NDVI", paste0("X[", metadata$time$rodent_cast_moons - metadata$time$start_moon, "]"))



  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      mu        <- rnorm(1, log_mean_count, 0.1)
      log_K_int <- rnorm(1, log_max_count, 0.01)

      log_K_int <- max(c(mu, log_K_int)) + 0.1

      list(.RNG.name    = sample(rngs, 1),
           .RNG.seed    = sample(1:1e+06, 1),
            mu          = mu, 
            sigma       = runif(1, 0.01, 0.5),
            r_int       = rnorm(1, 0, 0.01),
            r_slope     = rnorm(1, 0, 0.1),
            log_K_int   = log_K_int,
            log_K_slope = rnorm(1, 0, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu          ~  dnorm(log_mean_count, 5)
    sigma       ~  dunif(0, 1) 
    tau         <- pow(sigma, -2)
    r_int       ~  dnorm(0, 5)
    r_slope     ~  dnorm(0, 1)
    log_K_int   ~  dnorm(log_max_count, 5)
    log_K_slope ~  dnorm(0, 1)
 
    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 

    # exampand parameters

    for (i in 1:N) {

      r[i] <- r_int + r_slope * warm_rain_three_newmoons[i]
      K[i] <- exp(log_K_int + log_K_slope * ndvi_thirteen_newmoons[i]) 

    }


    # through time

    for(i in 2:N) {

      # Process model

      pred_X[i]     <- X[i-1] * exp(r[i] * (1 - (X[i - 1] / K[i])))
      pred_log_X[i] <- log(pred_X[i])
      log_X[i]      ~  dnorm(pred_log_X[i], tau)
      X[i]          <- exp(log_X[i])

      # observation model

      count[i] ~ dpois(X[i])

    }

  }"



  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  covariates    <- read_covariates(main     = main,
                                   settings = settings)



    true_count_lead   <- length(metadata$time$rodent_cast_moons)

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(abundance, cast_count)
   
    log_mean_count <- log(mean(abundance, na.rm = TRUE))
    log_max_count  <- log(max(abundance, na.rm = TRUE))

    warm_rain_three_newmoons <- scale(covariates$warm_precip_3_moon[covariates$newmoonnumber >= metadata$time$start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])
    ndvi_thirteen_newmoons     <- scale(covariates$ndvi_13_moon[covariates$newmoonnumber >= metadata$time$start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])


    data <- list(count                  = count, 
                 N                      = length(count),
                 log_mean_count    = log_mean_count,
                 log_max_count     = log_max_count,
                 warm_rain_three_newmoons = warm_rain_three_newmoons[ , 1],
                 ndvi_thirteen_newmoons     = ndvi_thirteen_newmoons[ , 1])


    mods <- run.jags(model     = jags_model, 
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
      vals    <- mods$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods$mcmc[[j]])

        }

      }


      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      HPD            <- HPDinterval(as.mcmc(vals))
      casts        <- data.frame(pred = round(apply(vals, 2, mean), 3),
                                 lower       = round(HPD[ , "lower"], 3), 
                                 upper       = round(HPD[ , "upper"], 3),
                                 moon           = metadata$time$rodent_cast_moons)

      rownames(casts)      <- NULL

      cast_tab <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = metadata$dataset_controls[[dataset]]$args$output,
                               model            = model, 
                               dataset          = dataset, 
                               species          = species, 
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               estimate         = casts$pred, 
                               lower_pi         = casts$lower,
                               upper_pi         = casts$upper, 
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)


  metadata <- update_list(metadata,
                          models           = model,
                          datasets         = dataset,
                          dataset_controls = metadata$dataset_controls[[dataset]])

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts) 

}


#' @rdname prefab_model_functions
#'
#' @export
#'
jags_logistic <- function (main            = ".", 
                           dataset         = NULL,
                           species         = NULL,
                           settings        = directory_settings(), 
                           control_runjags = runjags_control(), 
                           quiet           = FALSE, 
                           verbose         = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  dataset     <- tolower(dataset)
  model       <- "jags_logistic"


  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)


  abundance <- prepare_rodent_abundance(main     = main,
                                        dataset  = dataset,
                                        species  = species,
                                        model    = model,
                                        settings = settings,
                                        quiet    = quiet,
                                        verbose  = verbose)


  monitor <- c("mu", "sigma", "r", "log_K", paste0("X[", metadata$time$rodent_cast_moons - metadata$time$start_moon, "]"))


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    function (chain = chain) {

      mu    <- rnorm(1, log_mean_count, 0.1)
      log_K <- rnorm(1, log_max_count, 0.01)

      log_K <- max(c(mu, log_K)) + 0.01

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = mu, 
            sigma    = runif(1, 0.01, 0.5),
            r        = rnorm(1, 0, 0.01),
            log_K    = log_K)

    }

  }

  jags_model <- "model { 
 
    # priors

    mu    ~  dnorm(log_mean_count, 5)
    sigma ~  dunif(0, 1) 
    tau   <- pow(sigma, -2)
    r     ~  dnorm(0, 5)
    log_K ~  dnorm(log_max_count, 5)
    K     <- exp(log_K) 
 
    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 


    # through time

    for(i in 2:N) {

      # Process model

      pred_X[i]     <- X[i-1] * exp(r * (1 - (X[i - 1] / K)))
      pred_log_X[i] <- log(pred_X[i])
      log_X[i]      ~  dnorm(pred_log_X[i], tau)
      X[i]          <- exp(log_X[i])

      # observation model

      count[i] ~ dpois(X[i])

    }

  }"


  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)


    true_count_lead   <- length(metadata$time$rodent_cast_moons)

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(abundance, cast_count)
   
    log_mean_count <- log(mean(abundance, na.rm = TRUE))
    log_max_count  <- log(max(abundance, na.rm = TRUE))


    data <- list(count                  = count, 
                 N                      = length(count),
                 log_mean_count    = log_mean_count,
                 log_max_count     = log_max_count)


    mods <- run.jags(model     = jags_model, 
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
      vals    <- mods$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods$mcmc[[j]])

        }

      }


      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      HPD            <- HPDinterval(as.mcmc(vals))
      casts        <- data.frame(pred = round(apply(vals, 2, mean), 3),
                                 lower       = round(HPD[ , "lower"], 3), 
                                 upper       = round(HPD[ , "upper"], 3),
                                 moon           = metadata$time$rodent_cast_moons)

      rownames(casts)      <- NULL

      cast_tab <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = metadata$dataset_controls[[dataset]]$args$output,
                               model            = model, 
                               dataset          = dataset, 
                               species          = species, 
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               estimate         = casts$pred, 
                               lower_pi         = casts$lower,
                               upper_pi         = casts$upper, 
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)


  metadata <- update_list(metadata,
                          models           = model,
                          datasets         = dataset,
                          dataset_controls = metadata$dataset_controls[[dataset]])

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts) 

}



#' @rdname prefab_model_functions
#'
#' @export
#'
jags_RW <- function (main            = ".", 
                     dataset         = NULL,
                     species         = NULL,
                     settings        = directory_settings(),
                     control_runjags = runjags_control(), 
                     quiet           = FALSE, 
                     verbose         = FALSE){
 
  return_if_null(x = dataset)
  return_if_null(x = species)
  dataset     <- tolower(dataset)
  model       <- "jags_RW"


  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)


  abundance <- prepare_rodent_abundance(main     = main,
                                        dataset  = dataset,
                                        species  = species,
                                        model    = model,
                                        settings = settings,
                                        quiet    = quiet,
                                        verbose  = verbose)



  monitor <- c("mu", "sigma", paste0("X[", metadata$time$rodent_cast_moons - metadata$time$start_moon, "]"))

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

  model     <- "jags_RW"

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)


    true_count_lead   <- length(metadata$time$rodent_cast_moons)

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(abundance, cast_count)
   
    log_mean_count <- log(mean(abundance, na.rm = TRUE))


    data <- list(count                  = count, 
                 N                      = length(count),
                 log_mean_count    = log_mean_count)



    mods <- run.jags(model     = jags_model, 
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
      vals    <- mods$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods$mcmc[[j]])

        }

      }


      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      HPD            <- HPDinterval(as.mcmc(vals))
      casts        <- data.frame(pred = round(apply(vals, 2, mean), 3),
                                 lower       = round(HPD[ , "lower"], 3), 
                                 upper       = round(HPD[ , "upper"], 3),
                                 moon           = metadata$time$rodent_cast_moons)

      rownames(casts)      <- NULL

      cast_tab <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = metadata$dataset_controls[[dataset]]$args$output,
                               model            = model, 
                               dataset          = dataset, 
                               species          = species, 
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               estimate         = casts$pred, 
                               lower_pi         = casts$lower,
                               upper_pi         = casts$upper, 
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)


  metadata <- update_list(metadata,
                          models           = model,
                          datasets         = dataset,
                          dataset_controls = metadata$dataset_controls[[dataset]])

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts) 

}


#' @title Create a Control List for a runjags JAGS Model Run
#'
#' @description We leverage the \code{\link[runjags]{run.jags}} function in the runjags (Denwood 2016) package to run JAGS (Plummer 2003) models in portalcasting. That function has a number of control parameters that users may be interested in changing, and this function wraps those parameters with a few portalcasting-specific parameters into a control list for input into specific model functions.
#'
#' @param nchains Non-negative \code{integer}-conformable value of the number of parallel chains to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param adapt Non-negative \code{integer}-conformable value of the number of adaptation steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param burnin Non-negative \code{integer}-conformable value of the number of burnin steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param sample Non-negative \code{integer}-conformable value of the number of sampling steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param thin Positive \code{integer}-conformable value of the thinning interval to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param modules \code{character} vector of external modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param method \code{character} value of the \code{\link[runjags]{run.jags}} method to use. Options include \code{"rjags"}, \code{"simple"}, \code{"interruptible"}, \code{"parallel"}, \code{"rjparallel"}, \code{"background"}, \code{"bgparallel"}, and \code{"snow"}. See \code{\link[runjags]{run.jags}}.
#'
#' @param factories \code{character} vector of factory modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param mutate A \code{function} or \code{list} (with the first element being a \code{function}) used to add variables to the posterior chain (rather than throughout sampling). See \code{\link[runjags]{run.jags}}. 
#'
#' @param silent_jags \code{logical} value for quieting the output from the runjags function, including the underlying JAGS output. 
#'
#' @return \code{list} of controls. 
#'
#' @references 
#'  Denwood, M. J. 2016. runjags: an R package providing interface utilities, model templates, parallel computing methods and additional distributions for MCMC models in JAGS. Journal of Statistical Software, 71:9. \href{https://www.jstatsoft.org/article/view/v071i09}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical models using Gibbs Sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X. \href{https://bit.ly/33aQ37Y}{URL}.
#'
#' @examples
#'  runjags_control()
#'
#' @export
#'
runjags_control <- function (nchains     = 4, 
                             adapt       = 1e4, 
                             burnin      = 1e4, 
                             sample      = 1e4, 
                             thin        = 10, 
                             modules     = "glm", 
                             method      = "interruptible", 
                             factories   = "", 
                             mutate      = NA, 
                             silent_jags = FALSE){

  list(nchains     = nchains, 
       adapt       = adapt, 
       burnin      = burnin, 
       sample      = sample,
       thin        = thin, 
       modules     = modules, 
       method      = method, 
       factories   = factories,
       mutate      = mutate, 
       silent_jags = silent_jags)

}
