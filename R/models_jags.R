#' @rdname prefab_model_functions
#'
#' @export
#'
jags_logistic_covariates <- function (main            = ".", 
                                      dataset         = "dm_controls",  
                                      settings        = directory_settings(), 
                                      control_runjags = runjags_control(), 
                                      quiet           = FALSE, 
                                      verbose         = FALSE) {


  dataset     <- tolower(dataset)
  messageq("  -jags_logistic_covariates for ", dataset, quiet = quiet)

  monitor <- c("mu", "sigma", "r_int", "r_slope", "log_K_int", "log_K_slope")


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    log_mean_past_count <- log(mean(data$past_count, na.rm = TRUE))
    log_max_past_count  <- log(max(data$past_count, na.rm = TRUE))

    sd_log_past_count <- sd(log(data$past_count), na.rm = TRUE)

    function (chain = chain) {

      mu        <- rnorm(1, log_mean_past_count, 0.1)
      log_K_int <- rnorm(1, log_max_past_count, 0.01)

      log_K_int <- max(c(mu, log_K_int)) + 0.1

      list(.RNG.name    = sample(rngs, 1),
           .RNG.seed    = sample(1:1e+06, 1),
            mu          = mu, 
            sigma       = runif(1, 0, 0.0005),
            r_int       = rnorm(1, 0, 0.01),
            r_slope     = rnorm(1, 0, 0.1),
            log_K_int   = log_K_int,
            log_K_slope = rnorm(1, 0, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu          ~  dnorm(log_mean_past_count, 5)
    sigma       ~  dunif(0, 1) 
    tau         <- pow(sigma, -1/2)
    r_int       ~  dnorm(0, 5)
    r_slope     ~  dnorm(0, 1)
    log_K_int   ~  dnorm(log_max_past_count, 5)
    log_K_slope ~  dnorm(0, 1)
 
    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 

    # exampand parameters

    for (i in 1:N) {

      r[i] <- r_int + r_slope * warm_rain_three_months[i]
      K[i] <- exp(log_K_int + log_K_slope * ndvi_twelve_months[i]) 

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




  model_name     <- "jags_logistic_covariates"

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  rodents_table <- read_rodents_table(main     = main,
                                      dataset = dataset, 
                                      settings = settings) 
  covariates    <- read_covariates(main     = main,
                                   settings = settings)
  metadata      <- read_metadata(main     = main,
                                 settings = settings)

  dataset_controls  <- metadata$controls_r[[dataset]]
  start_moon        <- metadata$time$start_moon
  end_moon          <- metadata$time$end_moon
  true_count_lead   <- length(metadata$time$rodent_cast_moons)
  confidence_level  <- metadata$confidence_level
  dataset_controls  <- metadata$dataset_controls[[dataset]]

  species <- species_from_table(rodents_tab = rodents_table, 
                                total       = TRUE, 
                                nadot       = TRUE)
  nspecies <- length(species)
  mods     <- named_null_list(species)
  casts    <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)

    moon_in      <- which(rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon)
    past_moon_in <- which(rodents_table$newmoonnumber < start_moon)
    moon         <- rodents_table[moon_in, "newmoonnumber"] 
    moon         <- c(moon, metadata$time$rodent_cast_moons)
    past_moon    <- rodents_table[past_moon_in, "newmoonnumber"]

    ntraps           <- rodents_table[moon_in, "ntraps"] 
    na_traps         <- which(is.na(ntraps) == TRUE)
    ntraps[na_traps] <- 0
    cast_ntraps      <- rep(max(ntraps), true_count_lead)
    ntraps           <- c(ntraps, cast_ntraps)
    past_ntraps      <- rodents_table[past_moon_in, "ntraps"]

    species_in <- which(colnames(rodents_table) == s)
    count <- rodents_table[moon_in, species_in]

    if (sum(count, na.rm = TRUE) == 0) {

      next()

    }

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(count, cast_count)
    past_count  <- rodents_table[past_moon_in, species_in]
    no_count    <- which(is.na(past_count) == TRUE)

    if (length(no_count) > 0) {

      past_moon   <- past_moon[-no_count]
      past_count  <- past_count[-no_count]
      past_ntraps <- past_ntraps[-no_count]

    }

    log_mean_past_count <- log(mean(past_count, na.rm = TRUE))
    log_max_past_count  <- log(max(past_count, na.rm = TRUE))

    warm_rain_three_months <- scale(covariates$warm_precip_3_month[covariates$newmoonnumber >= start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])
    ndvi_twelve_months     <- scale(covariates$ndvi_12_month[covariates$newmoonnumber >= start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])

    data <- list(count                  = count, 
                 ntraps                 = ntraps, 
                 N                      = length(count),
                 moon                   = moon, 
                 log_mean_past_count    = log_mean_past_count,
                 log_max_past_count     = log_max_past_count,
                 past_count             = past_count,
                 warm_rain_three_months = warm_rain_three_months[ , 1],
                 ndvi_twelve_months     = ndvi_twelve_months[ , 1])


    obs_pred_times      <- metadata$time$rodent_cast_moons 
    obs_pred_times_spot <- obs_pred_times - metadata$time$start_moon

    train_text <- paste0("train: ", metadata$start_moon, " to ", metadata$end_moon)
    test_text  <- paste0("test: ",  metadata$end_moon + 1, " to ", metadata$end_moon + metadata$lead)
    model_text <- paste0("model: ", model_name, "; ", train_text, "; ", test_text)

    if (control_runjags$cast_obs) {

      obs_pred <- paste0("X[", obs_pred_times_spot, "]")
      monitor  <- c(monitor, obs_pred) 

    }

    mods[[i]] <- run.jags(model     = jags_model, 
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

    if (control_runjags$cast_obs) {

      nchains <- control_runjags$nchains
      vals    <- mods[[i]]$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods[[i]]$mcmc[[j]])

        }

      }

      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      point_forecast <- round(apply(vals, 2, mean), 3)
      HPD            <- HPDinterval(as.mcmc(vals))
      lower_cl       <- round(HPD[ , "lower"], 3)
      upper_cl       <- round(HPD[ , "upper"], 3)
      casts_i        <- data.frame(Point.Forecast = point_forecast,
                                   lower_cl       = lower_cl, 
                                   upper_cl       = upper_cl,
                                   moon           = obs_pred_times)

      colnames(casts_i)[2:3] <- paste0(c("Lo.", "Hi."), confidence_level * 100)
      rownames(casts_i)      <- NULL
      casts[[i]]             <- casts_i

      cast_tab_i <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = dataset_controls$args$output,
                               model            = model_name, 
                               dataset          = dataset, 
                               species          = ss, 
                               estimate         = point_forecast,
                               lower_pi         = lower_cl, 
                               upper_pi         = upper_cl,
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)

      cast_tab <- rbind(cast_tab, cast_tab_i)

    }

  }

  metadata <- update_list(metadata,
                          models           = "jags_logistic_covariates",
                          datasets         = dataset,
                          dataset_controls = dataset_controls)

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
                           dataset         = "dm_controls",  
                           settings        = directory_settings(), 
                           control_runjags = runjags_control(), 
                           quiet           = FALSE, 
                           verbose         = FALSE) {


  dataset     <- tolower(dataset)
  messageq("  -jags_logistic for ", dataset, quiet = quiet)

  monitor <- c("mu", "sigma", "r", "log_K")


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    log_mean_past_count <- log(mean(data$past_count, na.rm = TRUE))
    log_max_past_count  <- log(max(data$past_count, na.rm = TRUE))

    sd_log_past_count <- sd(log(data$past_count), na.rm = TRUE)

    function (chain = chain) {

      mu    <- rnorm(1, log_mean_past_count, 0.1)
      log_K <- rnorm(1, log_max_past_count, 0.01)

      log_K <- max(c(mu, log_K)) + 0.01

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = mu, 
            sigma    = runif(1, 0, 0.0005),
            r        = rnorm(1, 0, 0.01),
            log_K    = log_K)

    }

  }

  jags_model <- "model { 
 
    # priors

    mu    ~  dnorm(log_mean_past_count, 5)
    sigma ~  dunif(0, 1) 
    tau   <- pow(sigma, -1/2)
    r     ~  dnorm(0, 5)
    log_K ~  dnorm(log_max_past_count, 5)
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




  model_name     <- "jags_logistic"

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  rodents_table <- read_rodents_table(main     = main,
                                      dataset = dataset, 
                                      settings = settings) 
  metadata      <- read_metadata(main     = main,
                                 settings = settings)

  dataset_controls  <- metadata$controls_r[[dataset]]
  start_moon        <- metadata$time$start_moon
  end_moon          <- metadata$time$end_moon
  true_count_lead   <- length(metadata$time$rodent_cast_moons)
  confidence_level  <- metadata$confidence_level
  dataset_controls  <- metadata$dataset_controls[[dataset]]

  species <- species_from_table(rodents_tab = rodents_table, 
                                total       = TRUE, 
                                nadot       = TRUE)
  nspecies <- length(species)
  mods     <- named_null_list(species)
  casts    <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)

    moon_in      <- which(rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon)
    past_moon_in <- which(rodents_table$newmoonnumber < start_moon)
    moon         <- rodents_table[moon_in, "newmoonnumber"] 
    moon         <- c(moon, metadata$time$rodent_cast_moons)
    past_moon    <- rodents_table[past_moon_in, "newmoonnumber"]

    ntraps           <- rodents_table[moon_in, "ntraps"] 
    na_traps         <- which(is.na(ntraps) == TRUE)
    ntraps[na_traps] <- 0
    cast_ntraps      <- rep(max(ntraps), true_count_lead)
    ntraps           <- c(ntraps, cast_ntraps)
    past_ntraps      <- rodents_table[past_moon_in, "ntraps"]

    species_in <- which(colnames(rodents_table) == s)
    count <- rodents_table[moon_in, species_in]

    if (sum(count, na.rm = TRUE) == 0) {

      next()

    }

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(count, cast_count)
    past_count  <- rodents_table[past_moon_in, species_in]
    no_count    <- which(is.na(past_count) == TRUE)

    if (length(no_count) > 0) {

      past_moon   <- past_moon[-no_count]
      past_count  <- past_count[-no_count]
      past_ntraps <- past_ntraps[-no_count]

    }

    log_mean_past_count <- log(mean(past_count, na.rm = TRUE))
    log_max_past_count  <- log(max(past_count, na.rm = TRUE))

    data <- list(count               = count, 
                 ntraps              = ntraps, 
                 N                   = length(count),
                 moon                = moon, 
                 log_mean_past_count = log_mean_past_count,
                 log_max_past_count  = log_max_past_count,
                 past_count          = past_count)


    obs_pred_times      <- metadata$time$rodent_cast_moons 
    obs_pred_times_spot <- obs_pred_times - metadata$time$start_moon

    train_text <- paste0("train: ", metadata$start_moon, " to ", metadata$end_moon)
    test_text  <- paste0("test: ",  metadata$end_moon + 1, " to ", metadata$end_moon + metadata$lead)
    model_text <- paste0("model: ", model_name, "; ", train_text, "; ", test_text)

    if (control_runjags$cast_obs) {

      obs_pred <- paste0("X[", obs_pred_times_spot, "]")
      monitor  <- c(monitor, obs_pred) 

    }

    mods[[i]] <- run.jags(model     = jags_model, 
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

    if (control_runjags$cast_obs) {

      nchains <- control_runjags$nchains
      vals    <- mods[[i]]$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods[[i]]$mcmc[[j]])

        }

      }

      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      point_forecast <- round(apply(vals, 2, mean), 3)
      HPD            <- HPDinterval(as.mcmc(vals))
      lower_cl       <- round(HPD[ , "lower"], 3)
      upper_cl       <- round(HPD[ , "upper"], 3)
      casts_i        <- data.frame(Point.Forecast = point_forecast,
                                   lower_cl       = lower_cl, 
                                   upper_cl       = upper_cl,
                                   moon           = obs_pred_times)

      colnames(casts_i)[2:3] <- paste0(c("Lo.", "Hi."), confidence_level * 100)
      rownames(casts_i)      <- NULL
      casts[[i]]             <- casts_i

      cast_tab_i <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = dataset_controls$args$output,
                               model            = model_name, 
                               dataset          = dataset, 
                               species          = ss, 
                               estimate         = point_forecast,
                               lower_pi         = lower_cl, 
                               upper_pi         = upper_cl,
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)

      cast_tab <- rbind(cast_tab, cast_tab_i)

    }

  }

  metadata <- update_list(metadata,
                          models           = model_name,
                          datasets         = dataset,
                          dataset_controls = dataset_controls)

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
                     dataset         = "dm_controls",  
                     settings        = directory_settings(),
                     control_runjags = runjags_control(), 
                     quiet           = FALSE, 
                     verbose         = FALSE){
 
  dataset <- tolower(dataset)
  messageq("  -jags_RW for ", dataset, quiet = quiet)

  monitor <- c("mu", "sigma")

  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")
 
    log_mean_past_count <- log(mean(data$past_count, na.rm = TRUE))

    function (chain = chain) {

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = rnorm(1, mean = log_mean_past_count, sd = 0.1),  
            sigma    = runif(1, min = 0, max = 0.0005))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu    ~  dnorm(log_mean_past_count, 5)
    sigma ~  dunif(0, 1) 
    tau   <- pow(sigma, -1/2)

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

  model_name     <- "jags_RW"

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  rodents_table <- read_rodents_table(main     = main,
                                      dataset = dataset, 
                                      settings = settings) 
  metadata      <- read_metadata(main     = main,
                                 settings = settings)

  dataset_controls  <- metadata$controls_r[[dataset]]
  start_moon        <- metadata$time$start_moon
  end_moon          <- metadata$time$end_moon
  true_count_lead   <- length(metadata$time$rodent_cast_moons)
  confidence_level  <- metadata$confidence_level
  dataset_controls  <- metadata$dataset_controls[[dataset]]

  species <- species_from_table(rodents_tab = rodents_table, 
                                total       = TRUE, 
                                nadot       = TRUE)
  nspecies <- length(species)
  mods     <- named_null_list(species)
  casts    <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)

    moon_in      <- which(rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon)
    past_moon_in <- which(rodents_table$newmoonnumber < start_moon)
    moon         <- rodents_table[moon_in, "newmoonnumber"] 
    moon         <- c(moon, metadata$time$rodent_cast_moons)
    past_moon    <- rodents_table[past_moon_in, "newmoonnumber"]

    ntraps           <- rodents_table[moon_in, "ntraps"] 
    na_traps         <- which(is.na(ntraps) == TRUE)
    ntraps[na_traps] <- 0
    cast_ntraps      <- rep(max(ntraps), true_count_lead)
    ntraps           <- c(ntraps, cast_ntraps)
    past_ntraps      <- rodents_table[past_moon_in, "ntraps"]

    species_in <- which(colnames(rodents_table) == s)
    count <- rodents_table[moon_in, species_in]

    if (sum(count, na.rm = TRUE) == 0) {

      next()

    }

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(count, cast_count)
    past_count  <- rodents_table[past_moon_in, species_in]
    no_count    <- which(is.na(past_count) == TRUE)

    if (length(no_count) > 0) {

      past_moon   <- past_moon[-no_count]
      past_count  <- past_count[-no_count]
      past_ntraps <- past_ntraps[-no_count]

    }

    log_mean_past_count <- log(mean(past_count, na.rm = TRUE))

    data <- list(count               = count, 
                 ntraps              = ntraps, 
                 N                   = length(count),
                 moon                = moon, 
                 log_mean_past_count = log_mean_past_count,
                 past_count          = past_count)


    obs_pred_times      <- metadata$time$rodent_cast_moons 
    obs_pred_times_spot <- obs_pred_times - metadata$time$start_moon

    train_text <- paste0("train: ", metadata$start_moon, " to ", metadata$end_moon)
    test_text  <- paste0("test: ",  metadata$end_moon + 1, " to ", metadata$end_moon + metadata$lead)
    model_text <- paste0("model: ", model_name, "; ", train_text, "; ", test_text)

    if (control_runjags$cast_obs) {

      obs_pred <- paste0("X[", obs_pred_times_spot, "]")
      monitor  <- c(monitor, obs_pred) 

    }

    mods[[i]] <- run.jags(model     = jags_model, 
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

    if (control_runjags$cast_obs) {

      nchains <- control_runjags$nchains
      vals    <- mods[[i]]$mcmc[[1]]

      if (nchains > 1) {

        for (j in 2:nchains) {

          vals <- rbind(vals, mods[[i]]$mcmc[[j]])

        }

      }

      pred_cols      <- grep("X", colnames(vals))
      vals           <- vals[ , pred_cols]
      point_forecast <- round(apply(vals, 2, mean), 3)
      HPD            <- HPDinterval(as.mcmc(vals))
      lower_cl       <- round(HPD[ , "lower"], 3)
      upper_cl       <- round(HPD[ , "upper"], 3)
      casts_i        <- data.frame(Point.Forecast = point_forecast,
                                   lower_cl       = lower_cl, 
                                   upper_cl       = upper_cl,
                                   moon           = obs_pred_times)

      colnames(casts_i)[2:3] <- paste0(c("Lo.", "Hi."), confidence_level * 100)
      rownames(casts_i)      <- NULL
      casts[[i]]             <- casts_i

      cast_tab_i <- data.frame(cast_date        = metadata$time$cast_date, 
                               cast_month       = metadata$time$rodent_cast_months,
                               cast_year        = metadata$time$rodent_cast_years, 
                               moon             = metadata$time$rodent_cast_moons,
                               currency         = dataset_controls$args$output,
                               model            = model_name, 
                               dataset          = dataset, 
                               species          = ss, 
                               estimate         = point_forecast,
                               lower_pi         = lower_cl, 
                               upper_pi         = upper_cl,
                               cast_group       = metadata$cast_group,
                               confidence_level = metadata$confidence_level,
                               start_moon       = metadata$time$start_moon,
                               end_moon         = metadata$time$end_moon)

      cast_tab <- rbind(cast_tab, cast_tab_i)

    }

  }

  metadata <- update_list(metadata,
                          models           = model_name,
                          datasets         = dataset,
                          dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts) 
 


}


#' @title Create a control list for a runjags JAGS model run
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
#' @param thin Non-negative \code{integer}-conformable value of the thinning interval to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param modules \code{character} vector of external modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param method \code{character} value of the \code{\link[runjags]{run.jags}} method to use. Options include \code{"rjags"}, \code{"simple"}, \code{"interruptible"}, \code{"parallel"}, \code{"rjparallel"}, \code{"background"}, \code{"bgparallel"}, and \code{"snow"}. See \code{\link[runjags]{run.jags}}.
#'
#' @param factories \code{character} vector of factory modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param mutate A \code{function} or \code{list} (with the first element being a \code{function}) used to add variables to the posterior chain (rather than throughout sampling). See \code{\link[runjags]{run.jags}}. 
#'
#' @param cast_obs \code{logical} value indicating if the observations should be forecast as well (should be kept as \code{TRUE} for the vast majority of uses).
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
                             thin        = 1, 
                             modules     = "glm", 
                             method      = "interruptible", 
                             factories   = "", 
                             mutate      = NA, 
                             cast_obs    = TRUE, 
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
       cast_obs    = cast_obs, 
       silent_jags = silent_jags)

}
