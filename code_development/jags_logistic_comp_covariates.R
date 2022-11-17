jags_logistic_competition_covariates <- function (main            = ".", 
                                                  dataset         = "controls",  
                                                  settings        = directory_settings(), 
                                                  control_runjags = runjags_control(), 
                                                  quiet           = FALSE, 
                                                  verbose         = FALSE) {


  dataset     <- tolower(dataset)
  messageq("  -jags_logistic_competition_covariates for ", dataset, quiet = quiet)

  monitor <- c("mu", "sigma", "r_int", "r_slope", "log_K_int", "log_K_slope_NDVI", "log_K_slope_DS")


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    log_mean_past_count <- log(mean(data$past_count, na.rm = TRUE))
    log_max_past_count  <- log(max(data$past_count, na.rm = TRUE))

    sd_log_past_count <- sd(log(data$past_count), na.rm = TRUE)

    function (chain = chain) {

      mu        <- rnorm(1, log_mean_past_count, 0.1)
      log_K_int <- rnorm(1, log_max_past_count, 0.01)

      log_K_int <- max(c(mu, log_K_int)) + 0.1

      list(.RNG.name         = sample(rngs, 1),
           .RNG.seed         = sample(1:1e+06, 1),
            mu               = mu, 
            sigma            = runif(1, 0, 0.0005),
            r_int            = rnorm(1, 0, 0.01),
            r_slope          = rnorm(1, 0, 0.1),
            log_K_int        = log_K_int,
            log_K_slope_NDVI = rnorm(1, 0, 0.01),
            log_K_slope_DS   = rnorm(1, 0, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu               ~  dnorm(log_mean_past_count, 5)
    sigma            ~  dunif(0, 0.001) 
    tau              <- pow(sigma, -1/2)
    r_int            ~  dnorm(0, 5)
    r_slope          ~  dnorm(0, 1)
    log_K_int        ~  dnorm(log_max_past_count, 5)
    log_K_slope_NDVI ~  dnorm(0, 4)
    log_K_slope_DS   ~  dnorm(0, 4)
 
    # initial state

    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      ~  dpois(X[1]) 

    # exampand parameters

    for (i in 1:N) {

      r[i] <- r_int + r_slope * warm_rain_three_months[i]
      K[i] <- exp(log_K_int + log_K_slope_DS * spectab[i] + log_K_slope_NDVI * ndvi_twelve_months[i]) 

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




  model_name     <- "jags_logistic_competition_covariates"

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
  temp_species  <- read_model_controls(main = main, settings = settings)$jags_logistic_competition_covariates$species
  if (temp_species == "all") {
    species <- species
  } else {
    species <- species[species %in% temp_species]
  }
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
    spectab_controls       <- scale(covariates$spectab_controls[covariates$newmoonnumber >= start_moon & covariates$newmoonnumber <= max(metadata$time$covariate_cast_moons)])
 

    data <- list(count                  = count, 
                 ntraps                 = ntraps, 
                 N                      = length(count),
                 moon                   = moon, 
                 log_mean_past_count    = log_mean_past_count,
                 log_max_past_count     = log_max_past_count,
                 past_count             = past_count,
                 warm_rain_three_months = warm_rain_three_months[ , 1],
                 ndvi_twelve_months     = ndvi_twelve_months[ , 1],
                 spectab                = spectab_controls[ , 1])


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