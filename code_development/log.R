
  dataset <- tolower(dataset)
  messageq("  -jags_logistic for ", dataset, quiet = quiet)

  monitor <- c("mu", "sigma", "r", "log_K")


  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

    log_mean_past_count <- log(mean(data$past_count, na.rm = TRUE))
    log_max_past_count  <- log(max(data$past_count, na.rm = TRUE))

    sd_log_past_count <- sd(log(data$past_count), na.rm = TRUE)

    function (chain = chain) {

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = rnorm(1, log_mean_past_count, 0.1), 
            sigma    = runif(1, 0, 0.0005),
            r        = rnorm(1, 0, 0.01),
            log_K    = rnorm(1, log_max_past_count, 0.01))

    }

  }

  jags_model <- "model { 
 
    # priors

    mu    ~  dnorm(log_mean_past_count, 5)
    sigma ~  dunif(0, 0.001) 
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


          model_name      = "jags_logistic"



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

i<-1

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
    log_max_past_count <- log(max(past_count, na.rm = TRUE))


    data <- list(count       = count, 
                 ntraps      = ntraps, 
                 N           = length(count),
                 moon        = moon, 
                 log_mean_past_count = log_mean_past_count,
                 log_max_past_count = log_max_past_count,
                 past_count  = past_count)

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
                          n.chains  = 6, #control_runjags$nchains, 
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


plot(mods[[i]])

pow <- function(x1, x2){x1 ^ x2}
mcmc <- runjags:::combine.mcmc(mods[[1]])


    mu    <- mcmc[ , "mu"]
    sigma <- mcmc[ , "sigma"]
    tau   <- pow(sigma, -1/2)
    r     <- mcmc[ , "r"]
    log_K <- mcmc[ , "log_K"]

    # initial state

N <- length(count)

plot(1:N, count, type = "p")
for(j in 1:100){

pcount <- NULL
log_X <- X <- pred_log_X <- pred_X <- NULL

    log_X[1]      <- mu[j]
    X[1]          <- exp(log_X[1])
    pcount[1]     <- rpois(1, X[1]) 


    # through time

    for(i in 2:N) {

      # Process model

      pred_X[i]     <- X[i-1] * exp(r[j] * (1 - (X[i - 1] / exp(log_K[j]))))

      pred_log_X[i] <- log(pred_X[i])
      log_X[i]      <- rnorm(1, pred_log_X[i], sigma[j])
      X[i]          <- exp(log_X[i])
   

      # observation model

      pcount[i] <- rpois(1, X[i])

    }


points(1:N, pcount, type = "l", col = grey(0.7, 0.2))
}

