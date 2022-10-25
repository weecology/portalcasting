
  dataset <- tolower(dataset)
  messageq("  -jags_RW for ", dataset, quiet = quiet)

  monitor <- c("mu", "tau")

  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")
    past_N     <- data$past_N 
    past_count <- data$past_count 
    past_moon  <- data$past_moon

    log_past_count      <- log(past_count + 0.1)
    mean_log_past_count <- mean(log_past_count)
    sd_log_past_count   <- max(c(sd(log_past_count), 0.01))
    diff_log_past_count <- rep(NA, past_N - 1)

    for (i in 1:(past_N - 1)) {

      diff_count             <- log_past_count[i + 1] - log_past_count[i]
      diff_time              <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count / diff_time

    }
    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)

    rate  <- 0.1
    shape <- precision_diff_log_past_count * rate

    function (chain = chain) {

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = rnorm(1, mean_log_past_count, sd_log_past_count), 
            tau      = rgamma(1, shape = shape, rate = rate))

    }

  }

  jags_model <- "model { 
 
    # priors

    log_past_count           <- log(past_count + 0.1)
    mean_log_past_count      <- mean(log_past_count)
    sd_log_past_count        <- max(c(sd(log_past_count), 0.01))
    var_log_past_count       <- sd_log_past_count^2
    precision_log_past_count <- 1/(var_log_past_count)

    diff_count[1]          <- log_past_count[2] - log_past_count[1]
    diff_time[1]           <- past_moon[2] - past_moon[1] 
    diff_log_past_count[1] <- diff_count[1] / diff_time[1]

    for (i in 2:(past_N - 1)) {

      diff_count[i]          <- log_past_count[i + 1] - log_past_count[i]
      diff_time[i]           <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count[i] / diff_time[i]

    }    

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)

    rate  <- 0.1
    shape <- precision_diff_log_past_count * rate

    mu  ~ dnorm(mean_log_past_count, precision_log_past_count); 
    tau ~ dgamma(shape, rate); 
   

    # initial state

    X[1]          <- mu;
    pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
    count[1]      ~  dpois(pred_count[1]) 


    # through time

    for(i in 2:N) {

      # Process model

      predX[i]      <- X[i-1];
      checkX[i]     ~  dnorm(predX[i], tau); 
      X[i]          <- c(checkX[i]); 
      pred_count[i] <- max(c(exp(X[i]) - 0.1, 0.00001));
   

      # observation model

      count[i] ~ dpois(pred_count[i])

    }

  }"


          model_name      = "jags_RW"







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

    data <- list(count       = count, 
                 ntraps      = ntraps, 
                 N           = length(count),
                 moon        = moon, 
                 past_moon   = past_moon, 
                 past_count  = past_count,
                 past_ntraps = past_ntraps, 
                 past_N      = length(past_count))

    obs_pred_times      <- metadata$time$rodent_cast_moons 
    obs_pred_times_spot <- obs_pred_times - metadata$time$start_moon

    train_text <- paste0("train: ", metadata$start_moon, " to ", metadata$end_moon)
    test_text  <- paste0("test: ",  metadata$end_moon + 1, " to ", metadata$end_moon + metadata$lead)
    model_text <- paste0("model: ", model_name, "; ", train_text, "; ", test_text)

    if (control_runjags$cast_obs) {

      obs_pred <- paste0("pred_count[", obs_pred_times_spot, "]")
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


plot(mods[[i]])
