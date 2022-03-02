devtools::load_all()
main <- "./testing"
setup_dir(main)

data_set <- "DM_controls"


# the inits function helps, but we should be doing this processing once and passing the data in
# esp since re doing the calc in the model code eep
# simple repositioning for future

# start with uninformed priors really!


  NA_to_0 <- function (x) {

    x[which(is.na(x) == TRUE)] <- 0
    x

  }

  rodents_table <- read_rodents_table(main = main, data_set = data_set)

  metadata      <- read_metadata(main = main)

  start_moon    <- metadata$start_moon
  end_moon      <- metadata$end_moon

  moon_in      <- which(rodents_table$moon >= start_moon & rodents_table$moon <= end_moon)
  past_moon_in <- which(rodents_table$moon < start_moon)
  moon         <- rodents_table[moon_in, "moon"] 
  moon         <- c(moon, metadata$rodent_cast_moons)
  past_moon    <- rodents_table[past_moon_in, "moon"]
 
  true_count_lead <- length(metadata$rodent_cast_moons)

  ntraps       <- NA_to_0(rodents_table[moon_in, "ntraps"] )
  cast_ntraps  <- rep(max(ntraps), true_count_lead)
  ntraps       <- c(ntraps, cast_ntraps)
  past_ntraps  <- rodents_table[past_moon_in, "ntraps"]

  s <- "DM"

  species_in   <- which(colnames(rodents_table) == s)
  count        <- rodents_table[moon_in, species_in]
  cast_count   <- rep(NA, true_count_lead)
  count        <- c(count, cast_count)
  past_count   <- rodents_table[past_moon_in, species_in]
  N            <- length(count)
  past_N       <- length(past_count)
  all_count    <- rodents_table[ , species_in]
 


  log_past_count      <- log(past_count + 1 / past_ntraps)
  mean_log_past_count <- mean(log_past_count, na.rm = TRUE)
  sd_log_past_count   <- sd(log_past_count, na.rm = TRUE)
  max_log_past_count  <- max(log_past_count, na.rm = TRUE)
  count0              <- past_count[past_N]

  data <- list(count               = count, 
               count0              = count0,
               ntraps              = ntraps, 
               N                   = N,
               moon                = moon,
               max_log_past_count  = max_log_past_count,
               mean_log_past_count = mean_log_past_count,
               sd_log_past_count   = sd_log_past_count)
    




  jags_model <- "model {

    r       ~ dnorm(0, 5)

    log_K   ~  dnorm(max_log_past_count, 10 / max_log_past_count)
    K       <- exp(log_K)

    log_X0  ~  dnorm(mean_log_past_count, pow(sd_log_past_count, -2))
    X0      <- exp(log_X0)
    count0  ~  dpois(X0)
  
    X[1]     <- max(c(X0 * exp(r * (1 - (X0 / K))), 0.000001))
    count[1] ~  dpois(X[1])

    for (i in 2:nmoons) {

      X[i]     <- max(c(X[i-1] * exp(r * (1 - (X[i - 1] / K))), 0.000001))

      count[i] ~  dpois(X[i])

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
                        adapt     = 10000,
                        burnin    = 10000,
                        sample    = 10000,
                        thin      = 1,
                        modules   = "glm", 
                        method    = "parallel")

  round(summary(model_fit), 4)
  plot(model_fit)

















  log_count0  <- log(count_prior[nmoons_prior])




  data <- list(count_train     = count_train, 
               log_count0      = log_count0,
               nmoons_train    = nmoons_train)


  jags_model <- "model {

    log_X0 ~ dnorm(log_count0, 0.1)

    r ~ dnorm(0, 0.1)
    c ~ dnorm(0, 0.1)

    X0             <- exp(log_X0)
    X[1]           <- max(c(exp(log(X0) + r - c * X0), 0.000001))
    count_train[1] ~ dpois(X[1])

    for (i in 2:nmoons_train) {

     X[i]            <- max(c(exp(log(X[i - 1]) + r - c * X[i - 1]), 0.000001))

     count_train[i]  ~ dpois(X[i])

    }

   }"

  inits <- function (data = NULL) {

    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")


    function (chain = chain) {

      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
           log_X0    = rnorm(1, log_count0, 1),
           r         = rnorm(1, 0, 1),
           c         = rnorm(1, 0, 1))

    }

  }

  monitor <- c("log_X0", "r", "c")

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





#####################################################################












  monitor <- c("mu", "tau")


  data_set_controls <- metadata$controls_r[[data_set]]
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  true_count_lead <- length(metadata$rodent_cast_moons)
  CL <- metadata$confidence_level

  if(covariatesTF){
    last_cc_moon <- max(metadata$covariate_cast_moons)
    covar <- read_covariates(main = main, control_files = control_files)
    covar_lag <- lag_covariates(covariates = covar, lag = lag, 
                                tail = TRUE)
    covar_moon_in <- which(covar_lag$moon >= start_moon & 
                           covar_lag$moon <= last_cc_moon)
    col_in <- which(colnames(covar_lag) != "source")
    covar_in <- as.matrix(covar_lag[covar_moon_in, col_in])

    past_covar_moon_in <- which(covar_lag$moon < start_moon)
    past_covar_in <- as.matrix(covar_lag[covar_moon_in, col_in])
  }

  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)
  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()
  for(i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq(paste0("   -", ss), !verbose)

    moon_in <- which(rodents_table$moon >= start_moon & 
                     rodents_table$moon <= end_moon)
    past_moon_in <- which(rodents_table$moon < start_moon)
    moon <- rodents_table[moon_in, "moon"] 
    moon <- c(moon, metadata$rodent_cast_moons)
    past_moon <- rodents_table[past_moon_in, "moon"]

    ntraps <- rodents_table[moon_in, "ntraps"] 
    ntraps[which(is.na(ntraps) == TRUE)] <- 0
    cast_ntraps <- rep(max(ntraps), true_count_lead)
    ntraps <- c(ntraps, cast_ntraps)
    past_ntraps <- rodents_table[past_moon_in, "ntraps"]

    species_in <- which(colnames(rodents_table) == s)
    count <- rodents_table[moon_in, species_in]
    if(sum(count, na.rm = TRUE) == 0){
      next()
    }
    cast_count <- rep(NA, true_count_lead)
    count <- c(count, cast_count)
    past_count <- rodents_table[past_moon_in, species_in]

    no_count <- which(is.na(past_count) == TRUE)
    past_moon <- past_moon[-no_count]
    past_count <- past_count[-no_count]
    past_ntraps <- past_ntraps[-no_count]

    data <- list(count = count, ntraps = ntraps, N = length(count),
                 moon = moon, past_moon = past_moon, past_count = past_count,
                 past_ntraps = past_ntraps, past_N = length(past_count))



    log_past_count           <- log(past_count + 0.1)
    mean_log_past_count      <- mean(log_past_count)
    sd_log_past_count        <- max(c(sd(log_past_count) * sqrt(2), 0.01))
    var_log_past_count       <- sd_log_past_count^2
    precision_log_past_count <- 1/(var_log_past_count)

    diff_count[1] <- log_past_count[2] - log_past_count[1]
    diff_time[1] <- past_moon[2] - past_moon[1] 
    diff_log_past_count[1] <- diff_count[1] / diff_time[1]
    for(i in 2:(past_N - 1)){
      diff_count[i] <- log_past_count[i + 1] - log_past_count[i]
      diff_time[i] <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count[i] / diff_time[i]
    }    
    sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate <- 0.1
    shape <- precision_diff_log_past_count * rate



  inits <- function(data = NULL){
    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")
    past_N <- data$past_N 
    past_count <- data$past_count 
    past_moon <- data$past_moon

    function(chain = chain){
      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu = rnorm(1, mean_log_past_count, sd_log_past_count), 
            tau = rgamma(1, shape = shape, rate = rate))
    }
  }


  jags_model <- "model {  

    # priors

    mu ~ dnorm(mean_log_past_count, precision_log_past_count); 
    tau ~ dgamma(shape, rate); 
   
    # initial state

    X[1] <- mu;
    pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
    count[1] ~ dpois(max(c(exp(X[1]) - 0.1, 0.00001))) T(0, ntraps[1]);

    # through time

    for(i in 2:N) {

      # Process model

      predX[i] <- X[i-1];
      checkX[i] ~ dnorm(predX[i], tau); 
      X[i] <- min(c(checkX[i], log(ntraps[i] + 1))); 
      pred_count[i] <- max(c(exp(X[i]) - 0.1, 0.00001));
   
      # observation model

      count[i] ~ dpois(max(c(exp(X[i]) - 0.1, 0.00001))) T(0, ntraps[i]); 
    }

  }"




    if(covariatesTF){
      data[["covariates"]] <- covar_in
    }

    obs_pred_times <- metadata$rodent_cast_moons 
    obs_pred_times_spot <- obs_pred_times - metadata$start_moon

    name1 <- paste0("train: ", metadata$start_moon, " to ", metadata$end_moon)
    name2 <- paste0("test: ",  metadata$end_moon + 1, " to ", 
                    metadata$end_moon + metadata$lead)
    model_text <- paste0("model: ", name1, "; ", name2)

    if(control_runjags$cast_obs){
      obs_pred <- paste0("pred_count[", obs_pred_times_spot, "]")
      monitor <- c(monitor, obs_pred) 
    }

    mod_fit <- run.jags(model     = jags_model, 
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


