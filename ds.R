main <- "./testing"

data_set <- "all"
s <- "DS"
ss <- "DS"

 arg_checks <- TRUE

control_files <- files_control() 
control_runjags <- runjags_control()
lag <- NA 
quiet <- FALSE
verbose <- TRUE

  rodents_table <- read_rodents_table(main = main, data_set = data_set, 
                                      arg_checks = arg_checks)

  metadata <- read_metadata(main = main, control_files = control_files,
                            arg_checks = arg_checks)
  data_set_controls <- metadata$controls_r[[data_set]]
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  true_count_lead <- length(metadata$rodent_cast_moons)
  CL <- metadata$confidence_level

N_past_moons <- 12
past_moon_0 <- start_moon - N_past_moons

    moon_in <- which(rodents_table$moon >= start_moon & 
                     rodents_table$moon <= end_moon)
    past_moon_in <- which(rodents_table$moon >= past_moon_0 &
                          rodents_table$moon < start_moon)
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

    no_count <- is.na(past_count) == TRUE
    past_moon <- past_moon[!no_count]
    past_count <- past_count[!no_count]
    past_ntraps <- past_ntraps[!no_count]

    past_N <- length(past_count)
    past_diff_rate <- diff_count <- diff_time <- rep(NA, past_N - 1)
    diff_count[1] <- past_count[2] - past_count[1]
    diff_time[1] <- past_moon[2] - past_moon[1]
    past_diff_rate[1] <- diff_count[1] / diff_time[1]
    for(j in 2:(past_N - 1)){
      diff_count[j] <- past_count[j] - past_count[j-1] 
      diff_time[j] <- past_moon[j] - past_moon[j-1]
      past_diff_rate[j] <- diff_count[j] / diff_time[j]
    }


    data <- list(count = count, ntraps = ntraps, max_ntraps = max(ntraps),
                 N = length(count), past_diff_rate = past_diff_rate,
                 moon = moon, past_moon = past_moon, past_count = past_count,
                 past_ntraps = past_ntraps, past_N = past_N)


# remove X !!!
  monitor <-c( "mu", "r", "K", "a", "tau", "X")

  inits <- function(data = NULL){
    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")

    past_count <- data$past_count 
    past_moon <- data$past_moon 
    past_N <- data$past_N
    moon <- data$moon

    mean_past_count <- mean(past_count)
    max_past_count <- max(past_count)
    sd_past_count <- sd(past_count)
    
    last_count <- past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon
    
    mean_past_diff_rate <- mean(past_diff_rate)
    sd_past_diff_rate <- sd(past_diff_rate)

    pred_mu <- last_count + last_time_diff * mean_past_diff_rate


    function(chain = chain){
      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu = max(c(rnorm(1, pred_mu, sd_past_count), 1e-4)),
            r = rnorm(1, mean_past_diff_rate, sd_past_diff_rate))#,
           # K = max(c(rnorm(1, 10, 2), 1e-4)), 
           # a = runif(1, 0.9, 1.1),
           # tau = rgamma(1, shape = 3, rate = 100))
    }
  }

  jags_model <- "model {  

    mean_past_count <- mean(past_count)
    max_past_count <- max(past_count)
    sd_past_count <- sd(past_count)
    var_past_count <- sd_past_count^2
    precision_past_count <- 1/(var_past_count)
 

    last_count <- past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon

    mean_past_diff_rate <- mean(past_diff_rate)
    sd_past_diff_rate <- sd(past_diff_rate)
    var_past_diff_rate <- sd_past_diff_rate^2
    precision_past_diff_rate <- 1/(var_past_diff_rate)

    pred_mu <- last_count + last_time_diff * mean_past_diff_rate

    mu ~ dnorm(pred_mu, precision_past_count) T(1e-4, max_ntraps); 
    r ~ dnorm(mean_past_diff_rate, precision_past_diff_rate);
    K ~ dnorm(max_past_count, 1) T( , max_ntraps); 
    tau <-3#~ dgamma(3, 100);
    a <-1#~ dunif(0.9, 1.1);

    X[1] <- mu;
    pred_count[1] <- X[1]
    count[1] ~ dpois(X[1]) T( , ntraps[1]); 

    for(i in 2:N) {

      pred_X[i] <- X[i-1] * exp(r) * exp(-(r / K) * X[i-1] ^ a);
      X[i] ~ dnorm(pred_X[i], tau) T(1e-4, max_ntraps);
      pred_count[i] <- X[i]
      count[i] ~ dpois(X[i]) T( , ntraps[i]); 
    }
  }"


    modd <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = 2,
                          adapt = 1e3,
                          burnin = 1e3,
                          sample = 1e3,
                          thin = control_runjags$thin, 
                          modules = control_runjags$modules, 
                          method = control_runjags$method, 
                          factories = control_runjags$factories, 
                          mutate = control_runjags$mutate, 
                          summarise = FALSE, plots = FALSE)







xxx <- summary(modd)
head(xxx)
tail(xxx)

plot(count)
points(((xxx[6:NROW(xxx),2])), type = "l")


plot(modd, vars = c("mu", "r", "K"))#, "a", "tau"))














# # # #

  data_set <- tolower(data_set)
  messageq(paste0("  -jags_Ricker for ", data_set), quiet)
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)

  monitor <-c( "mu", "r", "K", "a", "tau")

  inits <- function(data = NULL){
    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")

    past_count <- data$past_count 
    past_moon <- data$past_moon 
    past_N <- data$past_N
    moon <- data$moon

    mean_past_count <- mean(past_count)
    max_past_count <- max(past_count)
    sd_past_count <- sd(past_count)
    
    last_count <- past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon
    
    mean_past_diff_rate <- mean(past_diff_rate)
    sd_past_diff_rate <- max(c(sd(past_diff_rate) * sqrt(2), 0.01))
    var_past_diff_rate <- sd_past_diff_rate^2
    precision_past_diff_rate <- 1/(var_past_diff_rate)

    pred_mu <- last_count + last_time_diff * mean_past_diff_rate

    rate <- 100
    shape <- precision_past_diff_rate * rate

    function(chain = chain){
      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu = max(c(rnorm(1, pred_mu, sd_past_count), 1e-4)),
            r = rnorm(1, mean_past_diff_rate, 
                         max(c(1e-4, mean_past_diff_rate))),
            K = max(c(rnorm(1, max_past_count, sd_past_count),
                      1e-4)), 
            a = runif(1, 0.5, 1.5),
            tau = rgamma(1, shape = shape, rate = rate))
    }
  }


  jags_model <- "model {  

    mean_past_count <- mean(past_count)
    max_past_count <- max(past_count)
    sd_past_count <- max(c(sd(past_count) * sqrt(2), 0.01))
    var_past_count <- sd_past_count^2
    precision_past_count <- 1/(var_past_count)

    last_count <- past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon

    mean_past_diff_rate <- mean(past_diff_rate)
    sd_past_diff_rate <- max(c(sd(past_diff_rate) * sqrt(2), 0.01))
    var_past_diff_rate <- sd_past_diff_rate^2
    precision_past_diff_rate <- 1/(var_past_diff_rate)

    pred_mu <- max(c(last_count + last_time_diff * mean_past_diff_rate,
                     1e-4))

    rate <- 100
    shape <- precision_past_diff_rate * rate

    mu ~ dnorm(pred_mu, 1) T(1e-4, max_ntraps); 
    r ~ dnorm(mean_past_diff_rate, 
              1);
    K ~ dnorm(max_past_count,
             1) T(1e-4, max_ntraps); 
    tau ~ dgamma(shape, rate);
    a ~ dunif(0, 2);

    X[1] <- mu;
    pred_count[1] <- X[1]
    count[1] ~ dpois(X[1]) T( , ntraps[1]); 

    for(i in 2:N) {

      pred_X[i] <- X[i-1] * exp(r) * exp(-(r / K) * X[i-1] ^ a);
      X[i] ~ dnorm(pred_X[i], tau) T(1e-4, max_ntraps);
      pred_count[i] <- X[i]
      count[i] ~ dpois(X[i]) T( , ntraps[i]); 
    }
  }"




  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)
  runjags.options(silent.jags = F,#control_runjags$silent_jags, 
                  silent.runjags = F)#control_runjags$silent_jags)
  rodents_table <- read_rodents_table(main = main, data_set = data_set, 
                                      arg_checks = arg_checks)

  metadata <- read_metadata(main = main, control_files = control_files,
                            arg_checks = arg_checks)
  data_set_controls <- metadata$controls_r[[data_set]]
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  true_count_lead <- length(metadata$rodent_cast_moons)
  CL <- metadata$confidence_level


  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)
  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()




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

    past_N <- length(past_count)
    past_diff_rate <- diff_count <- diff_time <- rep(NA, past_N - 1)
    diff_count[1] <- past_count[2] - past_count[1]
    diff_time[1] <- past_moon[2] - past_moon[1]
    past_diff_rate[1] <- diff_count[1] / diff_time[1]
    for(j in 2:(past_N - 1)){
      diff_count[j] <- past_count[j] - past_count[j-1] 
      diff_time[j] <- past_moon[j] - past_moon[j-1]
      past_diff_rate[j] <- diff_count[j] / diff_time[j]
    }


    data <- list(count = count, ntraps = ntraps, max_ntraps = max(ntraps),
                 N = length(count), past_diff_rate = past_diff_rate,
                 moon = moon, past_moon = past_moon, past_count = past_count,
                 past_ntraps = past_ntraps, past_N = past_N)
   

    modd <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = 2,#control_runjags$nchains, 
                          adapt = 1e3,#control_runjags$adapt, 
                          burnin = 1e3,#control_runjags$burnin, 
                          sample = 1e3,#control_runjags$sample, 
                          thin = control_runjags$thin, 
                          modules = control_runjags$modules, 
                          method = control_runjags$method, 
                          factories = control_runjags$factories, 
                          mutate = control_runjags$mutate, 
                          summarise = FALSE, plots = FALSE)





            mu = max(c(rnorm(1, pred_mu, sd_past_count), 1e-4))
            r = rnorm(1, mean_past_diff_rate, 
                         max(c(1e-4, mean_past_diff_rate)))
            K = max(c(rnorm(1, max_past_count, sd_past_count),
                      1e-4)) 
            a = runif(1, 0.5, 1.5)
            tau = rgamma(1, shape = shape, rate = rate)


xxx <- summary(modd)
head(xxx)
tail(xxx)


plot(modd, vars = c("mu", "r", "K", "a", "tau"))
vals <- xxx[6:NROW(xxx),2]
plot(vals, type = "l", ylim = c(0, max(c(vals, count), na.rm = TRUE)))
points(count)




x <- rep(0, 300)


x[1] <- mu
for(i in 2:20){
  x[i] <- max(c(rnorm(1, 
                      x[i-1] * exp(r) * exp(-(r/K) * (x[i-1]^a)), 
                      sqrt(1/tau)), 
              0.01))
}
plot(x, type = "l", ylim = c(0, 30), col = 1)


