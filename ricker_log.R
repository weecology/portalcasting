
#
# trying to get this to work for the critical species
#
#   dm do pp ot pb 
#   pe rm
#
#   big issue is the K estimation
#   look like it works now if i just generalize the range 
#

devtools::load_all()
main <- "./testing"

data_set <- "all"
s <- "DM"
ss <- "DM"

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
    no_count <- is.na(past_count) == TRUE
    past_moon <- past_moon[!no_count]
    past_count <- past_count[!no_count]
    past_ntraps <- past_ntraps[!no_count]

    log_past_count <- log(apply(cbind(past_count, 1e-3), 1, max))


    past_N <- length(past_count)
    past_diff_log_rate <- diff_log_count <- diff_time <- rep(NA, past_N - 1)

    diff_log_count[1] <- log_past_count[2] - log_past_count[1]
    diff_time[1] <- past_moon[2] - past_moon[1]
    past_diff_log_rate[1] <- diff_log_count[1] / diff_time[1]
    for(j in 2:(past_N - 1)){
      diff_log_count[j] <- log_past_count[j] - log_past_count[j-1] 
      diff_time[j] <- past_moon[j] - past_moon[j-1]
      past_diff_log_rate[j] <- diff_log_count[j] / diff_time[j]
    }


    data <- list(count = count, ntraps = ntraps, max_ntraps = max(ntraps),
                 N = length(count), past_diff_log_rate = past_diff_log_rate,
                 moon = moon, past_moon = past_moon, 
                 past_ntraps = past_ntraps, past_N = past_N,
                 log_past_count = log_past_count)



  monitor <-c( "log_mu", "r", "log_K", "a", "tau", "X")

  inits <- function(data = NULL){
    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")

    log_past_count <- data$log_past_count 
    past_moon <- data$past_moon 
    past_N <- data$past_N
    moon <- data$moon
    max_ntraps <- data$max_ntraps

    mean_log_past_count <- mean(log_past_count)
    max_log_past_count <- max(c(max(log_past_count), exp(1)))
    sd_log_past_count <- max(c(sd(log_past_count), 1e-4))
    
    log_last_count <- log_past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon
    
    mean_past_diff_log_rate <- mean(past_diff_log_rate)
    sd_past_diff_log_rate <- sd(past_diff_log_rate)
    var_past_diff_log_rate <- sd_past_diff_log_rate^2
    precision_past_diff_log_rate <- 1/(var_past_diff_log_rate)

    pred_log_mu <- log_last_count + last_time_diff * mean_past_diff_log_rate

    rate <- 0.1
    shape <- precision_past_diff_log_rate * rate

    function(chain = chain){
      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            log_mu = rnorm(1, pred_log_mu, sd_log_past_count),
            log_K = runif(1, log(2), log(max_ntraps)),
            r = rnorm(1, mean_past_diff_log_rate, sd_past_diff_log_rate), 
            a = runif(1, 0.8, 1.2),
            tau = rgamma(1, shape = shape, rate = rate))
    }
  }


  jags_model <- "model {  


    mean_log_past_count <- mean(log_past_count)
    max_log_past_count <- max(c(max(log_past_count), exp(1)))
    sd_log_past_count <- max(c(sd(log_past_count), 1e-4))
    var_log_past_count <- sd_log_past_count ^ 2
    precision_log_past_count <- 1/(var_log_past_count)

    
    log_last_count <- log_past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon
    
    mean_past_diff_log_rate <- mean(past_diff_log_rate)
    sd_past_diff_log_rate <- sd(past_diff_log_rate)
    var_past_diff_log_rate <- sd_past_diff_log_rate^2
    precision_past_diff_log_rate <- 1/(var_past_diff_log_rate)

    pred_log_mu <- log_last_count + last_time_diff * mean_past_diff_log_rate


    rate <- 0.1
    shape <- precision_past_diff_log_rate * rate

    tau ~ dgamma(shape, rate);
 

    log_K ~ dunif(log(2), log(max_ntraps));
    log_mu ~ dnorm(pred_log_mu, precision_log_past_count);
    r ~ dnorm(mean_past_diff_log_rate, precision_past_diff_log_rate);
    a ~ dnorm(1, 100) T(0.1, 1.9)

    K <- exp(log_K)
    X[1] <- log_mu;
    pred_count[1] <- exp(X[1])
    count[1] ~ dpois(pred_count[1]) T( , ntraps[1]); 

    for(i in 2:N) {

      exp_last_X[i - 1] <- exp(max(c(X[i - 1], 1)))
      pred_X[i] <- X[i - 1] + r + -(r / K) * exp_last_X[i - 1] ^ a;
      X[i] ~ dnorm(pred_X[i], tau);
      pred_count[i] <- exp(X[i])
      count[i] ~ dpois(pred_count[i]) T( , ntraps[i]); 
    }
  }"




    modd <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = 4,
                          adapt = 1e4,
                          burnin = 1e4,
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
points(exp((xxx[6:NROW(xxx),2])), type = "l", lwd = 2)
points(exp((xxx[6:NROW(xxx),1])), type = "l")
points(exp((xxx[6:NROW(xxx),3])), type = "l")

plot(modd, vars = c("log_mu", "r", "log_K", "a", "tau"))


