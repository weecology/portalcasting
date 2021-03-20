
#
#
#  from scratch raw scale
#
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

    mu ~ dnorm(pred_mu, 0.5 * precision_past_count) T(1e-4, max_ntraps); 
    r ~ dnorm(mean_past_diff_rate, 
              0.5 * 1/(max(c(mean_past_diff_rate, 1e-4))^ 2));
    K ~ dnorm(max_past_count,
              0.5 * precision_past_count) T(1e-4, max(ntraps)); 
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


plot(modd, vars = c("mu", "r", "K", "a", "tau"))




