 
data_set        = "dm_controls"  
control_files   = files_control()
control_runjags = runjags_control(silent_jags = FALSE)
lag             = NA 
quiet           = FALSE 
verbose         = TRUE 
arg_checks      = FALSE




  data_set     <- tolower(data_set)
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)

  messageq(paste0("  -jags_logistic for ", data_set), quiet)


  monitor <- c("mu", "tau", "r", "K")

  inits <- function (data = NULL) {

    rngs       <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")
    past_N     <- data$past_N 
    past_count <- data$past_count 
    past_moon  <- data$past_moon

    log_past_count      <- log(past_count + 0.1)
    mean_log_past_count <- mean(log_past_count)
    sd_log_past_count   <- max(c(sd(log_past_count) * sqrt(2), 0.01))
    diff_log_past_count <- rep(NA, past_N - 1)

    log_bonus_max_past_count <- max(log(past_count * 1.2))

    for (i in 1:(past_N - 1)) {

      diff_count             <- log_past_count[i + 1] - log_past_count[i]
      diff_time              <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count / diff_time

    }

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate                          <- 0.1
    shape                         <- precision_diff_log_past_count * rate

    function(chain = chain){
      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = rnorm(1, mean_log_past_count, sd_log_past_count), 
            tau      = rgamma(1, shape = shape, rate = rate),
            r        = rnorm(1, 0, sqrt(1/10)),
            log_K    = log(rnorm(1, log_bonus_max_past_count, 0.1 * sqrt(log_bonus_max_past_count) )))

    }
  }
  jags_model <- "model {  

    # priors

    log_past_count           <- log(past_count + 0.1)
    mean_log_past_count      <- mean(log_past_count)
    sd_log_past_count        <- max(c(sd(log_past_count) * sqrt(2), 0.01))
    var_log_past_count       <- sd_log_past_count^2
    precision_log_past_count <- 1/(var_log_past_count)
    log_bonus_max_past_count <- max(log(past_count * 1.2))

    diff_count[1]          <- log_past_count[2] - log_past_count[1]
    diff_time[1]           <- past_moon[2] - past_moon[1] 
    diff_log_past_count[1] <- diff_count[1] / diff_time[1]

    for (i in 2:(past_N - 1)) {

      diff_count[i]          <- log_past_count[i + 1] - log_past_count[i]
      diff_time[i]           <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count[i] / diff_time[i]

    }    

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate                          <- 0.1
    shape                         <- precision_diff_log_past_count * rate

    mu    ~  dnorm(mean_log_past_count, precision_log_past_count); 
    tau   ~  dgamma(shape, rate); 
    r     ~  dnorm(0, 10);
    log_K ~  dnorm(log_bonus_max_past_count, 1 / (0.1 * log_bonus_max_past_count))
    K     <- exp(log_K) 
 
    # initial state

    X[1]          <- mu;
    pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
    count[1]      ~  dpois(max(c(exp(X[1]) - 0.1, 0.00001))) T(0, ntraps[1]);

    # through time

    for (i in 2:N) {

      # Process model

      predX[i]      <- X[i-1] * exp(r * (1 - (X[i - 1] / K)));
      checkX[i]     ~  dnorm(predX[i], tau); 
      X[i]          <- min(c(checkX[i], log(ntraps[i] + 1))); 
      pred_count[i] <- max(c(exp(X[i]) - 0.1, 0.00001));
   
      # observation model

      count[i] ~ dpois(max(c(exp(X[i]) - 0.1, 0.00001))) T(0, ntraps[i]); 

    }

  }"
  jags_ss(main = main, model_name = "jags_logistic", data_set = data_set, control_files = control_files,
          control_runjags = control_runjags, jags_model = jags_model,
          monitor = monitor, inits = inits, lag = lag, quiet = quiet, 
          verbose = verbose, arg_checks = arg_checks)
