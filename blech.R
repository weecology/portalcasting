
  r ~ dnorm(0, 400)
  a <- 1
  c <- 0.001
  precision_log_delta <- 1e4

  log_last_N[1] <- log_last_N_1
  mean_log_delta[1] <- r - exp(log(c) + a * log_last_N[1])
  log_delta[1] ~ dnorm(mean_log_delta[1], precision_log_delta)
  N[1] <- exp(log_last_N[1] + log_delta[1]) 

  for(i in 2:nmoons){
    log_last_N[i] <- log(max(c(N[i - 1], min_val)))
    mean_log_delta[i] <- r - exp(log(c) + a * log_last_N[i])
    log_delta[i] ~ dnorm(mean_log_delta[i], precision_log_delta) 
    N[i] <- exp(log_last_N[i] + log_delta[i]) 
    count[i] ~ dpois(N[i]) T( , ntraps[i])
  }