            mu       = rnorm(1, log_mean_past_count, 1)
            sigma    = runif(1, 0, 10)
            log_r    = rnorm(1, 0, sqrt(1/10))
            log_K    = log(rnorm(1, log_max_past_count, 0.1))

    tau   <- pow(sigma, -1/2)
    r     <- exp(log_r)
    K     <- exp(log_K) 
 
X <- log_X <- count <- pred_X <- pred_log_X <- NA


    log_X[1]      <- mu
    X[1]          <- exp(log_X[1])
    count[1]      <- rpois(1, X[1]) 

i<-2

    # through time

    for(i in 2:N) {

      # Process model

      pred_X[i]     <- X[i-1] + X[i-1] * r * (1 - X[i-1] / K)
      pred_log_X[i] <- log(pred_X[i])
      log_X[i]      <- rnorm(1, pred_log_X[i], sigma)
      X[i]          <- exp(log_X[i])
   

      # observation model

      count[i] ~ rpois(1, X[i])

    }

  }"

