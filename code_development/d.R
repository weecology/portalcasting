for(j in 1:200){
    mu        <-rnorm(1,mean_log_past_count, precision_log_past_count^(-1/2)); 
    tau       <-rgamma(1,shape, rate); 
    r_int     <-rnorm(1,0, 10^(-1/2));
    r_slope   <-rnorm(1,0, 1^(-1/2));
    log_K_int <-rnorm(1,log_bonus_max_past_count, (1 / (0.1 * log_bonus_max_past_count))^(-1/2))
    K_int     <- exp(log_K_int) 
 

X<-r<-K<-pred_count<-count<-predX<-checkX<-NA

    for (i in 1:N) {

      r[i] <- r_int + r_slope * warm_rain_three_months[i]
      K[i] <- K_int

    }

    # initial state

    X[1]          <- mu;
    pred_count[1] <- exp(X[1]);
    count[1]      <- rpois(1, exp(X[1]));

    # through time

    for (i in 2:N) {

      # Process model

      predX[i]      <- exp(X[i-1]) * exp(r[i] * (1 - (exp(X[i - 1]) / K[i])));
      checkX[i]     <-  rnorm(1, log(predX[i]), 0); 
      X[i]          <- min(max(c(checkX[i], -5)), log(ntraps[i] + 1))

      pred_count[i] <- exp(X[i]) 
   
      # observation model

      count[i] <- rpois(1, exp(X[i]))
    }
print(max(count))
}
plot(exp(X))