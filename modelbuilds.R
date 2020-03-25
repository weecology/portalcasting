devtools::load_all()
main <- "~/builder"
setup_dir(main = main)


read_rodents(main)


   monitor <- c("mu", "tau")
   inits <- function(data = NULL){
     rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")
     past_N <- data$past_N 
     past_count <- data$past_count 
     past_moon <- data$past_moon
 
     log_past_count <- log(past_count + 0.1)
     mean_log_past_count <- mean(log_past_count)
     sd_log_past_count <- max(c(sd(log_past_count) * sqrt(2), 0.01))
     diff_log_past_count <- rep(NA, past_N - 1)
     for(i in 1:(past_N - 1)){
       diff_count <- log_past_count[i + 1] - log_past_count[i]
       diff_time <- past_moon[i + 1] - past_moon[i] 
       diff_log_past_count[i] <- diff_count / diff_time
     }
     sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 
                                   0.01))
     var_diff_log_past_count <- sd_diff_log_past_count^2
     precision_diff_log_past_count <- 1/(var_diff_log_past_count)
     rate <- 0.1
     shape <- precision_diff_log_past_count * rate

     function(chain = chain){
       list(.RNG.name = sample(rngs, 1),
            .RNG.seed = sample(1:1e+06, 1),
             mu = rnorm(1, mean_log_past_count, sd_log_past_count), 
             tau = rgamma(1, shape = shape, rate = rate))
     }
   }
   jags_model <- "model {  
     # priors
     log_past_count <- log(past_count + 0.1)
     mean_log_past_count <- mean(log_past_count)
     sd_log_past_count <- max(c(sd(log_past_count) * sqrt(2), 0.01))
     var_log_past_count <- sd_log_past_count^2
     precision_log_past_count <- 1/(var_log_past_count)
 
     diff_count[1] <- log_past_count[2] - log_past_count[1]
     diff_time[1] <- past_moon[2] - past_moon[1] 
     diff_log_past_count[1] <- diff_count[1] / diff_time[1]
     for(i in 2:(past_N - 1)){
       diff_count[i] <- log_past_count[i + 1] - log_past_count[i]
       diff_time[i] <- past_moon[i + 1] - past_moon[i] 
       diff_log_past_count[i] <- diff_count[i] / diff_time[i]
     }    
     sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 
                                   0.01))
     var_diff_log_past_count <- sd_diff_log_past_count^2
     precision_diff_log_past_count <- 1/(var_diff_log_past_count)
     rate <- 0.1
     shape <- precision_diff_log_past_count * rate
 
     mu ~ dnorm(mean_log_past_count, precision_log_past_count); 
     tau ~ dgamma(shape, rate); 
    
     # initial state
     X[1] <- mu;
     pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
     count[1] ~ dpois(max(c(exp(X[1]) - 0.1, 0.00001))) T(0,ntraps[1]);
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

   jags_ss(jags_model = jags_model, monitor = monitor, inits = inits)
  }

