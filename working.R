#
#  working version of a logistic growth model
#

# just needs to get tidied up and integrated!

devtools::load_all()
main <- "./testing"




jags_Ricker <- function(main = ".", data_set = "all",  
                        control_files = files_control(), 
                        control_runjags = runjags_control(), lag = NA, 
                        quiet = FALSE, verbose = FALSE, arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  data_set <- tolower(data_set)
  messageq(paste0("  -jags_Ricker for ", data_set), quiet)
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)

# remove X
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
            mu = rnorm(1, pred_mu, sd_past_count),
            r = rnorm(1, mean_past_diff_rate, mean_past_diff_rate),
            K = rnorm(1, max_past_count, sd_past_count),
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

    pred_mu <- last_count + last_time_diff * mean_past_diff_rate

    rate <- 100
    shape <- precision_past_diff_rate * rate

    mu ~ dnorm(pred_mu, 0.5 * precision_past_count) T(0.1, max(ntraps)); 
    r ~ dnorm(mean_past_diff_rate, 0.5 * 1/(mean_past_diff_rate ^ 2));
    K ~ dnorm(max_past_count, 0.5 * precision_past_count) T(0.1, max(ntraps)); 
    tau ~ dgamma(shape, rate);
    a ~ dunif(0.5, 1.5);

    X[1] <- mu;
    count[1] ~ dpois(X[1]) T(0.1, ntraps[1]); 

    for(i in 2:N) {

      pred_X[i] <- X[i-1] * exp(r) * exp(-(r / K) * X[i-1] ^ a);
      X[i] ~ dnorm(pred_X[i], tau) T(0, max_ntraps);
      count[i] ~ dpois(X[i]) T( , ntraps[i]); 
    }
  }"

i<-2





s


modd <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = control_runjags$nchains, 
                          adapt = control_runjags$adapt, 
                          burnin = control_runjags$burnin, 
                          sample = control_runjags$sample, 
                          thin = control_runjags$thin, 
                          modules = control_runjags$modules, 
                          method = control_runjags$method, 
                          factories = control_runjags$factories, 
                          mutate = control_runjags$mutate, 
                          summarise = FALSE, plots = FALSE)









DM <- read_rodents_table(main, "DM_controls")
covar <- read_covariates(main)
covar
plot(covar$moon[1:NROW(DM)], DM[,4])

DMs <- na.omit(DM[,4])
Nt1 <- DMs[-length(DMs)]
Nt2 <- DMs[-1]
Rt2 <- log(Nt2/Nt1)
ts <- covar$moon[1:NROW(DM)][!is.na(DM[,4])]
t1 <- ts[-length(ts)]
t2 <- ts[-1]
t2_t1 <- t2 - t1
Rbar <- Rt2 / t2_t1

hist(Rbar)

quiet <- verbose <- FALSE
arg_checks <- TRUE
control_files <- files_control()
control_runjags <- runjags_control(adapt = 1000, burnin = 1000, sample = 1000)

lag <- NA
data_set <- "DM_controls"
  rodents_table <- read_rodents_table(main = main, data_set = data_set, 
                                      arg_checks = arg_checks)

  metadata <- read_metadata(main = main, control_files = control_files,
                            arg_checks = arg_checks)
  data_set_controls <- metadata$controls_r[[data_set]]
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  true_count_lead <- length(metadata$rodent_cast_moons)
  CL <- metadata$confidence_level


    ss <- s <- "DM"
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

past_diff_rate<-diff_count<-diff_time<-rep(NA, length(past_count) - 1)
  diff_count[1] <- past_count[2] - past_count[1]
  diff_time[1] <- past_moon[2] - past_moon[1]
  past_diff_rate[1] <- diff_count[1] / diff_time[1]
  for(i in 2:(past_N - 1)){
    diff_count[i] <- past_count[i] - past_count[i-1] 
    diff_time[i] <- past_moon[i] - past_moon[i-1]
    past_diff_rate[i] <- diff_count[i] / diff_time[i]
  }


    data <- list(count = count, ntraps = ntraps, max_ntraps = max(ntraps),
                 N = length(count), past_diff_rate = past_diff_rate,
                 moon = moon, past_moon = past_moon, past_count = past_count,
                 past_ntraps = past_ntraps, past_N = length(past_count))


model <- "model {  

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

  pred_mu <- last_count + last_time_diff * mean_past_diff_rate

  rate <- 100
  shape <- precision_past_diff_rate * rate

  mu ~ dnorm(pred_mu, 0.5 * precision_past_count) T(0.1, max(ntraps)); 
  r ~ dnorm(mean_past_diff_rate, 0.5 * 1/(mean_past_diff_rate ^ 2));
  K ~ dnorm(max_past_count, 0.5 * precision_past_count) T(0.1, max(ntraps)); 
  tau ~ dgamma(shape, rate);
  a ~ dunif(0.5, 1.5);



  X[1] <- mu;
  count[1] ~ dpois(X[1]) T(0.1, ntraps[1]); 

  for(i in 2:N) {

    pred_X[i] <- X[i-1] * exp(r) * exp(-(r / K) * X[i-1] ^ a);
    X[i] ~ dnorm(pred_X[i], tau) T(0, max_ntraps);
    count[i] ~ dpois(X[i]) T( , ntraps[i]); 
  }
}"



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
            mu = rnorm(1, pred_mu, sd_past_count),
            r = rnorm(1, mean_past_diff_rate, mean_past_diff_rate),
            K = rnorm(1, max_past_count, sd_past_count),
            a = runif(1, 0.5, 1.5),
            tau = rgamma(1, shape = shape, rate = rate))
    }
  }

#control_runjags <- runjags_control(nchains = 4, thin = 10)

modd <- run.jags(model = model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = control_runjags$nchains, 
                          adapt = control_runjags$adapt, 
                          burnin = control_runjags$burnin, 
                          sample = control_runjags$sample, 
                          thin = control_runjags$thin, 
                          modules = control_runjags$modules, 
                          method = control_runjags$method, 
                          factories = control_runjags$factories, 
                          mutate = control_runjags$mutate, 
                          summarise = FALSE, plots = FALSE)



xxx <- summary(modd)
head(xxx)
tail(xxx)


plot(modd, vars = c("mu", "r", "K", "a", "tau"))

plot(((xxx[6:NROW(xxx),2])), type = "l", ylim = c(0, 45))
points(count)






count




x <- rep(0, 20)

r <- 0.05318
K <- 28.17558
a <- 1.19433
tau <- 0.1591
mu <- 24

x[1] <- mu
for(i in 2:20){
  x[i] <- max(c(rnorm(1, 
                      x[i-1] * exp(r) * exp(-(r/K) * (x[i-1]^a)), 
                      sqrt(1/tau)), 
              0.01))
}
plot(x, type = "l", ylim = c(0, 30), col = 1)


r <- 0.0306
K <- 24.679
a <- 1.106
tau <- 0.334
mu <- 12.386

x[1] <- mu
for(i in 2:300){
  x[i] <- max(c(rnorm(1, 
                      x[i-1] * exp(r) * exp(-(r/K) * (x[i-1]^a)), 
                      sqrt(1/tau)), 
              0.01))
}
points(x, type = "l", col = 2)







count

  X[1] <- mu;
  count[1] ~ dpois(X[1]) T(0.1, ntraps[1]); 

  for(i in 2:N) {

    pred_X[i] <- X[i-1] * exp(r) * exp(-(r / K) * X[i-1] ^ a);
    X[i] ~ dnorm(pred_X[i], tau) T(0, max_ntraps);
    count[i] ~ dpois(X[i]) T( , ntraps[i]); 
  }

####################################################################
#
# good to work with but old
#

mod <- nls(Rbar ~ log(rm) - c * Nt1, start = list(rm = 0.5, c = 1))
summary(mod)


mod2 <- nls(Rbar ~ log(rm) - c * Nt1^a, 
            start = list(rm = 0.5, c = 1, a = -1))
summary(mod2)


data <- list(counts = DM[,1])
parameters <- c(rm = 0.75, c = -2.5, a = -0.88)

fun <- function(parameters, data){
  rm <- parameters[1]
  c <- parameters[2]
  a <- parameters[3]
  counts <- data$counts
  dens <- rep(0, length(counts))
  dens[1] <- counts[1] 
  for(i in 2:length(dens)){
    dens[i] <- dens[i-1] * exp(log(rm)-c*dens[i-1]^a)
  }
  lliks <- dpois(counts[-1], dens[-1], log = TRUE)
  -sum(lliks, na.rm = TRUE)
}

mod3 <- optim(parameters, fun, data = data, hessian = TRUE)
mod3





nstep <- 40
ns <- ns2 <- ns3 <- rep(0, nstep)
ns[1] <- ns2[1] <- ns3[1] <- 1
for(i in 2:nstep){
  ns[i] <- ns[i-1] * exp(log(1.3) - (0.02)*ns[i-1]^(1))
  ns2[i] <- ns2[i-1] * exp(log(0.75) - (-2.5)*ns2[i-1]^(-0.88))
  ns3[i] <- ns3[i-1] * exp(log(1.001) - (-2.0002)*ns3[i-1]^(-1.174))
}
plot(ns, col = rgb(0,0,0.7),ylim=c(0,20))
points(ns2, col = rgb(0,0.6,0.2))
points(ns3, col = rgb(0.8,0.1,0.6))


AIC(mod)
AIC(mod2)


  ns[i] <- ns[i-1] * exp(log(1.3) - (0.02)*ns[i-1]^(1))
  ns2[i] <- ns2[i-1] * exp(log(0.75) - (-2.5)*ns2[i-1]^(-0.88))


nfuture <- 12
nreps <- 100
out <- matrix(NA, nfuture, nreps)
for(j in 1:nreps){

  rm <- rnorm(nfuture, summary(mod)$parameters["rm", "Estimate"], 
                      summary(mod)$parameters["rm", "Std. Error"])
  c <- rnorm(nfuture, summary(mod)$parameters["c", "Estimate"], 
                      summary(mod)$parameters["c", "Std. Error"])
  n <- DM[NROW(DM), 4]
  for(i in 1:nfuture){
    n_predict <- n * exp(log(rm[i])-c[i]*n)
    out[i,j] <- n_predict
    n <- n_predict
  }
}



nfuture <- 12
nreps <- 100
out2 <- matrix(NA, nfuture, nreps)
for(j in 1:nreps){

  rm <- rnorm(nfuture, summary(mod2)$parameters["rm", "Estimate"],
                      summary(mod2)$parameters["rm", "Std. Error"])
  c <- rnorm(nfuture, summary(mod2)$parameters["c", "Estimate"], 
                      summary(mod2)$parameters["c", "Std. Error"])
  a <- rnorm(nfuture, summary(mod2)$parameters["a", "Estimate"], 
                      summary(mod2)$parameters["a", "Std. Error"])
  n <- DM[NROW(DM), 4]
  for(i in 1:nfuture){
    n_predict <- n * exp(log(rm[i])-c[i]*n^a[i])
    out2[i,j] <- n_predict
    n <- n_predict
  }
}

nfuture <- 12
nreps <- 100
out3 <- matrix(NA, nfuture, nreps)
for(j in 1:nreps){

  rm <- rnorm(nfuture, mod3$par["rm"],
                      sqrt(diag(solve(mod3$hessian)))["rm"])
  c <- rnorm(nfuture, mod3$par["c"], 
                      sqrt(diag(solve(mod3$hessian)))["c"])
  a <- rnorm(nfuture, mod3$par["a"], 
                      sqrt(diag(solve(mod3$hessian)))["a"])
  n <- DM[NROW(DM), 4]
  for(i in 1:nfuture){
    n_predict <- n * exp(log(rm[i])-c[i]*n^a[i])
    out3[i,j] <- n_predict
    n <- n_predict
  }
}

plot(covar$moon[1:NROW(DM)], DM[,4], xlim = c(400,550),ylim=c(0,45))


for(j in 1:nreps){
  points(covar$moon[NROW(DM)]+1:12, out[,j],col=rgb(0,0,0.7,alpha=0.1), 
         type = "l",lwd=1)
  points(covar$moon[NROW(DM)]+1:12, out2[,j],col=rgb(0,0.6,0.2,alpha=0.1), 
         type = "l",lwd=1)
  points(covar$moon[NROW(DM)]+1:12, out3[,j],col=rgb(0.8,0.1,0.6,alpha=0.1), 
         type = "l",lwd=1)
}

points(ns3, col = rgb(0.8,0.1,0.6))


nfuture <- 120
nreps <- 100
out <- matrix(NA, nfuture, nreps)
for(j in 1:nreps){

  r <- rnorm(nfuture, summary(mod)$parameters["r", "Estimate"], 
                      summary(mod)$parameters["r", "Std. Error"])
  C <- rnorm(nfuture, summary(mod)$parameters["c", "Estimate"], 
                      summary(mod)$parameters["c", "Std. Error"])
  a <- rnorm(nfuture, summary(mod)$parameters["a", "Estimate"], 
                      summary(mod)$parameters["a", "Std. Error"])
  n <- DM[rownames(DM) == 426, 1]
  n <- 15
  for(i in 1:nfuture){
    n_predict <- n * r[i] * exp(-C[i]*n^a[i])
    out[i,j] <- n_predict
    n <- n_predict
  }
}

plot(DM[,4], xlim = c(400,500),ylim=c(0,20))
for(j in 1:nreps){
  points(426+1:nfuture, out[,j],col=rgb(0,0,0.8,alpha=0.01), 
  type = "l")
}






nfuture <- 12
nreps <- 1000
out <- matrix(NA, nfuture, nreps)
for(j in 1:nreps){

  r <- rnorm(nfuture, summary(mod)$parameters["r", "Estimate"], 
                      summary(mod)$parameters["r", "Std. Error"])
  C <- rnorm(nfuture, summary(mod)$parameters["C", "Estimate"], 
                      summary(mod)$parameters["C", "Std. Error"])
  a <- rnorm(nfuture, summary(mod)$parameters["a", "Estimate"], 
                      summary(mod)$parameters["a", "Std. Error"])

  for(i in 1:nfuture){
    n <- DM[rownames(DM) == 400 + i - 1, 1]
    n_predict <- n * r[i] * exp(-C[i]*n^a[i])
    out[i,j] <- n_predict
  }
}

plot(DM[,1], xlim = c(400,550),ylim=c(0,80))
for(j in 1:nreps){
  points(400+1:nfuture, out[,j],col=rgb(0,0,0.8,alpha=0.01), 
  type = "l")
}


r<-rnorm(500, summary(mod)$parameters["r", "Estimate"], 
              summary(mod)$parameters["r", "Std. Error"])
C<-rnorm(500, summary(mod)$parameters["C", "Estimate"], 
              summary(mod)$parameters["C", "Std. Error"])
a<-rnorm(500, summary(mod)$parameters["a", "Estimate"], 
              summary(mod)$parameters["a", "Std. Error"])






set.seed(12)
set.seed(33)
n <- 1
for(i in 1:500){
n[i+1] <- n[i] * r[i] * exp(-C[i]*n[i]^a[i])
}
plot(n)






parameters <- c(rm = 0.75, c = -2.5, a = -0.88, sd = 0.1)

fun <- function(parameters, data){
  rm <- parameters[1]
  c <- parameters[2]
  a <- parameters[3]
  cse <- parameters[4]
  if(cse<1e-5){
    return(Inf)
  }
  counts <- data$counts
  dens <- rep(0, length(counts))
  dens[1] <- counts[1] 
  for(i in 2:length(dens)){
    dens[i] <- dens[i-1] * exp(log(rm)-c*dens[i-1]^a)
  }
  lliks <- dnorm(counts[-1], dens[-1], cse, log = TRUE)
  -sum(lliks, na.rm = TRUE)
}



mod4 <- optim(parameters, fun, data = data, hessian = TRUE)
mod4
solve(mod4$hessian)

