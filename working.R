working here to do some simple nls modeling of DMs in controls
follow bolker!
looks like from this that a general ricker model with the three params
is flexible enough that a fit of the data can generate trajectories like it
as well as others but still its a great starting point



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

plot(density(Rbar))

mod <- nls(Rbar ~ log(rm) - c * Nt1, start = list(rm = 0.5, c = 1))
summary(mod)


mod2 <- nls(Rbar ~ log(rm) - c * Nt1^a, 
            start = list(rm = 0.5, c = -1, a = -1))
summary(mod2)

nstep <- 40
ns <- ns2 <- ns3 <- rep(0, nstep)
ns[1] <- ns2[1] <- ns3[1] <- 1
for(i in 2:nstep){
  ns[i] <- ns[i-1] * exp(log(1.3) - (0.02)*ns[i-1]^(1))
  ns2[i] <- ns2[i-1] * exp(log(0.75) - (-2.5)*ns2[i-1]^(-0.88))
  ns3[i] <- ns3[i-1] * exp(log(0.22) - (-6.1)*ns3[i-1]^(-.52))
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
  #points(covar$moon[NROW(DM)]+1:12, out[,j],col=rgb(0,0,0.7,alpha=0.1), 
   #      type = "l",lwd=1)
  points(covar$moon[NROW(DM)]+1:12, out2[,j],col=rgb(0,0.6,0.2,alpha=0.1), 
         type = "l",lwd=1)
#  points(covar$moon[NROW(DM)]+1:12, out3[,j],col=rgb(0.8,0.1,0.6,alpha=0.1), 
 #        type = "l",lwd=1)
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



data <- list(counts = DM[,1])
parameters <- c(lambda = 1)

fun <- function(parameters, data){
  lambda <- parameters[1]
  counts <- data$counts
  lliks <- dpois(counts, lambda, log = TRUE) 
  -sum(lliks, na.rm = TRUE)
}

optim(parameters, fun, data = data)

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
solve(mod3$hessian)
fun(parameters,data)




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



mod3 <- optim(parameters, fun, data = data, hessian = TRUE)
mod3
solve(mod3$hessian)

