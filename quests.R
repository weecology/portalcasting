working here to do some simple nls modeling of DMs in controls
follow bolker!
looks like from this that a general ricker model with the three params
is flexible enough that a fit of the data can generate trajectories like it
as well as others but still its a great starting point


DM <- read_rodents_table(main, "controls")[, "DM", drop = FALSE]
covar <- read_covariates(main)
covar
plot(covar$moon[1:NROW(DM)], DM[,1])


DM

(log(n_t) - log(n_t-1)) / (t - (t-1))

raw_change <- diff(log(na.omit(DM[,1])))
steps_change <- diff(covar$moon[1:NROW(DM)][!is.na(DM[,1])])
change <- raw_change/steps_change
hist(change)

plot(change)

nold <- as.numeric(na.omit(DM[-NROW(DM),1]))

plot(nold, change)

mod <- nls(change ~ log(r) - C * nold^a, start = list(r = 0.5, C = 1, a = -1))
summary(mod)
plot(mod)
xp <- seq(1, 40, 0.1)
yp <- predict(mod, newdata = list(nold = xp))

plot(nold, change)
points(xp, yp, type = "l")

set.seed(12)
set.seed(33)

r<-rnorm(500, summary(mod)$parameters["r", "Estimate"], 
              summary(mod)$parameters["r", "Std. Error"])
C<-rnorm(500, summary(mod)$parameters["C", "Estimate"], 
              summary(mod)$parameters["C", "Std. Error"])
a<-rnorm(500, summary(mod)$parameters["a", "Estimate"], 
              summary(mod)$parameters["a", "Std. Error"])


n <- 1
for(i in 1:500){
n[i+1] <- n[i] * r[i] * exp(-C[i]*n[i]^a[i])
}
plot(n)