library(runjags)
# run a model to calculate the intercept and slope of the expression
# y = m x + c, assuming normal observation errors for y:

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)

# Model in the JAGS format
model <- "model {
for(i in 1 : N){
	Y[i] ~ dnorm(true.y[i], precision);
	true.y[i] <- (m * X[i]) + c
}
m ~ dunif(-1000,1000)
c ~ dunif(-1000,1000)
precision ~ dexp(1)
}"

# Data and initial values in a named list format,
# with explicit control over the random number
# generator used for each chain (optional):
data <- list(X=X, Y=Y, N=length(X))
inits1 <- list(m=1, c=1, precision=1,
.RNG.name="base::Super-Duper", .RNG.seed=1)
inits2 <- list(m=0.1, c=10, precision=1,
.RNG.name="base::Wichmann-Hill", .RNG.seed=2)

## Not run: 
# Run the model and produce plots
results <- run.jags(model=model, monitor=c("m", "c", "precision"),
data=data, n.chains=2, method="rjags", inits=list(inits1,inits2))


library(arrow)

tf <- tempfile()
on.exit(unlink(tf))
write.csv(mtcars, file = tf)
df <- read_csv_arrow(tf)

library(httr)
GET("http://google.com/")