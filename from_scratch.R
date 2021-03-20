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

species_in <- which(colnames(rodents_table) == s)
count <- rodents_table[moon_in , species_in]

count
plot(count)


min_val <- 1e-3

log_count <- log(count + min_val)
N_count <- length(count)
diff_log_count <- diff(log_count)
moon <- rodents_table$moon[moon_in]
nmoons <- length(moon)
ntraps <- rodents_table$ntraps[moon_in]
ntraps[is.na(ntraps)] <- 0

mu <- 1
r <- 0.001
sd_log_delta <- 0.0001
a <- 1
c <- 0.001

nmoons <- length(moon)

y <- N <- log_last_N <- log_delta <- mean_log_delta <- rep(0, nmoons)

N[1] <- mu
for(i in 2:nmoons){
  log_last_N[i] <- log(max(c(N[i - 1], min_val)))
  mean_log_delta[i] <- r - exp(log(c) + a * log_last_N[i])
  log_delta[i] <- rnorm(1, mean_log_delta[i], sd_log_delta)
  N[i] <- exp(log_last_N[i] + log_delta[i])
  y[i] <- rpois(1, N[i])
}


plot(N)
plot(y)

max_ntraps <- max(ntraps)


last_spot <- max(which(rodents_table$moon < start_moon))
last_moon <- rodents_table$moon[last_spot]
last_count <- rodents_table[last_spot, species_in]



data <- list(count = count, ntraps = ntraps, max_ntraps = max(ntraps),
             N = length(count), last_count = last_count)

jags_model <- "model{

  pred_log_mu <- log(last_count)
  tau_log_mu <- 100

  log_mu ~ dnorm(pred_log_mu, tau_log_mu)
  tau ~ dgamma(100, 1)

  X[1] <- exp(log_mu)
  mu <- exp(log_mu)
  count[1] ~ dpois(X[1]) T( , ntraps[1]);

  r ~ dunif(-0.5, 0.5)
  c <- 1

  for(i in 2:N){
    pred_X[i] <- X[i - 1]
    log_X_1[i] <- log(X[i - 1])
    log_X[i] ~ dnorm(log(pred_X[i] + r + exp(log_X_1[i])), tau)
    X[i] <- exp(log_X[i])
    count[i] ~ dpois(X[i]) T( , ntraps[i]);
  }

}"

monitor <- c("mu", "tau", "r")

inits <- function(data = NULL){
  rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")

  last_count <- data$last_count

  function(chain = chain){
    list(.RNG.name = sample(rngs, 1),
         .RNG.seed = sample(1:1e+06, 1),
          log_mu = rnorm(1, log(last_count), 1),
          tau = rgamma(1, shape = 100, rate = 1),
          r = runif(1, -0.5, 0.5))
  }
}


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

plot(modd)
xxx <- summary(modd)
head(xxx)
plot(xxx[,2])






  mean_log_N1 ~ dnorm(log_last_N_1, 100)
  N[1] <- exp(mean_log_N1)
  count[1] ~ dpois(N[1])

  for(i in 2:nmoons){
    mean_log_N[i] <- mean_log_N1
    N[i] <- exp(mean_log_N1)
    count[i] ~ dpois(N[i]) 
  }
