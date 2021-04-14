#
#  jags modeling
#
#   building up piece by piece
#
#    1. mean overall density, not fluctuating
#    2. random walk overall density
#    3. logistic growth (r and K)
#


devtools::load_all()
main <- "./testing"

rngs <- function(){
  c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
    "base::Super-Duper", "base::Mersenne-Twister")
}


data_set <- "controls"
s <- "PP"
ss <- "PP"
arg_checks <- TRUE

control_files <- files_control() 
control_runjags <- runjags_control(nchains = 4)
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


start_past_moon <- 1
min_count <- 0.001


moon_in <- which(rodents_table$moon >= start_moon & 
                     rodents_table$moon <= end_moon)
species_in <- which(colnames(rodents_table) == s)
count <- rodents_table[moon_in, species_in]
ntraps <- rodents_table$ntraps[moon_in]
ntraps[is.na(ntraps)] <- 0 
moon <- rodents_table$moon[moon_in] 
N <- length(count)

past_moon_in <- which(rodents_table$moon >= start_past_moon & 
                     rodents_table$moon < start_moon)
past_count <- rodents_table[past_moon_in, species_in]
past_ntraps <- rodents_table$ntraps[past_moon_in] 
past_moon <- rodents_table$moon[past_moon_in] 

na_past_count <- is.na(past_count)
past_count <- past_count[!na_past_count]
past_ntraps <- past_ntraps[!na_past_count]
past_moon <- past_moon[!na_past_count]
past_N <- length(past_count)

min_count_sd <- 0.01

log_past_count <- log(past_count + min_count)
mean_log_past_count <- mean(log_past_count)
sd_log_past_count <- max(c(sd(log_past_count), min_count_sd))
precision_log_past_count <- 1/(sd_log_past_count ^ 2) 

precision_scale <- 0.5

pred_log_x1 <- mean_log_past_count
sd_pred_log_x1 <- sd_log_past_count
precision_pred_log_x1 <- precision_scale * precision_log_past_count


min_diff_sd <- 0.01
diff_log_past_count <- diff(log_past_count)
nneg_diff_log_past_count <- diff_log_past_count[diff_log_past_count >= 0] 
mean_diff_log_past_count <- mean(nneg_diff_log_past_count)
sd_diff_log_past_count <- max(c(sd(diff_log_past_count), min_diff_sd))
precision_diff_log_past_count <- 1/(sd_diff_log_past_count ^ 2) 


rate_delta <- 0.1
shape_delta <- rate_delta * precision_scale * precision_diff_log_past_count

sd_r <- 0.1
precision_r <- 1/sd_r^2

max_K <- max(ntraps) * 0.8
min_K <- 1

max_c 


max_past_count <- max(c(max(past_count), min_count))
max_log_past_count <- max(log(pmax(past_count, min_count)))
max_c <- max(ntraps)
log_max_c <- log(max(ntraps))
log_min_count <- log(min_count)


data <- list(pred_log_x1 = pred_log_x1,
             sd_pred_log_x1 = sd_pred_log_x1,
             precision_pred_log_x1 = precision_pred_log_x1,

             mean_diff_log_past_count = mean_diff_log_past_count,
             sd_diff_log_past_count = sd_diff_log_past_count,
             precision_diff_log_past_count = precision_diff_log_past_count,

             shape_delta = shape_delta,
             rate_delta = rate_delta,

             y = count,
             T = N,       # T = time steps
             c = ntraps,  # c = cap

             max_past_count = max_past_count,
             max_log_past_count = max_log_past_count,
             sd_log_past_count = sd_log_past_count,
             precision_log_past_count = precision_log_past_count,
             min_K = min_K, 
             max_K = max_K,
             max_c = max(ntraps),

             min_count = min_count,
             log_max_c = log_max_c,
             log_min_count = log_min_count,

             precision_r = precision_r,
             sd_r = sd_r)



jags_model <- "model {  

  log_x1 ~ dnorm(pred_log_x1, 
                 precision_pred_log_x1) T(log_min_count, log_max_c)

  tau_delta ~ dgamma(shape_delta, rate_delta) T(1e-100, )

  r ~ dnorm(mean_diff_log_past_count, precision_r)

  K ~ dlnorm(max_log_past_count, precision_log_past_count) T(min_K, max_K)


  log_x[1] <- min(log_x1, log(max_c))
  x[1] <- max(c(exp(log_x[1]), min_count))
  y[1] ~ dpois(x[1]) T( , c[1])

  for(t in 2:T){

    pred_log_x[t] <- log_x[t - 1] + r - (r / K) * x[t - 1]
    check_log_x[t] ~ dnorm(pred_log_x[t], tau_delta)
    log_x[t] <- min(c(check_log_x[t], log(max_c)))

    x[t] <- max(c(exp(log_x[t]), min_count))
    y[t] ~ dpois(x[t]) T( , c[t])

  }
}"


inits <- function(data = NULL, generators = rngs(), seeds = 1:1e6){

  function(chain = chain){

    list(.RNG.name = sample(generators, 1),
         .RNG.seed = sample(seeds, 1),
         log_x1 = max(c(min(c(rnorm(1, data$pred_log_x1, data$sd_pred_log_x1),
                            data$log_max_c)),
                        data$log_min_count)),
         tau_delta = max(c(rgamma(1, shape = data$shape_delta, 
                                  rate = data$rate_delta),
                           1e-100)),
         r = rnorm(1, data$mean_diff_log_past_count, 
                      data$sd_r),
         K = runif(1, data$max_past_count, 
                      min(c(5 * max(c(data$max_past_count, 1)), data$max_c)))
    )

  }

}


monitor <- c("log_x1", "tau_delta", "r", "K", "x")

modd <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = control_runjags$nchains,
                          adapt = 1e3,
                          burnin = 1e3,
                          sample = 1e3,
                          thin = control_runjags$thin, 
                          modules = control_runjags$modules, 
                          method = control_runjags$method, 
                          factories = control_runjags$factories, 
                          mutate = control_runjags$mutate, 
                          summarise = FALSE, plots = FALSE)

modd_sum <- summary(modd)
head(modd_sum)

plot(modd, vars = c("log_x1", "tau_delta", "r", "K"))

windows()
plot(count)
points(modd_sum[5:NROW(modd_sum), 2], type = "l")



