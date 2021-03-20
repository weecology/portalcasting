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


# add to metadata

start_past_moon <- 200
min_val <- 1e-3

# make these into functions too

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

min_sd <- 1e-3

log_past_count <- log(past_count + min_val)
mean_log_past_count <- mean(log_past_count)
sd_log_past_count <- max(c(sd(log_past_count), min_sd))
precision_log_past_count <- 1/(sd_log_past_count ^ 2) 

precision_scale <- 0.5

pred_log_x1 <- mean_log_past_count
sd_pred_log_x1 <- sd_log_past_count
precision_pred_log_x1 <- precision_scale * precision_log_past_count

diff_log_past_count <- diff(log_past_count)
mean_diff_log_past_count <- mean(diff_log_past_count)
sd_diff_log_past_count <- max(c(sd(diff_log_past_count), min_sd))
precision_diff_log_past_count <- 1/(sd_diff_log_past_count ^ 2) 


rate_delta <- 0.1
shape_delta <- rate_delta * precision_scale * precision_diff_log_past_count


data <- list(pred_log_x1 = pred_log_x1,
             sd_pred_log_x1 = sd_pred_log_x1,
             precision_pred_log_x1 = precision_pred_log_x1,

             mean_diff_log_past_count = mean_diff_log_past_count,

             shape_delta = shape_delta,
             rate_delta = rate_delta,

             y = count,
             T = N,       # T = time steps
             c = ntraps,  # c = cap

             log_max_past_count = log(max(past_count) + min_val),
             sd_log_past_count = sd_log_past_count,
             max_c = max(ntraps),
             min_val = min_val)



jags_model <- "model {  

  log_x1 ~ dnorm(pred_log_x1, precision_pred_log_x1)

  tau_delta ~ dgamma(shape_delta, rate_delta)

  r ~ dnorm(mean_diff_log_past_count, 1e4)

  K ~ dlnorm(log_max_past_count, sd_log_past_count)


  log_x[1] <- min(log_x1, log(max_c))
  x[1] <- max(c(exp(log_x[1]), min_val))
  y[1] ~ dpois(x[1]) T( , c[1])

  for(t in 2:T){

    pred_log_x[t] <- log_x[t - 1] + r - (r / K) * x[t - 1]
    check_log_x[t] ~ dnorm(pred_log_x[t], tau_delta)
    log_x[t] <- min(c(check_log_x[t], log(max_c)))

    x[t] <- max(c(exp(log_x[t]), min_val))
    y[t] ~ dpois(x[t]) T( , c[t])

  }
}"


inits <- function(data = NULL, generators = rngs(), seeds = 1:1e6){

  function(chain = chain){

    list(.RNG.name = sample(generators, 1),
         .RNG.seed = sample(seeds, 1),
         log_x1 = max(c(rnorm(1, data$pred_log_x1, data$sd_pred_log_x1),
                        log(data$max_c))),
         tau_delta = rgamma(1, shape = data$shape_delta, 
                               rate = data$rate_delta),
         r = rnorm(1, data$mean_diff_log_past_count, 1e-5),
         K = max(c(rlnorm(1, data$log_max_past_count, data$sd_log_past_count),
                   data$max_c))     
    )

  }

}


monitor <- c("log_x1", "tau_delta", "r", "K", "x")

modd <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits(data), data = data, 
                          n.chains = 4,
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


plot(count)
points(modd_sum[5:NROW(modd_sum), 2], type = "l")






count
plot(count)

