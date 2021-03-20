#
#  jags mean
#
#   a single mean density
#
#  we have not yet added the forecasting
#

devtools::load_all()
main <- "./testing"

rngs <- function(){
  c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
    "base::Super-Duper", "base::Mersenne-Twister")
}


data_set <- "all"
s <- "DS"
ss <- "DS"
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

data <- list(pred_log_x1 = pred_log_x1,
             sd_pred_log_x1 = sd_pred_log_x1,
             precision_pred_log_x1 = precision_pred_log_x1,
             y = count,
             T = N,       # T = time steps
             c = ntraps)  # c = cap



jags_model <- "model {  

  log_x1 ~ dnorm(pred_log_x1, precision_pred_log_x1)

  x1 <- exp(log_x1)

  log_x[1] <- log_x1
  x[1] <- exp(log_x[1])
  y[1] ~ dpois(x[1]) T( , c[1])

  for(t in 2:T){
    log_x[t] <- log_x1
    x[t] <- exp(log_x[t])
    y[t] ~ dpois(x[t]) T( , c[t])
  }

}"


inits <- function(data = NULL, generators = rngs(), seeds = 1:1e6){

  function(chain = chain){

    list(.RNG.name = sample(generators, 1),
         .RNG.seed = sample(seeds, 1),
         log_x1 = rnorm(1, pred_log_x1, sd_pred_log_x1)     
    )

  }

}


monitor <- c("log_x1", "x")

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

plot(modd, vars = "log_x1")


plot(count)
points(modd_sum[2:NROW(modd_sum), 2], type = "l")






count
plot(count)

