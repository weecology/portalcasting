# working here to develop the logistic w covariates model
# doing some basic data exploration and isolated regressions
# just to get a sense of where things land

devtools::load_all()
devtools::document()

main <- "~/portalcasting"
#setup_production(main = main)


dataset         = "dm_controls"  
                           settings        = directory_settings() 
                           control_runjags = runjags_control(silent_jags = FALSE) 
                           quiet           = FALSE 
                           verbose         = TRUE




  rodents_table <- read_rodents_table(main     = main,
                                      dataset = dataset, 
                                      settings = settings) 
  covariates    <- read_covariates(main     = main,
                                   settings = settings)

  metadata      <- read_metadata(main     = main,
                                 settings = settings)

  start_moon        <- metadata$time$start_moon
  end_moon          <- metadata$time$end_moon
  true_count_lead   <- length(metadata$time$rodent_cast_moons)
  confidence_level  <- metadata$confidence_level
  dataset_controls  <- metadata$dataset_controls[[dataset]]

  species <- species_from_table(rodents_tab = rodents_table, 
                                total       = TRUE, 
                                nadot       = TRUE)
  nspecies <- length(species)
  mods     <- named_null_list(species)
  casts    <- named_null_list(species)
  cast_tab <- data.frame()


  moon_in      <- which(rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon)
  moon         <- rodents_table[moon_in, "newmoonnumber"] 
  count        <- rodents_table[moon_in, species]

  warm_rain_three_months <- covariates$warm_precip_3_month
  ndvi_twelve_months     <- covariates$ndvi_12_month

  warm_rain_three_months <- warm_rain_three_months[weather$newmoonnumber >= start_moon & weather$newmoonnumber <= max(rodents_table$newmoonnumber)]
  ndvi_twelve_months     <- ndvi_twelve_months[ndvi$newmoonnumber >= start_moon & ndvi$newmoonnumber <= max(rodents_table$newmoonnumber)]



diff_log_count <- diff(log(count))

n <- length(count)
plot(diff_log_count, type = 'l')
plot((count)[-n], diff_log_count)

plot(density(diff_log_count, na.rm = TRUE))

plot(log10(warm_rain_three_months[-n] + 0.1), diff_log_count)

past_rain <- log10(warm_rain_three_months[-n] + 0.1)
nas <- is.na(diff_log_count)

diff_log_count_nnas <- diff_log_count[!nas]
past_rain_nnas <- past_rain[!nas]

sm <- (lm(diff_log_count_nnas ~ past_rain_nnas))

pred_r <- predict(sm, newdata = list(past_rain_nnas = past_rain))

plot(ndvi_twelve_months)


h<-
summarize_rodent_data(path = file.path(main, settings$subs$resources), clean = TRUE, level = "Plot", 
    type = "Rodents", plots = "Longterm", unknowns = FALSE, 
    shape = "crosstab", time = "newmoon", output = "abundance", 
 na_drop = FALSE, zero_drop = FALSE, min_traps = 1, 
    min_plots = 24, effort = FALSE, download_if_missing = TRUE, 
    quiet = FALSE, include_unsampled = FALSE)
h <- h[h$treatment == "control" & h$newmoonnumber >= start_moon & h$newmoonnumber <= max(rodents_table$newmoonnumber), ]

nmoons <- length(217:558)
maxval <- numeric(nmoons)
nvals <- numeric(nmoons)

for(i in 1:nmoons){
  maxval[i] <- max(h$DM[h$newmoonnumber == (217:558)[i]], na.rm = TRUE)
  nvals[i]  <- length(na.omit(h$DM[h$newmoonnumber == (217:558)[i]]))
}
estK <- maxval * nvals

nas <- is.na(estK)

estK_nnas <- estK[!nas]
ndvi_twelve_months_nnas <- ndvi_twelve_months[!nas]

mm <- lm(estK_nnas ~ ndvi_twelve_months_nnas)
summary(mm)
plot(ndvi_twelve_months, estK)
abline(mm)
pred_K <- predict(mm, newdata = list(ndvi_twelve_months_nnas = ndvi_twelve_months))


r <- rep(0.1, n)
K <- 35

n    <- length(count)
X    <- numeric(n)
X[1] <- count[1] 


for (i in 2:(n)) {
  X[i] <- X[i-1] * exp(pred_r[i-1] * (1 - (X[i - 1] / pred_K[i-1])))
}

plot(start_moon:558, count, type ='l')

points(217:558, X, type = 'l', col = 2)


plot(X, count, ylim = c(0, 40), xlim = c(0, 40))





n    <- length(count)
X    <- numeric(n)
X[1] <- count[1] 


for (i in 2:(n)) {
  X[i] <- count[i-1] * exp(pred_r[i-1] * (1 - (count[i - 1] / pred_K[i-1])))
}




    log_past_count           <- log(past_count + 0.1)
    mean_log_past_count      <- mean(log_past_count)
    sd_log_past_count        <- max(c(sd(log_past_count), 0.01))
    var_log_past_count       <- sd_log_past_count^2
    precision_log_past_count <- 1/(var_log_past_count)
    log_bonus_max_past_count <- max(log(past_count * 1.2))

    diff_count[1]          <- log_past_count[2] - log_past_count[1]
    diff_time[1]           <- past_moon[2] - past_moon[1] 
    diff_log_past_count[1] <- diff_count[1] / diff_time[1]

    for (i in 2:(past_N - 1)) {

      diff_count[i]          <- log_past_count[i + 1] - log_past_count[i]
      diff_time[i]           <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count[i] / diff_time[i]

    }    

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate                          <- 0.1
    shape                         <- precision_diff_log_past_count * rate

    mu    ~  dnorm(mean_log_past_count, precision_log_past_count); 
    tau   ~  dgamma(shape, rate); 
    r     ~  dnorm(0, 10);
    log_K ~  dnorm(log_bonus_max_past_count, 1 / (0.1 * log_bonus_max_past_count))
    K     <- exp(log_K) 
 

    # initial state

    X[1]          <- mu;
    pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
    count[1]      ~  dpois(max(c(exp(X[1]) - 0.1, 0.00001)))

    # through time

    for (i in 2:N) {

      # Process model

      predX[i]      <- X[i-1] * exp(r * (1 - (X[i - 1] / K)));
      checkX[i]     ~  dnorm(predX[i], tau); 
      X[i]          <- min(c(checkX[i], log(ntraps[i] + 1))); 
      pred_count[i] <- max(c(exp(X[i]) - 0.1, 0.00001));
   
      # observation model

      count[i] ~ dpois(max(c(exp(X[i]) - 0.1, 0.00001))) 

    }






  i <- 1

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)

    moon_in      <- which(rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon)
    past_moon_in <- which(rodents_table$newmoonnumber < start_moon)
    moon         <- rodents_table[moon_in, "newmoonnumber"] 
    moon         <- c(moon, metadata$time$rodent_cast_moons)
    past_moon    <- rodents_table[past_moon_in, "newmoonnumber"]

    ntraps           <- rodents_table[moon_in, "ntraps"] 
    na_traps         <- which(is.na(ntraps) == TRUE)
    ntraps[na_traps] <- 0
    cast_ntraps      <- rep(max(ntraps), true_count_lead)
    ntraps           <- c(ntraps, cast_ntraps)
    past_ntraps      <- rodents_table[past_moon_in, "ntraps"]

    species_in <- which(colnames(rodents_table) == s)
    count <- rodents_table[moon_in, species_in]

    cast_count  <- rep(NA, true_count_lead)
    count       <- c(count, cast_count)
    past_count  <- rodents_table[past_moon_in, species_in]
    no_count    <- which(is.na(past_count) == TRUE)

    if (length(no_count) > 0) {

      past_moon   <- past_moon[-no_count]
      past_count  <- past_count[-no_count]
      past_ntraps <- past_ntraps[-no_count]

    }







weather <- weather(level = "newmoon", fill = TRUE, horizon = 90, path = file.path(main, settings$subs$resources))
ndvi    <- ndvi(level = "newmoon", fill = TRUE, path = file.path(main, settings$subs$resources))


warm_rain_three_months <- weather$warm_precip
ndvi_twelve_months     <- filter(ndvi$ndvi, rep(1, 12), sides = 1)


past_warm_rain_three_months <- warm_rain_three_months[weather$newmoonnumber < start_moon]
past_ndvi_twelve_months     <- ndvi_twelve_months[ndvi$newmoonnumber < start_moon]

warm_rain_three_months <- warm_rain_three_months[weather$newmoonnumber >= start_moon & weather$newmoonnumber <= 558]
ndvi_twelve_months     <- ndvi_twelve_months[ndvi$newmoonnumber >= start_moon & ndvi$newmoonnumber <= 558]







plot(warm_rain_three_months, count)







names(data)

past_count
count

    data <- list(count       = count, 
                 ntraps      = ntraps, 
                 N           = length(count),
                 moon        = moon, 
                 past_moon   = past_moon, 
                 past_count  = past_count,
                 past_ntraps = past_ntraps, 
                 past_N      = length(past_count))


  monitor <- c("mu", "tau", "r", "K")

    past_N     <- data$past_N 
    past_count <- data$past_count 
    past_moon  <- data$past_moon

    log_past_count      <- log(past_count + 0.1)
    mean_log_past_count <- mean(log_past_count)
    sd_log_past_count   <- max(c(sd(log_past_count) * sqrt(2), 0.01))
    diff_log_past_count <- rep(NA, past_N - 1)

    log_bonus_max_past_count <- max(log(past_count * 1.2))

    for (i in 1:(past_N - 1)) {

      diff_count             <- log_past_count[i + 1] - log_past_count[i]
      diff_time              <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count / diff_time

    }

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate                          <- 0.1
    shape                         <- precision_diff_log_past_count * rate

    function(chain = chain){
      list(.RNG.name = sample(rngs, 1),
           .RNG.seed = sample(1:1e+06, 1),
            mu       = rnorm(1, mean_log_past_count, sd_log_past_count), 
            tau      = rgamma(1, shape = shape, rate = rate),
            r        = rnorm(1, 0, sqrt(1/10)),
            log_K    = log(rnorm(1, log_bonus_max_past_count, 0.1 * sqrt(log_bonus_max_past_count) )))

    }
  }

  jags_model <- "model {  

    # priors

    log_past_count           <- log(past_count + 0.1)
    mean_log_past_count      <- mean(log_past_count)
    sd_log_past_count        <- max(c(sd(log_past_count) * sqrt(2), 0.01))
    var_log_past_count       <- sd_log_past_count^2
    precision_log_past_count <- 1/(var_log_past_count)
    log_bonus_max_past_count <- max(log(past_count * 1.2))

    diff_count[1]          <- log_past_count[2] - log_past_count[1]
    diff_time[1]           <- past_moon[2] - past_moon[1] 
    diff_log_past_count[1] <- diff_count[1] / diff_time[1]

    for (i in 2:(past_N - 1)) {

      diff_count[i]          <- log_past_count[i + 1] - log_past_count[i]
      diff_time[i]           <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count[i] / diff_time[i]

    }    

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate                          <- 0.1
    shape                         <- precision_diff_log_past_count * rate

    mu    ~  dnorm(mean_log_past_count, precision_log_past_count); 
    tau   ~  dgamma(shape, rate); 
    r     ~  dnorm(0, 10);
    log_K ~  dnorm(log_bonus_max_past_count, 1 / (0.1 * log_bonus_max_past_count))
    K     <- exp(log_K) 
 
    # initial state

    X[1]          <- mu;
    pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
    count[1]      ~  dpois(max(c(exp(X[1]) - 0.1, 0.00001))) T(0, ntraps[1]);

    # through time

    for (i in 2:N) {

      # Process model

      predX[i]      <- X[i-1] * exp(r * (1 - (X[i - 1] / K)));
      checkX[i]     ~  dnorm(predX[i], tau); 
      X[i]          <- min(c(checkX[i], log(ntraps[i] + 1))); 
      pred_count[i] <- max(c(exp(X[i]) - 0.1, 0.00001));
   
      # observation model

      count[i] ~ dpois(max(c(exp(X[i]) - 0.1, 0.00001))) T(0, ntraps[i]); 

    }

  }"
