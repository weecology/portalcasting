#' @rdname prefab_model_functions
#'
#' @export
#'
jags_RW <- function(main = ".", data_set = "all",  
                    control_files = files_control(), 
                    control_runjags = runjags_control(), lag = NA, 
                    quiet = FALSE, verbose = FALSE){
  
  data_set <- tolower(data_set)
  messageq("  -jags_RW for ", data_set, quiet = quiet)
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)
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
    sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
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
    sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate <- 0.1
    shape <- precision_diff_log_past_count * rate

    mu ~ dnorm(mean_log_past_count, precision_log_past_count); 
    tau ~ dgamma(shape, rate); 
   
    # initial state
    X[1] <- mu;
    pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
    count[1] ~ dpois(max(c(exp(X[1]) - 0.1, 0.00001))) T(0, ntraps[1]);
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
  jags_ss(main = main, data_set = data_set, control_files = control_files,
          control_runjags = control_runjags, jags_model = jags_model,
          monitor = monitor, inits = inits, lag = lag, quiet = quiet, 
          verbose = verbose)
}


#' @title Run a single-species JAGS model
#'
#' @description Provides an API to \code{\link[runjags]{run.jags}} for
#'  single-species portalcasting models that requires the user only provide
#'  the JAGS model, initializer, monitor, and control list. 
#'
#' @details The data structure constructed internally, which is available to
#'  the \code{jags_model} and \code{inits} components, contains the following
#'  elements: \code{count}, \code{moon}, and \code{ntraps} (all three 
#'  include the forecast horizon as well, with \code{NA} for \code{count},
#'  \code{moon} extended as expected, and \code{ntraps} using the max number
#'  of traps); \code{N} (the length of the \code{count} time series); 
#'  \code{past_count}, \code{past_moon}, and \code{past_ntraps}, which 
#'  provide all of the historic data that preceded \code{start_moon}
#'  (only including samples that are present; all \code{NA} counts are 
#'  removed as are associated trap count sand moons); \code{past_N} (the 
#'  length of the \code{past_count} time series); and if desired, 
#'  \code{covariates} (table of all covariates for the span of \code{moon}).
#'  and \code{past_covariates} (table of all covariates for the span of 
#'  \code{past_moon}).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param data_set \code{character} value name of the rodent data set, such as 
#'  (\code{"all"} or \code{"controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @param lag \code{integer} (or integer \code{numeric}) of the lag time to
#'  use for the covariates.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages).
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param control_runjags \code{list} of arguments passed to 
#'  \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}
#'
#' @param jags_model \code{character} value of the model. See \code{Examples}.
#'
#' @param monitor \code{character} vector of the names of the variables to
#'  monitor. See \code{Examples} and \code{\link[runjags]{run.jags}}.
#'
#' @param inits \code{function}, \code{character} vector, or \code{list} of
#'  values (including random number generation components) for initializing
#'  runs of the model. See \code{Examples} and 
#'  \code{\link[runjags]{run.jags}}.
#'
#' @return \code{list} of [1] model metadata \code{list} (\code{"metadata"}), 
#'  [2] cast summary \code{data.frame} (\code{"cast_tab"}),
#'  [3] \code{list} of model fit objects (\code{"model_fits"}), and 
#'  [4] \code{list} of model cast objects (\code{"model_casts"}).
#'
#' @references 
#'  Denwood, M. J. 2016. runjags: an R package providing interface
#'  utilities, model templates, parallel computing methods and additional
#'  distributions for MCMC models in JAGS. Journal of Statistical
#'  Software, 71:9. 
#'  \href{https://www.jstatsoft.org/article/view/v071i09}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical 
#'  models using Gibbs Sampling. Proceedings of the 3rd International 
#.  Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X.
#'  \href{https://bit.ly/33aQ37Y}{URL}.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#' 
#'   monitor <- c("mu", "tau")
#'   inits <- function(data = NULL){
#'     rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
#'              "base::Super-Duper", "base::Mersenne-Twister")
#'     past_N <- data$past_N 
#'     past_count <- data$past_count 
#'     past_moon <- data$past_moon
#' 
#'     log_past_count <- log(past_count + 0.1)
#'     mean_log_past_count <- mean(log_past_count)
#'     sd_log_past_count <- max(c(sd(log_past_count) * sqrt(2), 0.01))
#'     diff_log_past_count <- rep(NA, past_N - 1)
#'     for(i in 1:(past_N - 1)){
#'       diff_count <- log_past_count[i + 1] - log_past_count[i]
#'       diff_time <- past_moon[i + 1] - past_moon[i] 
#'       diff_log_past_count[i] <- diff_count / diff_time
#'     }
#'     sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 
#'                                   0.01))
#'     var_diff_log_past_count <- sd_diff_log_past_count^2
#'     precision_diff_log_past_count <- 1/(var_diff_log_past_count)
#'     rate <- 0.1
#'     shape <- precision_diff_log_past_count * rate
#'
#'     function(chain = chain){
#'       list(.RNG.name = sample(rngs, 1),
#'            .RNG.seed = sample(1:1e+06, 1),
#'             mu = rnorm(1, mean_log_past_count, sd_log_past_count), 
#'             tau = rgamma(1, shape = shape, rate = rate))
#'     }
#'   }
#'   jags_model <- "model {  
#'     # priors
#'     log_past_count <- log(past_count + 0.1)
#'     mean_log_past_count <- mean(log_past_count)
#'     sd_log_past_count <- max(c(sd(log_past_count) * sqrt(2), 0.01))
#'     var_log_past_count <- sd_log_past_count^2
#'     precision_log_past_count <- 1/(var_log_past_count)
#' 
#'     diff_count[1] <- log_past_count[2] - log_past_count[1]
#'     diff_time[1] <- past_moon[2] - past_moon[1] 
#'     diff_log_past_count[1] <- diff_count[1] / diff_time[1]
#'     for(i in 2:(past_N - 1)){
#'       diff_count[i] <- log_past_count[i + 1] - log_past_count[i]
#'       diff_time[i] <- past_moon[i + 1] - past_moon[i] 
#'       diff_log_past_count[i] <- diff_count[i] / diff_time[i]
#'     }    
#'     sd_diff_log_past_count <- max(c(sd(diff_log_past_count) * sqrt(2), 
#'                                   0.01))
#'     var_diff_log_past_count <- sd_diff_log_past_count^2
#'     precision_diff_log_past_count <- 1/(var_diff_log_past_count)
#'     rate <- 0.1
#'     shape <- precision_diff_log_past_count * rate
#' 
#'     mu ~ dnorm(mean_log_past_count, precision_log_past_count); 
#'     tau ~ dgamma(shape, rate); 
#'    
#'     # initial state
#'     X[1] <- mu;
#'     pred_count[1] <- max(c(exp(X[1]) - 0.1, 0.00001));
#'     count[1] ~ dpois(max(c(exp(X[1]) - 0.1, 0.00001))) T(0,ntraps[1]);
#'     # through time
#'     for(i in 2:N) {
#'       # Process model
#'       predX[i] <- X[i-1];
#'       checkX[i] ~ dnorm(predX[i], tau); 
#'       X[i] <- min(c(checkX[i], log(ntraps[i] + 1))); 
#'       pred_count[i] <- max(c(exp(X[i]) - 0.1, 0.00001));
#'    
#'       # observation model
#'       count[i] ~ dpois(max(c(exp(X[i]) - 0.1, 0.00001))) T(0, ntraps[i]); 
#'     }
#'   }"
#'
#'   jags_ss(jags_model = jags_model, monitor = monitor, inits = inits)
#'  }
#'
#'
#' @export
#'
jags_ss <- function(main = ".", data_set = "all",  
                    control_files = files_control(),
                    control_runjags = runjags_control(),
                    jags_model = NULL, monitor = NULL, inits = NULL, lag = NA, 
                    quiet = FALSE, verbose = FALSE){
  
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)
  runjags.options(silent.jags = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)
  rodents_table <- read_rodents_table(main = main, data_set = data_set)

  metadata <- read_metadata(main = main, control_files = control_files)
  data_set_controls <- metadata$controls_r[[data_set]]
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  true_count_lead <- length(metadata$rodent_cast_moons)
  CL <- metadata$confidence_level

  if(covariatesTF){
    last_cc_moon <- max(metadata$covariate_cast_moons)
    covar <- read_covariates(main = main, control_files = control_files)
    covar_lag <- lag_covariates(covariates = covar, lag = lag, 
                                tail = TRUE)
    covar_moon_in <- which(covar_lag$moon >= start_moon & 
                           covar_lag$moon <= last_cc_moon)
    col_in <- which(colnames(covar_lag) != "source")
    covar_in <- as.matrix(covar_lag[covar_moon_in, col_in])

    past_covar_moon_in <- which(covar_lag$moon < start_moon)
    past_covar_in <- as.matrix(covar_lag[covar_moon_in, col_in])
  }

  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)
  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()
  for(i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)

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

    data <- list(count = count, ntraps = ntraps, N = length(count),
                 moon = moon, past_moon = past_moon, past_count = past_count,
                 past_ntraps = past_ntraps, past_N = length(past_count))
    if(covariatesTF){
      data[["covariates"]] <- covar_in
    }

    obs_pred_times <- metadata$rodent_cast_moons 
    obs_pred_times_spot <- obs_pred_times - metadata$start_moon

    name1 <- paste0("train: ", metadata$start_moon, " to ", metadata$end_moon)
    name2 <- paste0("test: ",  metadata$end_moon + 1, " to ", 
                    metadata$end_moon + metadata$lead)
    model_text <- paste0("model: ", name1, "; ", name2)

    if(control_runjags$cast_obs){
      obs_pred <- paste0("pred_count[", obs_pred_times_spot, "]")
      monitor <- c(monitor, obs_pred) 
    }

    mods[[i]] <- run.jags(model = jags_model, monitor = monitor, 
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
    if(control_runjags$cast_obs){
      nchains <- control_runjags$nchains
      vals <- mods[[i]]$mcmc[[1]]
      if(nchains > 1){
        for(j in 2:nchains){
          vals <- rbind(vals, mods[[i]]$mcmc[[j]])
        }
      }
      pred_cols <- grep("pred_count", colnames(vals))
      vals <- vals[ , pred_cols]
      point_forecast <- round(apply(vals, 2, mean), 3)
      HPD <- HPDinterval(as.mcmc(vals))
      lower_cl <- round(HPD[ , "lower"], 3)
      upper_cl <- round(HPD[ , "upper"], 3)
      casts_i <- data.frame(Point.Forecast = point_forecast,
                             lower_cl = lower_cl, upper_cl = upper_cl,
                             moon = obs_pred_times)
      colnames(casts_i)[2:3] <- paste0(c("Lo.", "Hi."), CL * 100)
      rownames(casts_i) <- NULL
      casts[[i]] <- casts_i

      cast_tab_i <- data.frame(cast_date = metadata$cast_date, 
                               cast_month = metadata$rodent_cast_months,
                               cast_year = metadata$rodent_cast_years, 
                               moon = metadata$rodent_cast_moons,
                               currency = data_set_controls$output,
                               model = "jags_RW", data_set = data_set, 
                               species = ss, 
                               estimate = point_forecast,
                               lower_pi = lower_cl, 
                               upper_pi = upper_cl,
                               start_moon = metadata$start_moon,
                               end_moon = metadata$end_moon,
                               stringsAsFactors = FALSE)
      cast_tab <- rbind(cast_tab, cast_tab_i)
    }
  }
  metadata <- update_list(metadata, models = "jags_RW",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)  
}

#' @title Create a control list for a runjags JAGS model run
#'
#' @description We leverage the \code{\link[runjags]{run.jags}} function in
#'  the runjags (Denwood 2016) package to run JAGS (Plummer 2003)
#'  models in portalcasting. That function has a number of control parameters
#'  that users may be interested in changing, and this function wraps those
#'  parameters with a few portalcasting-specific parameters into a control
#'  list for input into reusable functions like \code{\link{jags_ss}} and 
#'  specific model functions.
#'
#' @param nchains Non-negative \code{integer}-conformable value of the
#'  number of parallel chains to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param adapt Non-negative \code{integer}-conformable value of the
#'  number of adaptation steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param burnin Non-negative \code{integer}-conformable value of the
#'  number of burnin steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param sample Non-negative \code{integer}-conformable value of the
#'  number of sampling steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param thin Non-negative \code{integer}-conformable value of the
#'  thinning interval to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param modules \code{character} vector of external modules to add to JAGS.
#'  See \code{\link[runjags]{run.jags}}.
#'
#' @param method \code{character} value of the \code{\link[runjags]{run.jags}}
#'  method to use. Options include \code{"rjags"}, \code{"simple"}, 
#'  \code{"interruptible"}, \code{"parallel"}, \code{"rjparallel"}, 
#'  \code{"background"}, \code{"bgparallel"}, and \code{"snow"}.
#'  See \code{\link[runjags]{run.jags}}.
#'
#' @param factories \code{character} vector of factory modules to add to JAGS.
#'  See \code{\link[runjags]{run.jags}}.
#'
#' @param mutate A \code{function} or \code{list} (with the first element
#'  being a \code{function}) used to add variables to the posterior chain
#'  (rather than throughout sampling). See \code{\link[runjags]{run.jags}}.
#'
#' @param cast_obs \code{logical} value indicating if the observations should
#'  be forecast as well (should be kept as \code{TRUE} for the vast majority
#'  of uses).
#'
#' @param silent_jags \code{logical} value for quieting the output from the
#'  runjags function, including the underlying JAGS output. 
#'
#' @return \code{list} of controls. 
#'
#' @references 
#'  Denwood, M. J. 2016. runjags: an R package providing interface
#'  utilities, model templates, parallel computing methods and additional
#'  distributions for MCMC models in JAGS. Journal of Statistical
#'  Software, 71:9. 
#'  \href{https://www.jstatsoft.org/article/view/v071i09}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical 
#'  models using Gibbs Sampling. Proceedings of the 3rd International 
#.  Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X.
#'  \href{https://bit.ly/33aQ37Y}{URL}.
#'
#' @examples
#'  runjags_control()
#'
#' @export
#'
runjags_control <- function(nchains = 2, adapt = 1e4, burnin = 1e4, 
                            sample = 1e4, thin = 1, modules = "", 
                            method = "interruptible", factories = "", 
                            mutate = NA, cast_obs = TRUE, silent_jags = TRUE){
  
  list(nchains = nchains, adapt = adapt, burnin = burnin, sample = sample,
       thin = thin, modules = modules, method = method, factories = factories,
       mutate = mutate, cast_obs = cast_obs, silent_jags = silent_jags)
}
