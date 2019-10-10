jags_RW <- function(main = ".", data_set = "all",  
                    control_files = files_control(), 
                    control_runjags = runjags_control(), lag = NA, 
                    quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_set <- tolower(data_set)
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)

  monitor <- c("mu", "tau")
  inits <- function(chain = chain){
    rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
              "base::Super-Duper", "base::Mersenne-Twister")
    list(.RNG.name = sample(rngs, 1),
         .RNG.seed = sample(1:1e+06, 1),
          mu = rnorm(1, 0, 1),
          tau = rgamma(1, shape = 0.1, rate = 0.1))
  }
  jags_model <- "model {  
    # priors
    mu ~ dnorm(log(1), 0.25); 
    tau ~ dgamma(0.1,0.1); 
   
    # initial state
    X[1] <- mu;
    pred_count[1] <- exp(X[1]);
    count[1] ~ dpois(exp(X[1])) T(0,ntraps[1]);

    # through time
    for(i in 2:N) {
      # Process model
      predX[i] <- X[i-1];
      checkX[i] ~ dnorm(predX[i], tau); 
      X[i] <- min(c(checkX[i], log(ntraps[i] + 1))); 
      pred_count[i] <- exp(X[i]);
   
      # observation model
      count[i] ~ dpois(exp(X[i])) T(0, ntraps[i]); 
    }
  }"

  jags_ss(main = main, data_set = data_set, control_files = control_files,
          control_runjags = control_runjags, jags_model = jags_model,
          monitor = monitor, inits = inits, lag = lag, quiet = quiet, 
          verbose = verbose, arg_checks = arg_checks)
}


#' @title Run a single-species JAGS model
#'
#' @description Provides an API to \code{\link[runjags]{run.jags}} for
#'  single-species portalcasting models that requires the user only provide
#'  the JAGS model, initializer, monitor, and control list. 
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. 
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
#'   monitor <- c("mu", "tau")
#'   inits <- function(chain = chain){
#'     rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
#'               "base::Super-Duper", "base::Mersenne-Twister")
#'     list(.RNG.name = sample(rngs, 1),
#'          .RNG.seed = sample(1:1e+06, 1),
#'           mu = rnorm(1, 0, 1),
#'           tau = rgamma(1, shape = 0.1, rate = 0.1))
#'   }
#'   jags_model <- "model {  
#'     # priors
#'     mu ~ dnorm(log(1), 0.25); 
#'     tau ~ dgamma(0.1,0.1); 
#'   
#'     # initial state
#'     X[1] <- mu;
#'     pred_count[1] <- exp(X[1]);
#'     count[1] ~ dpois(exp(X[1])) T(0,ntraps[1]);
#' 
#'     # through time
#'     for(i in 2:N) {
#'       # Process model
#'       predX[i] <- X[i-1];
#'       checkX[i] ~ dnorm(predX[i], tau); 
#'       X[i] <- min(c(checkX[i], log(ntraps[i] + 1))); 
#'       pred_count[i] <- exp(X[i]);
#'   
#'       # observation model
#'       count[i] ~ dpois(exp(X[i])) T(0, ntraps[i]); 
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
                    quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  covariatesTF <- ifelse(is.na(lag), FALSE, TRUE)
  runjags.options(silent.jags = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)
  rodents_table <- read_rodents_table(main = main, data_set = data_set, 
                                      arg_checks = arg_checks)

  metadata <- read_metadata(main = main, control_files = control_files,
                            arg_checks = arg_checks)
  data_set_controls <- metadata$controls_r[[data_set]]
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  last_cc_moon <- max(metadata$covariate_cast_moons)
  if(covariatesTF){
    covar <- read_covariates(main = main, control_files = control_files,
                             arg_checks = arg_checks)
    covar_lag <- lag_covariates(covariates = covar, lag = lag, 
                                tail = TRUE, arg_checks = arg_checks)
    moon_in <- which(covar_lag$moon >= start_moon & 
                     covar_lag$moon <= last_cc_moon)
    col_in <- which(colnames(covar_lag) != "source")
    covar_in <- as.matrix(covar_lag[moon_in, col_in])
  }
  moon_in <- which(rodents_table$moon >= start_moon & 
                   rodents_table$moon <= end_moon)
  moon <- rodents_table[moon_in, "moon"] 
  moon <- c(moon, metadata$rodent_cast_moons)
  ntraps <- rodents_table[moon_in, "ntraps"] 
  ntraps[which(is.na(ntraps) == TRUE)] <- 0
  true_lead <- length(metadata$rodent_cast_moons)
  cast_ntraps <- rep(max(ntraps), true_lead)
  ntraps <- c(ntraps, cast_ntraps)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)
  mods <- named_null_list(species)
  for(i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq(paste0("   -", ss), !verbose)
    species_in <- which(colnames(rodents_table) == s)
    count <- rodents_table[moon_in, species_in]
    if(sum(count, na.rm = TRUE) == 0){
      next()
    }

    cast_count <- rep(NA, true_lead)
    count <- c(count, cast_count)
 
    prior_moon <- which(rodents_table$moon < start_moon)
    data <- list(count = count, ntraps = ntraps, N = length(count))
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
                          inits = inits, data = data, 
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
  }
  metadata <- update_list(metadata, models = "AutoArima",
                              data_sets = data_set,
                              controls_r = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata,
       #cast_tab = cast_tab, 
       model_fits = mods)#, 
       #       model_casts = casts)  
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. 
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
runjags_control <- function(nchains = 1, adapt = 1e2, burnin = 5e2, 
                            sample = 1e2, thin = 1, modules = "", 
                            method = "interruptible", factories = "", 
                            mutate = NA, cast_obs = TRUE, silent_jags = TRUE,
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  list(nchains = nchains, adapt = adapt, burnin = burnin, sample = sample,
       thin = thin, modules = modules, method = method, factories = factories,
       mutate = mutate, cast_obs = cast_obs, silent_jags = silent_jags)
}
