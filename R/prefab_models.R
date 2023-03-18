
fit_runjags <- function (abundance, model, control_runjags, metadata, covariates) {

  monitor    <- runjags_monitor(model = model)  
  inits      <- runjags_inits(model = model)  
  jags_model <- runjags_model(model = model)  
  data       <- runjags_data(model = model,
                             abundance = abundance,
                             metadata  = metadata,
                             covariates = covariates)  

  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  model_fit <- run.jags(model     = jags_model, 
                        monitor   = monitor, 
                        inits     = inits(data), 
                        data      = data, 
                        n.chains  = control_runjags$nchains, 
                        adapt     = control_runjags$adapt, 
                        burnin    = control_runjags$burnin, 
                        sample    = control_runjags$sample, 
                        thin      = control_runjags$thin, 
                        modules   = control_runjags$modules, 
                        method    = control_runjags$method, 
                        factories = control_runjags$factories, 
                        mutate    = control_runjags$mutate, 
                        summarise = TRUE, 
                        plots     = FALSE)


}

forecast.runjags <- function (object, h, level) {

  nchains <- length(object$mcmc)
  vals    <- object$mcmc[[1]]

  if (nchains > 1) {

    for (j in 2:nchains) {

      vals <- rbind(vals, object$mcmc[[j]])

    }

  }

  pred_cols       <- grep("X", colnames(vals))
  vals            <- vals[ , pred_cols]
  HPD             <- HPDinterval(obj = as.mcmc(vals), prob = level)
  out             <- list(mean  = as.ts(round(apply(vals, 2, mean), 3)),
                          lower = as.ts(round(HPD[ , "lower", drop = FALSE], 3)), 
                          upper = as.ts(round(HPD[ , "upper", drop = FALSE], 3)))
  out$level       <- level
  structure(out, class = "forecast")

}

runjags_monitor <- function (model = NULL) {

  if (model == "jags_RW") {

    out <- c("mu", "sigma", paste0("X[", metadata$time$forecast_newmoons - metadata$time$historic_start_newmoon + 1, "]"))

  } 
  
  if (model == "jags_logistic") {

    out <- c("mu", "sigma", "r", "log_K", paste0("X[", metadata$time$forecast_newmoons - metadata$time$historic_start_newmoon + 1, "]"))
  
  }

  if (model == "jags_logistic_covariates") {

    out <- c("mu", "sigma", "r_int", "r_slope", "log_K_int", "log_K_slope", paste0("X[", metadata$time$forecast_newmoons - metadata$time$historic_start_newmoon + 1, "]"))
  
  }

  if (model == "jags_logistic_competition") {

    out <- c("mu", "sigma", "r_int", "log_K_int", "log_K_slope", paste0("X[", metadata$time$forecast_newmoons - metadata$time$historic_start_newmoon + 1, "]"))
  
  }

  if (model == "jags_logistic_competition_covariates") {

    out <- c("mu", "sigma", "r_int", "r_slope", "log_K_int", "log_K_slope_NDVI", "log_K_slope_DO", paste0("X[", metadata$time$forecast_newmoons - metadata$time$historic_start_newmoon + 1, "]"))
  
  }

  out

}

runjags_inits <- function (model = NULL) {

  rngs  <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")


  if (model == "jags_RW") {

    out <- function (data = NULL) {

             function (chain = chain) {

               list(.RNG.name = sample(rngs, 1),
                    .RNG.seed = sample(1:1e+06, 1),
                     mu       = rnorm(1, mean = data$log_mean_count, sd = 0.1),  
                     sigma    = runif(1, min = 0.01, max = 0.5))

             }

           }

  }

  if (model == "jags_logistic") {

    out <- function (data = NULL) {

             function (chain = chain) {

               mu    <- rnorm(1, data$log_mean_count, 0.1)
               log_K <- rnorm(1, data$log_max_count, 0.01)

               log_K <- max(c(mu, log_K)) + 0.01

               list(.RNG.name = sample(rngs, 1),
                    .RNG.seed = sample(1:1e+06, 1),
                     mu       = mu, 
                     sigma    = runif(1, 0.01, 0.5),
                     r        = rnorm(1, 0, 0.01),
                     log_K    = log_K)
 
             }
 
           }    
  
  }

  if (model == "jags_logistic_covariates") {

    out <- function (data = NULL) {

             function (chain = chain) {

               mu        <- rnorm(1, data$log_mean_count, 0.1)
               log_K_int <- rnorm(1, data$log_max_count, 0.01)

               log_K_int <- max(c(mu, log_K_int)) + 0.01

               list(.RNG.name    = sample(rngs, 1),
                    .RNG.seed    = sample(1:1e+06, 1),
                     mu          = mu, 
                     sigma       = runif(1, 0.01, 0.5),
                     r_int       = rnorm(1, 0, 0.01),
                     r_slope     = rnorm(1, 0, 0.1),
                     log_K_int   = log_K_int,
                     log_K_slope = rnorm(1, 0, 0.01))

 
             }
 
           }    
  
  }

  if (model == "jags_logistic_competition") {

    out <- function (data = NULL) {

             function (chain = chain) {

               mu        <- rnorm(1, data$log_mean_count, 0.1)
               log_K_int <- rnorm(1, data$log_max_count, 0.01)

               log_K_int <- max(c(mu, log_K_int)) + 0.01

               list(.RNG.name    = sample(rngs, 1),
                    .RNG.seed    = sample(1:1e+06, 1),
                     mu          = mu, 
                     sigma       = runif(1, 0.01, 0.5),
                     r_int       = rnorm(1, 0, 0.01),
                     log_K_int   = log_K_int,
                     log_K_slope = rnorm(1, 0, 0.01))

 
             }
 
           }    
  
  }

  if (model == "jags_logistic_competition_covariates") {

    out <- function (data = NULL) {

             function (chain = chain) {

               mu        <- rnorm(1, data$log_mean_count, 0.1)
               log_K_int <- rnorm(1, data$log_max_count, 0.01)

               log_K_int <- max(c(mu, log_K_int)) + 0.01

               list(.RNG.name         = sample(rngs, 1),
                    .RNG.seed         = sample(1:1e+06, 1),
                     mu               = mu, 
                     sigma            = runif(1, 0.01, 0.5),
                     r_int            = rnorm(1, 0, 0.01),
                     r_slope          = rnorm(1, 0, 0.1),
                     log_K_int        = log_K_int,
                     log_K_slope_NDVI = rnorm(1, 0, 0.01),
                     log_K_slope_DO   = rnorm(1, 0, 0.01))

 
             }
 
           }    
  
  }

  out 
}

runjags_model <- function (model = NULL) {

  if (model == "jags_RW") {

    out <- "model { 
 
             # priors

             mu    ~  dnorm(log_mean_count, 5)
             sigma ~  dunif(0, 1) 
             tau   <- pow(sigma, -2)

             # initial state

             log_X[1]      <- mu
             X[1]          <- exp(log_X[1])
             count[1]      ~  dpois(X[1]) 

             # through time
 
             for(i in 2:N) {

               # Process model

               pred_log_X[i] <- log_X[i-1]
               log_X[i]      ~  dnorm(pred_log_X[i], tau)
               X[i]          <- exp(log_X[i])
   
               # observation model

               count[i] ~ dpois(X[i])

             }

           }"

  }

  if (model == "jags_logistic") {

    out <- "model { 
 
             # priors

             mu    ~  dnorm(log_mean_count, 5)
             sigma ~  dunif(0, 1) 
             tau   <- pow(sigma, -2)
             r     ~  dnorm(0, 5)
             log_K ~  dnorm(log_max_count, 5)
             K     <- exp(log_K) 
 
             # initial state

             log_X[1]      <- mu
             X[1]          <- exp(log_X[1])
             count[1]      ~  dpois(X[1]) 

             # through time

             for(i in 2:N) {

               # Process model

               pred_X[i]     <- X[i-1] * exp(r * (1 - (X[i - 1] / K)))
               pred_log_X[i] <- log(pred_X[i])
               log_X[i]      ~  dnorm(pred_log_X[i], tau)
               X[i]          <- exp(log_X[i])

               # observation model

               count[i] ~ dpois(X[i])

             }

           }"
  
  }

  if (model == "jags_logistic_covariates") {

    out <- "model { 
 
             # priors

             mu          ~  dnorm(log_mean_count, 5)
             sigma       ~  dunif(0, 1) 
             tau         <- pow(sigma, -2)
             r_int       ~  dnorm(0, 5)
             r_slope     ~  dnorm(0, 1)
             log_K_int   ~  dnorm(log_max_count, 5)
             log_K_slope ~  dnorm(0, 1)
 
             # initial state

             log_X[1]      <- mu
             X[1]          <- exp(log_X[1])
             count[1]      ~  dpois(X[1]) 

             # expand parameters

             for (i in 1:N) {

               r[i] <- r_int + r_slope * warm_rain_three_newmoons[i]
               K[i] <- exp(log_K_int + log_K_slope * ndvi_thirteen_newmoons[i]) 

             }

             # through time

             for(i in 2:N) {

               # Process model

               pred_X[i]     <- X[i-1] * exp(r[i] * (1 - (X[i - 1] / K[i])))
               pred_log_X[i] <- log(pred_X[i])
               log_X[i]      ~  dnorm(pred_log_X[i], tau)
               X[i]          <- exp(log_X[i])

               # observation model

               count[i] ~ dpois(X[i])

             }

           }"
  
  }

  if (model == "jags_logistic_competition") {

    out <- "model {  

             # priors

             mu          ~  dnorm(log_mean_count, 5)
             sigma       ~  dunif(0, 1) 
             tau         <- pow(sigma, -2)
             r_int       ~  dnorm(0, 5)
             log_K_int   ~  dnorm(log_max_count, 5)
             log_K_slope ~  dnorm(0, 1)
 
             # initial state

             log_X[1]      <- mu
             X[1]          <- exp(log_X[1])
             count[1]      ~  dpois(X[1]) 

             # expand parameters

             for (i in 1:N) {

               r[i] <- r_int
               K[i] <- exp(log_K_int + log_K_slope * ordii_one_newmoon[i]) 

             }

             # through time

             for(i in 2:N) {

               # Process model

               pred_X[i]     <- X[i-1] * exp(r[i] * (1 - (X[i - 1] / K[i])))
               pred_log_X[i] <- log(pred_X[i])
               log_X[i]      ~  dnorm(pred_log_X[i], tau)
               X[i]          <- exp(log_X[i])

               # observation model

               count[i] ~ dpois(X[i])

             }
 
           }"
  
  }

  if (model == "jags_logistic_competition_covariates") {

    out <- "model { 
  
             # priors

             mu               ~  dnorm(log_mean_count, 5)
             sigma            ~  dunif(0, 1) 
             tau              <- pow(sigma, -1/2)
             r_int            ~  dnorm(0, 5)
             r_slope          ~  dnorm(0, 1)
             log_K_int        ~  dnorm(log_max_count, 5)
             log_K_slope_NDVI ~  dnorm(0, 4)
             log_K_slope_DO   ~  dnorm(0, 4)
 
             # initial state

             log_X[1]      <- mu
             X[1]          <- exp(log_X[1])
             count[1]      ~  dpois(X[1]) 

             # expand parameters

             for (i in 1:N) {

               r[i] <- r_int + r_slope * warm_rain_three_newmoons[i]
               K[i] <- exp(log_K_int + log_K_slope_DO * ordii_one_newmoon[i] + log_K_slope_NDVI * ndvi_thirteen_newmoons[i]) 

             }


             # through time

             for(i in 2:N) {

               # Process model

               pred_X[i]     <- X[i-1] * exp(r[i] * (1 - (X[i - 1] / K[i])))
               pred_log_X[i] <- log(pred_X[i])
               log_X[i]      ~  dnorm(pred_log_X[i], tau)
               X[i]          <- exp(log_X[i])

               # observation model

               count[i] ~ dpois(X[i])

             }

           }"
  
  }

  out

}

runjags_data <- function (model = NULL, abundance, metadata, covariates) {

  true_count_lead <- metadata$time$lead_time_newmoons
  cast_count      <- rep(NA, true_count_lead)
  count           <- c(abundance, cast_count)
  log_mean_count  <- log(mean(abundance, na.rm = TRUE))
  log_max_count   <- log(max(abundance, na.rm = TRUE))

  warm_rain_three_newmoons <- scale(covariates$warm_precip[covariates$newmoon %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoon) - 3)])[ , 1]
  ndvi_thirteen_newmoons   <- scale(covariates$ndvi[covariates$newmoon %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoon) - 13)])[ , 1]
  ordii_one_newmoon        <- scale(covariates$ordii[covariates$newmoon %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoon) - 1)])[ , 1]

  if (model == "jags_RW") {

    out <- list(count          = count, 
                N              = length(count),
                log_mean_count = log_mean_count)

  }

  if (model == "jags_logistic") {

    out <- list(count          = count, 
                N              = length(count),
                log_max_count  = log_max_count,
                log_mean_count = log_mean_count)
  
  }

  if (model == "jags_logistic_covariates") {

    out <- list(count                    = count, 
                N                        = length(count),
                log_max_count            = log_max_count,
                log_mean_count           = log_mean_count,
                warm_rain_three_newmoons = warm_rain_three_newmoons,
                ndvi_thirteen_newmoons   = ndvi_thirteen_newmoons)
  
  }

  if (model == "jags_logistic_competition") {

    out <- list(count                    = count, 
                N                        = length(count),
                log_max_count            = log_max_count,
                log_mean_count           = log_mean_count,
                ordii_one_newmoon        = ordii_one_newmoon)
  
  }

  if (model == "jags_logistic_competition_covariates") {

    out <- list(count                    = count, 
                N                        = length(count),
                log_max_count            = log_max_count,
                log_mean_count           = log_mean_count,
                warm_rain_three_newmoons = warm_rain_three_newmoons,
                ndvi_thirteen_newmoons   = ndvi_thirteen_newmoons,
                ordii_one_newmoon        = ordii_one_newmoon)
  
  }

  out
}


meta_tsglm <- function (ts, model, distr, link, lag, submodels, covariates, metadata, quiet = FALSE) {

  nsubmodels    <- length(submodels)
  submodel_fits <- named_null_list(element_names = submodels)

  for (j in 1:nsubmodels) {

    model_name <- paste(submodels[j][[1]], collapse = ", ")
    model_name <- ifnull(model_name, "<intercept only>")
    messageq("    -", j, ": ", model_name, quiet = quiet)  

    xreg <- covariates[covariates$newmoon %in% (metadata$time$historic_newmoons - lag), unlist(submodels[j])]

    submodel_fits[[j]] <- tsglm(ts    = ts, 
                                model = model, 
                                distr = distr, 
                                xreg  = xreg, 
                                link  = link)

  }

  best_fit     <- which.min(sapply(submodel_fits, AIC))
  out          <- submodel_fits[[best_fit]]
  out$submodel <- unlist(submodels[best_fit])
  out$lag      <- lag
  out

}



forecast.tsglm <- function (object, h, level, ...) {

  out <- predict(object  = object,
                 n.ahead = h,
                 level   = level,
                 ...     = ...)

  out$mean  <- as.ts(out$pred)
  out$lower <- as.ts(out$interval[ , 1, drop = FALSE])
  out$upper <- as.ts(out$interval[ , 2, drop = FALSE])

  dots <- list(...)

  if (!is.null(dots$newxreg)) {
    out$newxreg <- as.ts(dots$newxreg)
  }
  structure(out, class = "forecast")

}

#' @title Provide the Names or Controls for the Prefab Models
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) models or a \code{list} of their controls
#'
#' @return \code{prefab_models}: \code{character} vector of model names. \cr
#'         \code{prefab_model_controls}: \code{list} vector of model controls. \cr
#'
#' @examples
#'  prefab_models( )
#'  prefab_model_controls( )
#'
#' @name prefabricated_models
#'
NULL

#' @rdname prefabricated_models
#'
#' @export
#'
prefab_model_controls <- function( ) {

  prefab_controls_file <- system.file(...     = "extdata", 
                                      ...     = "prefab_model_controls.yaml", 
                                      package = "portalcasting")

  read_yaml(file = prefab_controls_file)

}

#' @rdname prefabricated_models
#'
#' @export
#'
prefab_models <- function( ) {

  names(x = prefab_model_controls( ))

}
