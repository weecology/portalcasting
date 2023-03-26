
#' @title Create and Run a runjags Portalcasting Model 
#'
#' @description Using the runjags (Denwood 2016) package to produce JAGS-based forecasts. \cr \cr
#'   \code{fit_runjags}: Wraps up the runjags model object preparation functions with the model running (\code{\link[runjags]{run.jags}} function in the runjags (Denwood 2016) package) we use to run JAGS (Plummer 2003) models in portalcasting. \cr
#'   \code{runjags_data}, \code{runjags_monit}, \code{runjags_model}, \code{runjags_inits}: Produce the model-specific components as named. \cr 
#'   \code{forecast.runjags}: A convenience function for extracting existing forecasts from runjags objects and summarizing them into a \code{"forecast"}-class object. \cr 
#'   \code{runjags_controls}: Combines the \code{\link[runjags]{run.jags}} control parameters that users may be interested in changing with a few portalcasting-specific parameters into a control list for input into specific model functions.
#'
#' @param abundance Non-negative \code{integer}-conformable vector of rodent abundances to use in forecasting. See \code{\link{prepare_abundance}}.
#'
#' @param metadata \code{list} of model control elements. See \code{\link{prepare_metadata}}.
#'
#' @param covariates \code{data.frame} of covariates used in modeling. See \code{\link{prepare_covariates}}.
#'
#' @param control_runjags \code{list} of controls for running runjags models. See \code{\link{runjags_controls}}. Optional. If not provided here, will be taken from the model controls list.
#'
#' @param model_controls \code{list} of model controls for the model of interest. See \code{\link{prefab_model_controls}}.
#'
#' @param nchains Non-negative \code{integer}-conformable value of the number of parallel chains to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param adapt Non-negative \code{integer}-conformable value of the number of adaptation steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param burnin Non-negative \code{integer}-conformable value of the number of burnin steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param sample Non-negative \code{integer}-conformable value of the number of sampling steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param thin Positive \code{integer}-conformable value of the thinning interval to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param modules \code{character} vector of external modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param method \code{character} value of the \code{\link[runjags]{run.jags}} method to use. Options include \code{"rjags"}, \code{"simple"}, \code{"interruptible"}, \code{"parallel"}, \code{"rjparallel"}, \code{"background"}, \code{"bgparallel"}, and \code{"snow"}. See \code{\link[runjags]{run.jags}}.
#'
#' @param factories \code{character} vector of factory modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param mutate A \code{function} or \code{list} (with the first element being a \code{function}) used to add variables to the posterior chain (rather than throughout sampling). See \code{\link[runjags]{run.jags}}. 
#'
#' @param silent_jags \code{logical} value for quieting the output from the runjags function, including the underlying JAGS output. 
#'
#' @param object A \code{runjags}-class object with columns of \code{"X"} values (state variables) in the the \code{mcmc} element.
#'
#' @param h \code{integer}-conformable number of steps forward to include in the forecast.
#'
#' @param level \code{numeric} of the confidence level to use in summarizing the predictions.
#'
#' @return 
#'   \code{fit_runjags}: An object of class \code{"runjags"} of model components. See \code{\link[runjags]{run.jags}}. \cr
#'   \code{runjags_data}: A \code{list} of model-specific data for use in \code{\link[runjags]{run.jags}}. \cr
#'   \code{runjags_monit}: A \code{vector} of model-specific \code{character} values of parameters to track in \code{\link[runjags]{run.jags}}.  \cr
#'   \code{runjags_model}: A single \code{character} value of the JAGS model block for \code{\link[runjags]{run.jags}}.  \cr
#'   \code{runjags_inits}: A \code{function} that takes the argument \code{data} to produce chain-specific initial values for \code{\link[runjags]{run.jags}}. \cr
#'   \code{runjags_controls}: A \code{list} of controls. \cr
#'   \code{forecast.runjags}: \code{list} with \code{"forecast"}-class with named elements including \code{"mean"}, \code{"lower"}, \code{"upper"}, and \code{"level"}. 
#' 
#'
#' @references 
#'  Denwood, M. J. 2016. runjags: an R package providing interface utilities, model templates, parallel computing methods and additional distributions for MCMC models in JAGS. Journal of Statistical Software, 71:9. \href{https://www.jstatsoft.org/article/view/v071i09}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical models using Gibbs Sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X. \href{https://bit.ly/33aQ37Y}{URL}.
#'
#' @name runjags models
#'
NULL


#' @rdname runjags-models
#'
#' @export
#'
runjags_inits <- function (model_controls) {

  rngs  <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

  function (data = NULL) {

    function(chain = chain) {

      model_specific_inits <- named_null_list(element_names = names(model_controls$fit$inits))
      for (i in 1:length(model_specific_inits)) {

        model_specific_inits[[i]] <- eval(parse(text = model_controls$fit$inits[[i]]))

      }

      c(list(.RNG.name = sample(rngs, 1),
             .RNG.seed = sample(1:1e+06, 1)),
        model_specific_inits)                  
      
    }

  }

}

#' @rdname runjags-models
#'
#' @export
#'
runjags_model <- function (model_controls) {

  scan(file  = eval(parse(text = model_controls$fit$full_model_file)),
       what  = "character",
       quiet = TRUE)
  
}

#' @rdname runjags-models
#'
#' @export
#'
runjags_monitors <- function (model_controls, 
                              metadata) {

  out <- paste0("count_predicted[", metadata$time$forecast_newmoonnumbers - metadata$time$historic_start_newmoonnumber + 1, "]")
  c(out, model_controls$fit$monitors)

}

#' @rdname runjags-models
#'
#' @export
#'
runjags_data <- function (model_controls,
                          abundance, 
                          metadata, 
                          covariates) {

  true_count_lead <- metadata$time$lead_time_newmoons
  cast_count      <- rep(NA, true_count_lead)
  count           <- c(abundance, cast_count)
  N               <- length(count)
  log_mean_count  <- log(mean(abundance, na.rm = TRUE))
  log_max_count   <- log(max(abundance, na.rm = TRUE))

  warm_rain_three_newmoons <- scale(covariates$warm_precip[covariates$newmoonnumber %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoonnumber) - 3)])[ , 1]
  ndvi_thirteen_newmoons   <- scale(covariates$ndvi[covariates$newmoonnumber %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoonnumber) - 13)])[ , 1]
  ordii_one_newmoon        <- scale(covariates$ordii[covariates$newmoonnumber %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoonnumber) - 1)])[ , 1]

  list(true_count_lead          = true_count_lead,
       cast_count               = cast_count,
       count                    = count,
       N                        = N,
       log_mean_count           = log_mean_count,
       log_max_count            = log_max_count,
       warm_rain_three_newmoons = warm_rain_three_newmoons,
       ndvi_thirteen_newmoons   = ndvi_thirteen_newmoons,
       ordii_one_newmoon        = ordii_one_newmoon)[model_controls$fit$data_needs]

}



#' @rdname runjags-models
#'
#' @export
#'
fit_runjags <- function (abundance, 
                         metadata, 
                         covariates, 
                         model_controls,
                         control_runjags = runjags_controls( )) {

  if (missing(control_runjags)) {
    control_runjags <- eval(parse(text = model_controls$fit$args$control_runjags))
  }

  monitor    <- runjags_monitors(model_controls = model_controls,
                                 metadata       = metadata)  
  inits      <- runjags_inits(model_controls    = model_controls)  
  jags_model <- runjags_model(model_controls    = model_controls)  
  data       <- runjags_data(model_controls     = model_controls,
                             abundance          = abundance,
                             metadata           = metadata,
                             covariates         = covariates)  

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

#' @rdname runjags-models
#'
#' @export
#'
forecast.runjags <- function (object, 
                              h, 
                              level) {

  vals      <- combine.mcmc(mcmc.objects = object$mcmc)

  pred_cols <- grep("X", colnames(vals))
  vals      <- vals[ , pred_cols]
  HPD       <- HPDinterval(obj = as.mcmc(vals), prob = level)
  out       <- list(mean  = as.ts(round(apply(vals, 2, mean)[1:h], 3)),
                    lower = as.ts(round(HPD[1:h, "lower", drop = FALSE], 3)), 
                    upper = as.ts(round(HPD[1:h, "upper", drop = FALSE], 3)))
  out$level <- level

  structure(.Data = out, 
            class = "forecast")

}

#' @rdname runjags-models
#'
#' @export
#'
runjags_controls <- function (nchains     = 4, 
                             adapt       = 1e4, 
                             burnin      = 1e4, 
                             sample      = 1e4, 
                             thin        = 10, 
                             modules     = "glm", 
                             method      = "interruptible", 
                             factories   = "", 
                             mutate      = NA, 
                             silent_jags = FALSE){

  list(nchains     = nchains, 
       adapt       = adapt, 
       burnin      = burnin, 
       sample      = sample,
       thin        = thin, 
       modules     = modules, 
       method      = method, 
       factories   = factories,
       mutate      = mutate, 
       silent_jags = silent_jags)

}
