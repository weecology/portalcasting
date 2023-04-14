
#' @title Create and Run a runjags Portalcasting Model 
#'
#' @description Using the runjags (Denwood 2016) package to produce JAGS-based forecasts. \cr 
#'              `fit_runjags`: Wraps up the runjags model object preparation functions with the model running ([`run.jags`][runjags::run.jags] function in the runjags (Denwood 2016) package) we use to run JAGS (Plummer 2003) models in portalcasting. \cr
#'              `runjags_data`, `runjags_monitor`, `runjags_model`, `runjags_inits`: Produce the model-specific components as named. \cr 
#'              `forecast.runjags`: A convenience function for extracting existing forecasts from runjags objects and summarizing them into a `"forecast"`-class object. \cr 
#'              `runjags_controls`: Combines the [`run.jags`][runjags::run.jags] control parameters that users may be interested in changing with a few portalcasting-specific parameters into a control list for input into specific model functions. 
#'
#' @param abundance Non-negative `integer`-conformable vector of rodent abundances to use in forecasting. See [`prepare_abundance`].
#'
#' @param metadata `list` of model control elements. See [`prepare_metadata`].
#'
#' @param covariates `data.frame` of covariates used in modeling. See [`prepare_covariates`].
#'
#' @param control_runjags `list` of controls for running runjags models. See [`runjags_controls`]. Optional. If not provided here, will be taken from the model controls list.
#'
#' @param inits `list` of model parameter initializer functions. See [`prefab_models_controls`].
#'
#' @param model `character` value of the model file name. See [`prefab_models_controls`].
#'
#' @param data_names `character` vector of data values to include in the data `list`. See [`prefab_models_controls`].
#'
#' @param seed A single `integer`-conformable value or `NULL` set in [`set.seed`].
#'
#' @param nchains Non-negative `integer`-conformable value of the number of parallel chains to use. See [`run.jags`][runjags::run.jags].
#'
#' @param adapt Non-negative `integer`-conformable value of the number of adaptation steps to use. See [`run.jags`][runjags::run.jags].
#'
#' @param burnin Non-negative `integer`-conformable value of the number of burnin steps to use. See [`run.jags`][runjags::run.jags].
#'
#' @param sample Non-negative `integer`-conformable value of the number of sampling steps to use. See [`run.jags`][runjags::run.jags].
#'
#' @param thin Positive `integer`-conformable value of the thinning interval to use. See [`run.jags`][runjags::run.jags].
#'
#' @param modules `character` vector of external modules to add to JAGS. See [`run.jags`][runjags::run.jags].
#'
#' @param method `character` value of the [`run.jags`][runjags::run.jags] method to use. Options include `"rjags"`, `"simple"`, `"interruptible"`, `"parallel"`, `"rjparallel"`, `"background"`, `"bgparallel"`, and `"snow"`. See [`run.jags`][runjags::run.jags].
#'
#' @param factories `character` vector of factory modules to add to JAGS. See [`run.jags`][runjags::run.jags].
#'
#' @param monitors `character` vector of parameters to track. Forecasted observations and state variables are tracked automatically.
#'
#' @param mutate A `function` or `list` (with the first element being a `function`) used to add variables to the posterior chain (rather than throughout sampling). See [`run.jags`][runjags::run.jags]. 
#'
#' @param silent_jags `logical` value for quieting the output from the runjags function, including the underlying JAGS output. 
#'
#' @param object A `runjags`-class object with columns of `"X"` values (state variables) in the the `mcmc` element.
#'
#' @param h `integer`-conformable number of steps forward to include in the forecast.
#'
#' @param level `numeric` of the confidence level to use in summarizing the predictions.
#'
#' @param nsamples `integer` (or integer `numeric`) number of samples used to summarizing model output of sample-based estimates. 
#'
#' @param ... Additional parameters 
#'
#' @return `fit_runjags`: An object of class `"runjags"` of model components. See [`run.jags`][runjags::run.jags]. \cr
#'         `runjags_data`: A `list` of model-specific data for use in [`run.jags`][runjags::run.jags]. \cr
#'         `runjags_monitor`: A `vector` of model-specific `character` values of parameters to track in [`run.jags`][runjags::run.jags].  \cr
#'         `runjags_model`: A single `character` value of the JAGS model block for [`run.jags`][runjags::run.jags].  \cr 
#'         `runjags_inits`: A `function` that takes the argument `data` to produce chain-specific initial values for [`run.jags`][runjags::run.jags]. \cr
#'         `runjags_controls`: A `list` of controls. \cr 
#'         `forecast.runjags`: `list` with `"forecast"`-class with named elements including `"mean"`, `"lower"`, `"upper"`, and `"level"`. 
#' 
#' @references 
#'  Denwood, M. J. 2016. runjags: an R package providing interface utilities, model templates, parallel computing methods and additional distributions for MCMC models in JAGS. Journal of Statistical Software, 71:9. [URL](https://www.jstatsoft.org/article/view/v071i09). 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical models using Gibbs Sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X. [URL](https://bit.ly/33aQ37Y).
#'
#' @name runjags models
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "runjags")
#'
#'    setup_dir(main = main1)
#'    dataset <- "all"
#'    species <- "DM"
#'    model   <- "runjags_RW"
#'  
#'    abundance      <- prepare_abundance(main    = main1,
#'                                        dataset = dataset,
#'                                        species = species,
#'                                        model   = model)
#'    model_controls <- models_controls(main       = main1,
#'                                      models     = model)[[model]]
#'    metadata       <- read_metadata(main        = main1)
#'    newmoons       <- read_newmoons(main        = main1)                                        
#'    covariates     <- read_covariates(main      = main1)
#'    control_runjags <- runjags_controls( )
#'    data_names      <- c("count", "N", "log_mean_count")
#'
#'    runjags_model(model = model)
#'
#'    runjags_monitors(monitors = c("mu", "sigma"),
#'                     metadata = metadata)
#'
#'    data <- runjags_data(data_names = data_names,
#'                         abundance  = abundance,
#'                         metadata   = metadata,
#'                         covariates = covariates)
#'
#'    runjags_inits(inits = list(mu    = rnorm(1, mean = data$log_mean_count, sd = 0.1),
#'                               sigma = runif(1, min  = 0.01, max = 0.5)))
#'
#'    fit <- fit_runjags(abundance       = abundance, 
#'                       metadata        = metadata,
#'                       covariates      = covariates, 
#'                       monitors        = c("mu", "sigma"), 
#'                       inits           = list(mu    = rnorm(1, data$log_mean_count, 0.1),
#'                                              sigma = runif(1, 0.01, 0.5)), 
#'                       model           = model,
#'                       data_names      = data_names,
#'                       control_runjags = control_runjags)
#'  
#'    forecast(object   = fit_runjags,  
#'             h        = metadata$lead_time_newmoons,   
#'             level    = metadata$confidence_level,   
#'             nsamples = metadata$nsamples)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL

#' @rdname runjags-models
#'
#' @export
#'
runjags_inits <- function (inits) {

  rngs  <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")

  function (data = NULL) {

    function(chain = chain) {

      model_specific_inits <- named_null_list(element_names = names(inits))
      for (i in 1:length(model_specific_inits)) {

        model_specific_inits[[i]] <- eval(parse(text = inits[[i]]))

      }

      c(.RNG.name = sample(rngs, 1),
        .RNG.seed = sample(1:1e+06, 1), 
        model_specific_inits)                  
      
    }

  }

}

#' @rdname runjags-models
#'
#' @export
#'
runjags_model <- function (model) {

  scan(file  = model,
       what  = "character",
       quiet = TRUE)
  
}

#' @rdname runjags-models
#'
#' @export
#'
runjags_monitors <- function (monitors,
                              metadata) {

  count_predicted <- paste0("count_predicted[", metadata$time$forecast_newmoonnumbers - metadata$time$historic_start_newmoonnumber + 1, "]")
  X               <- paste0("X[", metadata$time$forecast_newmoonnumbers - metadata$time$historic_start_newmoonnumber + 1, "]")
  c(count_predicted, X, monitors)

}

#' @rdname runjags-models
#'
#' @export
#'
runjags_data <- function (data_names,
                          abundance, 
                          metadata, 
                          covariates) {

  true_count_lead <- metadata$time$lead_time_newmoons
  forecast_count      <- rep(NA, true_count_lead)
  count           <- c(abundance, forecast_count)
  N               <- length(count)
  log_mean_count  <- log(mean(abundance, na.rm = TRUE))
  log_max_count   <- log(max(abundance, na.rm = TRUE))

  warm_rain_three_newmoons <- scale(covariates$warm_precip[covariates$newmoonnumber %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoonnumber) - 3)])[ , 1]
  ndvi_thirteen_newmoons   <- scale(covariates$ndvi[covariates$newmoonnumber %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoonnumber) - 13)])[ , 1]
  ordii_one_newmoon        <- scale(covariates$ordii[covariates$newmoonnumber %in% ((metadata$time$historic_start_newmoon:metadata$time$forecast_end_newmoonnumber) - 1)])[ , 1]

  list(true_count_lead          = true_count_lead,
       forecast_count               = forecast_count,
       count                    = count,
       N                        = N,
       log_mean_count           = log_mean_count,
       log_max_count            = log_max_count,
       warm_rain_three_newmoons = warm_rain_three_newmoons,
       ndvi_thirteen_newmoons   = ndvi_thirteen_newmoons,
       ordii_one_newmoon        = ordii_one_newmoon)[data_names]

}



#' @rdname runjags-models
#'
#' @export
#'
fit_runjags <- function (abundance, 
                         metadata, 
                         covariates, 
                         monitors, 
                         inits,
                         model,
                         data_names,
                         control_runjags = runjags_controls( )) {

  monitor    <- runjags_monitors(monitors = monitors,
                                 metadata = metadata)  
  init       <- runjags_inits(inits       = inits)  
  jags_model <- runjags_model(model       = model)  
  data       <- runjags_data(data_names   = data_names,
                             abundance    = abundance,
                             metadata     = metadata,
                             covariates   = covariates)  


  runjags.options(silent.jags    = control_runjags$silent_jags, 
                  silent.runjags = control_runjags$silent_jags)

  model_fit <- run.jags(model     = jags_model, 
                        monitor   = monitor, 
                        inits     = init(data), 
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
                              level,
                              nsamples,
                              seed = NULL,
                              ...) {

  vals        <- combine.mcmc(mcmc.objects = object$mcmc)

  pred_cols   <- grep("count_predicted", colnames(vals))
  vals        <- vals[ , pred_cols]
  HPD         <- HPDinterval(obj = as.mcmc(vals), prob = level)
  set.seed(seed = seed)
  sample_rows <- sample(1:nrow(vals), min(c(nrow(vals), nsamples)))
  sample      <- vals[sample_rows, ]
  out         <- list(mean   = as.ts(round(apply(vals, 2, mean)[1:h], 3)),
                      lower  = as.ts(round(HPD[1:h, "lower", drop = FALSE], 3)), 
                      upper  = as.ts(round(HPD[1:h, "upper", drop = FALSE], 3)),
                      level  = level,
                      sample = sample)

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
