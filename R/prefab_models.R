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


#' @title Functions for the prefab portalcasting models
#'
#' @description Functions used for fitting the prefab models. For detailed descriptions of the models, see the \href{https://bit.ly/2xP9jKI}{model vignette}. \cr \cr
#'  \code{AutoArima} fits a flexible auto-regressive integrated moving average model using \code{\link[forecast]{auto.arima}}. \cr \cr
#'  \code{NaiveArima} fits a baseline random walk with no drift model as an ARIMA(0,1,0) using \code{\link[forecast]{Arima}}. \cr \cr
#'  \code{ESSS} fits a flexible exponential smoothing state space model with the possibility of multiplicative trends using \code{\link[forecast]{ets}}; requires interpolation of data. \cr \cr
#'  \code{nbGARCH} fits a generalized autoregresive conditional heteroscedasticity model with overdispersion (\emph{i.e.}, a negative binomial response variable) using \code{\link[tscount]{tsglm}}; requires interpolation of data. \cr \cr
#'  \code{nbsGARCH} fits a seasonal generalized autoregresive conditional heteroscedasticity model with overdispersion (\emph{i.e.}, a negative binomial response variable) using \code{\link[tscount]{tsglm}}; requires interpolation of data. \cr \cr
#'  \code{pevGARCH} fits a set of generalized autoregressive conditional heteroscedasticity models with Poisson response variables and  environmental covariates (max temp, mean temp, min temp, precipitation, and NDVI) using \code{\link[tscount]{tsglm}}; requires interpolation of data. \cr \cr
#  \code{simplexEDM} fits an EDM model with the simplex projection "kernel" using \code{\link[rEDM]{simplex}}. \cr \cr
#  \code{GPEDM} fits an EDM model using Gaussian Processes for function approximation using \code{\link[rEDM]{tde_gp}}. \cr \cr
#'  \code{jags_RW} fits a log-scale density random walk with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. \cr \cr
#'  \code{jags_logistic} fits a log-scale density logistic-growth (r-K) model with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. 
#'  \code{jags_logistic_covariates} fits a log-scale density logistic-growth (r-K) based on covariates (warm rain influencing r, NDVI influencing K) model with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. 
#'  \code{jags_logistic_competition} fits a log-scale density logistic-growth (r-K) based on covariates (competitor density influencing K) model with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. 
#'  \code{jags_logistic_competition_covariates} fits a log-scale density logistic-growth (r-K) based on covariates (warm rain influencing r, NDVI and competitor density influencing K) model with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. 
#'
#' @details 
#'  \code{AutoArima} \cr
#'  Because the seasonality and sampling occurring with different frequencies, which \code{\link[forecast]{auto.arima}} cannot accommodate, seasonal AutoArima models are not included.  \cr \cr
#'  \code{pevGARCH} \cr
#'  Environmental predictors have an optional \code{lag} time, which is currently structured to impact all covariates the same and defaults to \code{6}. \cr
#'  All covariate models are fit and the best model (by AIC) is selected for use. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param dataset \code{character} value name of the rodent data set, such as (\code{"all"} or \code{"controls"}).
#'
#' @param species \code{character} value name of the rodent species.
#'
#' @param quiet \code{logical} value indicating if the function should be quiet.
#'
#' @param control_runjags \code{list} of arguments passed to \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{list} of \enumerate{
#'   \item{model metadata \code{list} (\code{"metadata"})}
#'   \item{cast summary \code{data.frame} (\code{"cast_tab"})}
#'   \item{\code{list} of model fit objects (\code{"model_fits"})}
#'   \item{\code{list} of model cast objects (\code{"model_casts"})}}
#'
#' @references 
#'  Hyndman, R., Bergmeir, C., Caceres, G., Chhay, L., O'Hara-Wild, M., Petropoulos, F., Razbash, S., Wang, E., and Yasmeen, F. 2018. forecast: Forecasting functions for time series and linear models. R package version 8.3. \href{http://pkg.robjhyndman.com/forecast}{URL}. 
#'
#'  Liboschik, T., Fokianos, K., and Fried, R. 2017. tscount: An R Package for Analysis of Count Time Series Following Generalized Linear Models. \emph{Journal of Statistical Software} \strong{82}:5, 1-51. \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical models using Gibbs Sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X. \href{https://bit.ly/33aQ37Y}{URL}.
#'
#' @name prefab_model_functions
#'
NULL

#' @rdname prefab_model_functions
#'
#' @export
#'
AutoArima <- function (main     = ".", 
                       dataset  = NULL,
                       species  = NULL,
                       settings = directory_settings( ), 
                       quiet    = FALSE, 
                       verbose  = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)

  model <- "AutoArima"

  dataset <- tolower(dataset)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)



  abundance <- prepare_rodent_abundance(main     = main,
                                        dataset  = dataset,
                                        species  = species,
                                        model    = model,
                                        settings = settings,
                                        quiet    = quiet,
                                        verbose  = verbose)

##
  metadata <- read_metadata(main     = main,
                            settings = settings)

  model_fit   <- auto.arima(y = abundance)
  model_cast  <- forecast(object = model_fit, 
                     h      = metadata$time$lead_time_newmoons, 
                     level  = metadata$confidence_level)

  model_cast  <- data.frame(pred  = model_cast$mean,
                          lower = model_cast$lower[,1], 
                          upper = model_cast$upper[,1])

##
  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 
}





#' @rdname prefab_model_functions
#'
#' @export
#'

NaiveArima <- function (main     = ".", 
                        dataset  = NULL,
                        species  = NULL,
                        settings = directory_settings( ), 
                        quiet    = FALSE, 
                        verbose  = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)

  model <- "NaiveArima"

  dataset <- tolower(dataset)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)


  metadata <- read_metadata(main     = main,
                            settings = settings)

  abundance <- prepare_rodent_abundance(main     = main,
                                      dataset  = dataset,
                                      species  = species,
                                      model    = model,
                                      settings = settings,
                                      quiet    = quiet,
                                      verbose  = verbose)

  model_fit   <- Arima(y     = abundance, 
                  order = c(0, 1, 0))
  model_cast  <- forecast(object = model_fit, 
                     h      = metadata$time$rodent_lead_time, 
                     level  = metadata$confidence_level)

  model_cast  <- data.frame(pred  = model_cast$mean,
                          lower = model_cast$lower[,1], 
                          upper = model_cast$upper[,1])

  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 

}


#' @rdname prefab_model_functions
#' 
#' @export
#'

ESSS <- function (main     = ".", 
                  dataset  = NULL,
                  species  = NULL,
                  settings = directory_settings( ), 
                  quiet    = FALSE, 
                  verbose  = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)

  model <- "ESSS"

  dataset <- tolower(dataset)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)


  metadata <- read_metadata(main     = main,
                            settings = settings)

  abundance <- prepare_rodent_abundance(main     = main,
                                      dataset  = dataset,
                                      species  = species,
                                      model    = model,
                                      settings = settings,
                                      quiet    = quiet,
                                      verbose  = verbose)

  model_fit   <- ets(y = abundance)
  model_cast  <- forecast(object = model_fit, 
                     h      = metadata$time$rodent_lead_time, 
                     level  = metadata$confidence_level,
                     allow.multiplicative.trend = TRUE)

  model_cast  <- data.frame(pred  = model_cast$mean,
                          lower = model_cast$lower[,1], 
                          upper = model_cast$upper[,1])

  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 
}



#' @rdname prefab_model_functions
#'
#' @export
#'

nbGARCH <- function (main     = ".", 
                     dataset  = NULL,
                     species  = NULL,
                     settings = directory_settings( ), 
                     quiet    = FALSE, 
                     verbose  = FALSE) {

 return_if_null(x = dataset)
  return_if_null(x = species)

  model <- "nbGARCH"

  dataset <- tolower(dataset)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  abundance <- prepare_rodent_abundance(main     = main,
                                      dataset  = dataset,
                                      species  = species,
                                      model    = model,
                                      settings = settings,
                                      quiet    = quiet,
                                      verbose  = verbose)

    past <- list(past_obs  = 1, 
                 past_mean = 13)
    model_fit <- tryCatch(
                   tsglm(ts    = abundance, 
                         model = past, 
                         distr = "nbinom", 
                         link  = "log"),
                   warning = function(x){NA}, 
                   error = function(x){NA})
    if(all(is.na(model_fit)) || AIC(model_fit) == Inf){
      model_fit <- tryCatch(
                   tsglm(ts    = abundance, 
                         model = past, 
                         distr = "poisson", 
                         link  = "log"),
                     warning = function(x){NA}, 
                     error = function(x){NA})
    }
    if(!all(is.na(model_fit))){
      model_cast <- predict(object  = model_fit, 
                       n.ahead = metadata$time$rodent_lead_time, 
                       level   = metadata$confidence_level)
      model_cast <- data.frame(pred = as.numeric(model_cast$pred),
                       lower = as.numeric(model_cast$interval[ , 1]),
                       upper = as.numeric(model_cast$interval[ , 2]))
    }  


  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 



}


#' @rdname prefab_model_functions
#'
#' @export
#'

nbsGARCH <- function (main     = ".", 
                     dataset  = NULL,
                     species  = NULL,
                     settings = directory_settings( ), 
                     quiet    = FALSE, 
                     verbose  = FALSE) {

 return_if_null(x = dataset)
  return_if_null(x = species)

  model <- "nbsGARCH"

  dataset <- tolower(dataset)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  abundance <- prepare_rodent_abundance(main     = main,
                                      dataset  = dataset,
                                      species  = species,
                                      model    = model,
                                      settings = settings,
                                      quiet    = quiet,
                                      verbose  = verbose)

#

  covariates <- read_covariates(main = main, settings = settings)


  for_hist         <- which(covariates$newmoonnumber >= metadata$time$start_moon & covariates$newmoonnumber < metadata$time$rodent_cast_moons[1])
  for_cast         <- which(covariates$newmoonnumber %in% metadata$time$rodent_cast_moons) 
  predictors       <- covariates[for_hist, c("sin2pifoy", "cos2pifoy")]
  cast_predictors  <- covariates[for_cast, c("sin2pifoy", "cos2pifoy")]

#



    past <- list(past_obs  = 1, 
                 past_mean = 13)
    model_fit <- tryCatch(
                   tsglm(ts    = abundance, 
                         model = past, 
                         distr = "nbinom", 
                         xreg  = predictors, 
                         link  = "log"),
                   warning = function(x){NA}, 
                   error   = function(x){NA})

    if (all(is.na(model_fit)) || AIC(model_fit) == Inf) {

      model_fit <- tryCatch(
                     tsglm(ts    = abundance, 
                           model = past, 
                           distr = "poisson", 
                           xreg  = predictors, 
                           link  = "log"),
                     warning = function(x){NA}, 
                     error   = function(x){NA})
    }
    if (!all(is.na(model_fit))) {

      model_cast <- predict(object  = model_fit, 
                       n.ahead = metadata$time$rodent_lead_time, 
                       level   = metadata$confidence_level,
                       newxreg = cast_predictors)
      model_cast <- data.frame(pred = as.numeric(model_cast$pred),
                       lower = as.numeric(model_cast$interval[ , 1]),
                       upper = as.numeric(model_cast$interval[ , 2]))
    }    

  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 
}



#' @rdname prefab_model_functions
#' 
#' @export
#'
pevGARCH <- function (main     = ".", 
                     dataset  = NULL,
                     species  = NULL,
                     settings = directory_settings( ), 
                     quiet    = FALSE, 
                     verbose  = FALSE) {

 return_if_null(x = dataset)
  return_if_null(x = species)

  model <- "pevGARCH"

  dataset <- tolower(dataset)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  abundance <- prepare_rodent_abundance(main     = main,
                                      dataset  = dataset,
                                      species  = species,
                                      model    = model,
                                      settings = settings,
                                      quiet    = quiet,
                                      verbose  = verbose)




  covariates <- read_covariates(main     = main,
                            settings = settings)

# lag the covariates when prepping and then we can skip this situation #
  covar_lag <- lag_covariates(covariates = covariates, 
                              lag        = 6,
                              tail       = TRUE)


  covariates <- read_covariates(main = main, settings = settings)

  for_hist         <- which(covariates$newmoonnumber >= metadata$time$start_moon & covariates$newmoonnumber < metadata$time$rodent_cast_moons[1])
  for_cast         <- which(covar_lag$newmoonnumber %in% metadata$time$rodent_cast_moons) 
  covar_hist       <- covar_lag[for_hist, ]
  covar_cast       <- covar_lag[for_cast, ]
  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()
  dataset_controls <- metadata$dataset_controls[[dataset]]      
  models           <- covariate_models(model = "pevGARCH")
  nmodels          <- length(models)

    past <- list(past_obs = 1, past_mean = 13)

    model_count <- 1


    mods  <- named_null_list(models)
    casts <- named_null_list(models)
    AICs    <- rep(NA, nmodels)

    for (j in 1:nmodels) {

      m          <- models[j]
      model_name <- paste(m[[1]], collapse = ", ")
      model_name <- ifnull(model_name, "<intercept only>")
      messageq("    -", j, ": ", model_name, quiet = !verbose)

      predictors      <- NULL
      cast_predictors <- NULL

      if (!(is.null(unlist(m)))) {

        cols_in         <- unlist(m) 
        predictors      <- covar_hist[ , cols_in]
        cast_predictors <- covar_cast[ , cols_in]

      }

      mods[[j]] <- tryCatch(
                     tsglm(ts    = abundance, 
                           model = past, 
                           distr = "poisson", 
                           xreg  = predictors, 
                           link  = "log"),
                     warning = function(x){NA}, 
                     error   = function(x){NA})
      if(!all(is.na(mods[[j]]))){
        casts[[j]] <-  tryCatch(
                              predict(object  = mods[[j]], 
                                      n.ahead = metadata$time$rodent_lead_time, 
                                      level   = metadata$confidence_level,
                                      newxreg = cast_predictors),
                              warning = function(x){NA}, 
                              error = function(x) {NA})
      }
      AICs[j] <- tryCatch(AIC(mods[[j]]), 
                          error = function(x) {Inf})
    } 
    if(all(AICs == Inf)){
      next()
    }
    best_mod <- which.min(AICs)

    model_fit <- mods[[best_mod]] 
    model_cast <- casts[[best_mod]] 

      model_cast <- data.frame(pred = as.numeric(model_cast$pred),
                       lower = as.numeric(model_cast$interval[ , 1]),
                       upper = as.numeric(model_cast$interval[ , 2]))

  process_model_output(main       = main,
                       model_fit  = model_fit,
                       model_cast = model_cast,
                       model      = model,
                       dataset    = dataset,
                       species    = species,
                       settings   = settings,
                       quiet      = quiet,
                       verbose    = verbose) 

}
