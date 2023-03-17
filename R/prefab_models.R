cast_AutoArima <- function (model_fit, metadata) {

  model_cast  <- forecast(object = model_fit, 
                          h      = metadata$time$lead_time_newmoons, 
                          level  = metadata$confidence_level)

  data.frame(pred  = model_cast$mean,
             lower = model_cast$lower[ , 1], 
             upper = model_cast$upper[ , 1])

}

cast_NaiveArima <- function (model_fit, metadata) {

  model_cast  <- forecast(object = model_fit, 
                          h      = metadata$time$lead_time_newmoons, 
                          level  = metadata$confidence_level)

  data.frame(pred  = model_cast$mean,
             lower = model_cast$lower[ , 1], 
             upper = model_cast$upper[ , 1])

}

cast_ESSS <- function (model_fit, metadata) {

  model_cast  <- forecast(object                     = model_fit, 
                          h                          = metadata$time$lead_time_newmoons, 
                          level                      = metadata$confidence_level,
                          allow.multiplicative.trend = TRUE)

  data.frame(pred  = model_cast$mean,
             lower = model_cast$lower[ , 1], 
             upper = model_cast$upper[ , 1])

}

cast_nbGARCH <- function (model_fit, metadata) {

  model_cast <- forecast(object = model_fit, 
                         h      = metadata$time$lead_time_newmoons, 
                         level  = metadata$confidence_level)

  data.frame(pred  = model_cast$mean,
             lower = model_cast$lower[ , 1], 
             upper = model_cast$upper[ , 1])

}

fit_nbsGARCH <- function (ts, model, distr, link, covariates, metadata) {

  xreg <- covariates[covariates$newmoon %in% metadata$time$historic_newmoons, c("cos2pifoy", "sin2pifoy")]

  tsglm(ts    = ts, 
        model = model, 
        distr = distr, 
        xreg  = xreg, 
        link  = link)


}


fit_pevGARCH <- function (ts, model, distr, link, lag, covariates, metadata, quiet = FALSE) {

  submodels     <- covariate_models(model = "pevGARCH")
  nsubmodels    <- length(submodels)
  submodel_fits <- named_null_list(element_names = submodels)
 
  for (j in 1:nsubmodels) {

    model_name <- paste(submodels[j][[1]], collapse = ", ")
    model_name <- ifnull(model_name, "<intercept only>")
    messageq("    -", j, ": ", model_name, quiet = quiet)  

    xreg <- covariates[covariates$newmoon %in% metadata$time$historic_newmoons - lag, unlist(submodels[j])]
    xreg <- na.interp(apply(xreg, 2, na.interp))

    submodel_fits[[j]] <- tsglm(ts    = ts, 
                                model = model, 
                                distr = distr, 
                                xreg  = xreg, 
                                link  = link)

  }

  



}


cast_nbsGARCH <- function (model_fit, covariates, metadata) {

  newxreg <- covariates[covariates$newmoon %in% metadata$time$forecast_newmoons, c("cos2pifoy", "sin2pifoy")]

  model_cast <- forecast(object  = model_fit, 
                         h       = metadata$time$lead_time_newmoons, 
                         level   = metadata$confidence_level,
                         newxreg = newxreg)

  data.frame(pred  = model_cast$mean,
             lower = model_cast$lower[ , 1], 
             upper = model_cast$upper[ , 1])

}

forecast.tsglm <- function (object, h, level, ...) {

  out <- predict(object  = object,
                 n.ahead = h,
                 level   = level,
                 ...     = ...)

  out$mean  <- as.ts(out$pred)
  out$lower <- as.ts(out$interval[ , 1, drop = FALSE])
  out$upper <- as.ts(out$interval[ , 2, drop = FALSE])

  out

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
