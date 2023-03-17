

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

    xreg <- covariates[covariates$newmoon %in% (metadata$time$historic_newmoons - lag), unlist(submodels[j])]

    submodel_fits[[j]] <- tsglm(ts    = ts, 
                                model = model, 
                                distr = distr, 
                                xreg  = xreg, 
                                link  = link)

  }

  best_fit     <- which.min(sapply(submodel_fits, AIC))
  out          <- submodel_fits[[best_fit]]
  out$submodel <- best_fit
  out$lag      <- lag
  out
}


cast_pevGARCH <- function (model_fit, covariates, metadata) {

  submodel <- model_fit$submodel

  newxreg  <- covariates[covariates$newmoon %in% (metadata$time$forecast_newmoons - model_fit$lag), unlist(submodel)]

  model_cast <- forecast(object  = model_fit, 
                         h       = metadata$time$lead_time_newmoons, 
                         level   = metadata$confidence_level,
                         newxreg = newxreg)

  data.frame(pred  = model_cast$mean,
             lower = model_cast$lower[ , 1], 
             upper = model_cast$upper[ , 1])

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
