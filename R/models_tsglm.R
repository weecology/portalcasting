

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


#' @title Forecast a TSGLM Object
#'
#' @description A wrapper around the \code{predict} function for tsglm objects that produces a \code{"forecast"}-class object.
#' 
#' @param object A \code{tsglm}-class object.
#'
#' @param h \code{integer}-conformable number of steps forward to forecast. Passed into \code{predict} as \code{n.ahead}.
#'
#' @param level \code{numeric} of the confidence level to use in summarizing the predictions.
#'
#' @param ... Additional parameters passed into \code{predict}. 
#'
#' @return \code{list} with \code{"forecast"}-class with named elements including \code{"mean"}, \code{"lower"}, \code{"upper"}, and \code{"newxreg"} (if provided for prediction) as well as the other elements returned by \code{predict}.
#'
#' @export
#'
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
