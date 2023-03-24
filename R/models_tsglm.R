


#' @title Create, Run, and Forecast Multi-model Inference TSGLM Model Runs
#'
#' @description Using the tscount (Liboschik et al. 2017) package to forecast time series of counts. \cr \cr
#'   \code{meta_tsglm}: Combines the model running with the covariate preparation functions for a multi-model \code{\link[tscount]{tsglm}} (from the tscount (Liboschik et al. 2017) package) model. \cr
#'   \code{forecast.tsglm}: A wrapper around the \code{predict} function for tsglm objects that produces a \code{"forecast"}-class object.
#'
#' @param ts Non-negative \code{integer}-conformable vector of rodent abundances to use in forecasting. See \code{\link{prepare_abundance}}.
#'
#' @param metadata \code{list} of model control elements. See \code{\link{prepare_metadata}}.
#'
#' @param covariates \code{data.frame} of covariates used in modeling. See \code{\link{prepare_covariates}}.
#'
#' @param distr \code{character} of the response distribution. See \code{\link[tscount]{tsglm}}.
#'
#' @param link \code{character} of the link funciton. See \code{\link[tscount]{tsglm}}.
#'
#' @param model A named \code{list} of model linear predictors. See \code{\link[tscount]{tsglm}}.
#'
#' @param submodels \code{list} of \code{character} vectors definind the covariates to include in each of the submodels.
#'
#' @param lag \code{integer}-conformable value of the number of timesteps used in a bulk lagging for all covariates in all submodels.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#' 
#' @param object A \code{tsglm}-class object.
#'
#' @param h \code{integer}-conformable number of steps forward to forecast. Passed into \code{predict} as \code{n.ahead}.
#'
#' @param level \code{numeric} of the confidence level to use in summarizing the predictions.
#'
#' @param ... Additional parameters passed into \code{predict}. 
#'
#' @return 
#'   \code{meta_tsglm}: An object of class \code{"tsglm"} with additional elements defining the submodel and lag.
#'   \code{forecast.tsglm}: \code{list} with \code{"forecast"}-class with named elements including \code{"mean"}, \code{"lower"}, \code{"upper"}, and \code{"newxreg"} (if provided for prediction) as well as the other elements returned by \code{predict}.
#'
#' @references 
#'  Liboschik T., K. Fokianos, and R. Fried. 2017. tscount: An R Package for Analysis of Count Time Series Following Generalized Linear Models. Journal of Statistical Software, 82:1-51. \href{https://doi.org/10.18637/jss.v082.i05}{URL}. 
#'
#' @name tsglm models
#'
NULL

#' @rdname tsglm-models
#'
#' @export
#'
meta_tsglm <- function (ts, 
                        model, 
                        distr, 
                        link, 
                        lag, 
                        submodels, 
                        covariates, 
                        metadata, 
                        quiet      = FALSE) {

  nsubmodels    <- length(submodels)
  submodel_fits <- named_null_list(element_names = submodels)

  for (j in 1:nsubmodels) {

    model_name <- paste(submodels[j][[1]], collapse = ", ")
    model_name <- paste0(j, ": ", ifnull(model_name, "<intercept only>"))

    model_message(model = model_name, 
                  quiet = quiet)

    covariates_j  <- unlist(submodels[j])

    xreg <- covariates[covariates$newmoonnumber %in% (metadata$time$historic_newmoonnumbers - lag), covariates_j]

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

#'
#' @name tsglm models
#'
NULL

#' @rdname tsglm-models
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
