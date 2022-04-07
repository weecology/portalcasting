

#' @title Evaluate Forecasts
#'
#' @description Evaluate forecasts in the directory, based on id or group, or (if \code{cast_ids = NULL} and \code{cast_groups = NULL}, the default) \code{evaluate_casts} will evaluate all and (if \code{cast_id = NULL} and \code{cast_group = NULL}, the default), \code{evaluate_cast} will evaluate the most recent cast.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param cast_group,cast_groups \code{integer} (or integer \code{numeric}) value(s) of the cast group(s) to evaluate, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information. \cr
#'  \code{cast_group} can only be a single value, whereas \code{cast_groups} can be multiple.
#'
#' @param cast_id,cast_ids \code{integer} (or integer \code{numeric}) value(s) representing the casts of interest for evaluating, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information. \cr
#'  \code{cast_id} can only be a single value, whereas \code{cast_ids} can be multiple.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @name evaluate forecasts
#'
#' @export
#'
evaluate_casts <- function (main        = ".", 
                            settings    = directory_settings(), 
                            cast_groups = NULL, 
                            cast_ids    = NULL,
                            quiet       = FALSE, 
                            verbose     = FALSE) {



  invisible()

}

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
evaluate_cast <- function (main       = ".", 
                           settings   = directory_settings(), 
                           cast_group = NULL, 
                           cast_id    = NULL,
                           quiet      = FALSE, 
                           verbose    = FALSE) {



  invisible()

}



