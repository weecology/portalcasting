

#' @title Evaluate Forecasts
#'
#' @description Evaluate forecasts in the directory, based on id or group, or (if \code{cast_ids = NULL} and \code{cast_groups = NULL} , the default) evaluate all.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast group to combine with an ensemble. If \code{NULL} (default), the most recent cast group is ensembled. 
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting ensembling, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
evaluate_casts <- function (main           = ".", 
                            settings       = directory_settings(), 
                            cast_groups    = NULL, 
                            cast_ids       = NULL,
                            quiet          = FALSE, 
                            verbose        = FALSE) {



  invisible()

}


