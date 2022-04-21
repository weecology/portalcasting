#' @title Evaluate Forecasts
#'
#' @description Evaluate forecasts in the directory, based on id or group, or (if \code{cast_ids = NULL} and \code{cast_groups = NULL}, the default) \code{evaluate_casts} will evaluate all and (if \code{cast_id = NULL} and \code{cast_group = NULL}, the default), \code{evaluate_cast} will evaluate the most recent cast.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
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
evaluate_casts <- function (main     = ".", 
                            settings = directory_settings(), 
                            cast_ids = NULL,
                            quiet    = FALSE, 
                            verbose  = FALSE) {


  casts_to_evaluate <- select_casts(main     = main, 
                                    settings = settings,
                                    cast_ids = cast_ids)

  if (NROW(casts_to_evaluate) == 0) {

    stop("no casts available for request")

  } else {

    cast_ids  <- casts_to_evaluate$cast_id
    ncast_ids <- length(cast_ids)

  }

  for (i in 1:ncast_ids) {

    evaluate_cast(main     = main,
                  settings = settings,
                  cast_id  = cast_ids[i],
                  quiet    = quiet,
                  verbose  = verbose)

  }

  invisible()

}

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
evaluate_cast <- function (main     = ".", 
                           settings = directory_settings(), 
                           cast_id  = NULL,
                           quiet    = FALSE, 
                           verbose  = FALSE) {

  return_if_null(cast_id)

  model_cast     <- read_model_cast(main     = main,
                                    cast_id  = cast_id,
                                    settings = settings)
  casts_metadata <- read_casts_metadata(main     = main,
                                       settings = settings)
  cast_model     <- casts_metadata$model[casts_metadata$cast_id == cast_id]

  cast_model_controls <- model_controls(main     = main,
                                        models   = cast_model,
                                        settings = settings)[[cast_model]]
  cast_model_response <- cast_model_controls$response

  cast_tab <- read_cast_tab(main     = main, 
                            settings = settings,
                            cast_id  = cast_id)

  cast_tab <- add_obs_to_cast_tab(main     = main,  
                                  settings = settings,
                                  cast_tab = cast_tab)
  cast_tab <- add_err_to_cast_tab(main     = main,  
                                  settings = settings,
                                  cast_tab = cast_tab)
  cast_tab <- add_lead_to_cast_tab(main     = main,  
                                   settings = settings,
                                   cast_tab = cast_tab)
  cast_tab <- add_covered_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
  cast_err <- measure_cast_level_error(cast_tab = cast_tab)



  invisible()

}



