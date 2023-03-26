#' @title Evaluate Forecasts
#'
#' @description Evaluate forecasts in the directory, based on id or group, or (if \code{cast_ids = NULL} and \code{cast_groups = NULL}, the default) \code{evaluate_casts} will evaluate all and (if \code{cast_id = NULL} and \code{cast_group = NULL}, the default), \code{evaluate_cast} will evaluate the most recent cast.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param cast_id,cast_ids \code{integer} (or integer \code{numeric}) value(s) representing the casts of interest for evaluating, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information. \cr
#'  \code{cast_id} can only be a single value, whereas \code{cast_ids} can be multiple.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information.
#'
#' @return A \code{data.frame}, or \code{list} of \code{data.frame}s.
#'
#' @name evaluate forecasts
#'
#' @export
#'
evaluate_casts <- function (main           = ".", 
                            settings       = directory_settings( ), 
                            cast_ids       = NULL,
                            quiet          = FALSE, 
                            verbose        = FALSE) {


  casts_to_evaluate <- select_casts(main     = main, 
                                    settings = settings,
                                    cast_ids = cast_ids)

  cast_ids  <- casts_to_evaluate$cast_id
  ncast_ids <- length(cast_ids)

  if (NROW(casts_to_evaluate) == 0) {

    stop("no casts available for request")

  }

  out <- named_null_list(element_names = cast_ids)

  messageq("Evaluating casts ...\n", quiet = quiet)

  for (i in 1:ncast_ids) {

    out[[i]] <- evaluate_cast(main     = main,
                              settings = settings,
                              cast_id  = cast_ids[i],
                              quiet    = quiet,
                              verbose  = verbose)

  }

  messageq("... done.\n", quiet = quiet)

  if (settings$save) {

    out_flat <- data.frame(cast_id = names(out)[1], 
                           out[[1]])

    if (ncast_ids > 1) {

      for (i in 2:ncast_ids) { 

        out_flat <- rbind(out_flat, 
                          data.frame(cast_id = names(out)[i], 
                                     out[[i]]))
      
      }

    }
    
    evaluations_file <- file.path(main, settings$subdirectories$forecasts, settings$files$cast_evaluations)

    if (file.exists(evaluations_file)) {

      if (settings$overwrite) {

        messageq("    **", settings$files$cast_evaluations, " updated**", quiet = quiet)
        write.csv(out_flat, file = evaluations_file, row.names = FALSE)

      }

    } else {

      messageq("    **", settings$files$cast_evaluations, " saved**", quiet = quiet)
      write.csv(out_flat, file = evaluations_file, row.names = FALSE)
 
    }


  }

  out

}

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
evaluate_cast <- function (main     = ".", 
                           settings = directory_settings( ), 
                           cast_id  = NULL,
                           quiet    = FALSE, 
                           verbose  = FALSE) {

  return_if_null(x = cast_id)

  cast_tab <- read_cast_tab(main               = main, 
                            settings           = settings,
                            cast_id            = cast_id)
  cast_tab <- add_obs_to_cast_tab(main         = main,  
                                  settings     = settings,
                                  cast_tab     = cast_tab)

  cast_tab$covered <- cast_tab$obs >= cast_tab$lower_pi & cast_tab$obs <= cast_tab$upper_pi 
  cast_tab$error   <- cast_tab$estimate - cast_tab$obs



  RMSE     <- sqrt(mean(cast_tab$error^2, na.rm = TRUE))
  coverage <- mean(cast_tab$covered, na.rm = TRUE)

  data.frame(cast_id  = ids, 
             species  = cast_tab$species[1], 
             model    = cast_tab$model[1], 
#             dataset  = cast_tab$dataset[1], 
             RMSE     = RMSE, 
             coverage = coverage)

}


#' @title Add Observations to a Cast Tab
#' 
#' @description Appends a column of observations to a cast's cast tab. 
#'
#' @details If a model interpolated a data set, \code{add_obs_to_cast_tab} adds the true (non-interpolated) observations so that model predictions are all compared to the same data.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See \code{\link{read_cast_tab}}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{data.frame} of \code{cast_tab} with an additional column. 
#'
#' @name add to cast tab
#'
NULL


#' @rdname add-to-cast-tab
#'
#' @export
#'
add_obs_to_cast_tab <- function (main     = ".", 
                                 settings = directory_settings( ),
                                 cast_tab = NULL) {

  return_if_null(cast_tab)

  dataset <- cast_tab$dataset[1]
  species <- cast_tab$species[1]

  cast_tab$obs   <- NA

  obs <- read_rodents_table(main     = main, 
                            settings = settings,
                            dataset  = dataset)

  obs_cols <- gsub("NA.", "NA", colnames(obs))

  cast_tab$obs <- obs[match(cast_tab$newmoonnumber, obs$newmoonnumber), species]

  cast_tab 

}

