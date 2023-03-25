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

  model_cast          <- read_model_cast(main         = main,
                                         cast_id      = cast_id,
                                         settings     = settings)
  casts_metadata      <- read_casts_metadata(main     = main,
                                             settings = settings)
  cast_model          <- casts_metadata$model[casts_metadata$cast_id == cast_id]
  cast_model_controls <- model_controls(main          = main,
                                        models        = cast_model,
                                        settings      = settings)[[cast_model]]
  cast_model_response <- cast_model_controls$response

  cast_tab            <- read_cast_tab(main           = main, 
                                       settings       = settings,
                                       cast_id        = cast_id)

  cast_tab <- add_obs_to_cast_tab(main     = main,  
                                  settings = settings,
                                  cast_tab = cast_tab)
  cast_tab <- add_err_to_cast_tab(main     = main,  
                                  settings = settings,
                                  cast_tab = cast_tab)
  cast_tab <- add_covered_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
  measure_cast_level_error(cast_tab = cast_tab)

}



#' @title Measure Error and Fit Metrics for Forecasts
#' 
#' @description Summarize the cast-level errors or fits to the observations. \cr 
#'  Presently included are root mean square error and coverage.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See \code{\link{read_cast_tab}}.
#'
#' @return \code{data.frame} of metrics for each cast for each species. 
#'
#' @export
#'
measure_cast_level_error <- function (cast_tab = NULL) {


  return_if_null(cast_tab)

  ucast_ids <- unique(cast_tab$cast_id)
  ncast_ids <- length(ucast_ids)
  uspecies  <- unique(cast_tab$species)
  nspecies  <- length(uspecies)
  RMSE      <- rep(NA, ncast_ids * nspecies)
  coverage  <- rep(NA, ncast_ids * nspecies)
  model     <- rep(NA, ncast_ids * nspecies)
  counter   <- 1

  for (i in 1:ncast_ids) {

    for (j in 1:nspecies) {

      ij          <- cast_tab$cast_id == ucast_ids[i] & cast_tab$species == uspecies[j]
      cast_tab_ij <- cast_tab[ij, ]
      err         <- cast_tab_ij$err

      if (!is.null(err)) {

        RMSE[counter] <- sqrt(mean(err^2, na.rm = TRUE))

      }

      covered <- cast_tab_ij$covered

      if (!is.null(covered)) {

        coverage[counter] <- mean(covered, na.rm = TRUE)            

      }

      if (length(cast_tab_ij$model) > 0) {

        model[counter] <- unique(cast_tab_ij$model)

      }

      counter <- counter + 1

    }

  }

  ids <- rep(ucast_ids, each = nspecies)
  spp <- rep(uspecies, ncast_ids)

  data.frame(cast_id  = ids, 
             species  = spp, 
             model    = model, 
             RMSE     = RMSE, 
             coverage = coverage)
}

#' @title Add the Associated Values to a Cast Tab
#' 
#' @description Add values to a cast's cast tab. If necessary components are missing (such as no observations added yet and the user requests errors), the missing components are added. \cr \cr
#'  \code{add_obs_to_cast_tab} appends a column of observations. \cr \cr
#'  \code{add_err_to_cast_tab} adds a column of raw error values. \cr \cr
#'  \code{add_covered_to_cast_tab} appends a \code{logical} column indicating if the observation was within the prediction interval. 
#'
#' @details If a model interpolated a data set, \code{add_obs_to_cast_tab} adds the true (non-interpolated) observations so that model predictions are all compared to the same data.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#' 
#' @param cast_tab A \code{data.frame} of a cast's output. See \code{\link{read_cast_tab}}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{data.frame} of \code{cast_tab} with an additional column or columns if needed. 
#'
#' @name add to cast tab
#'
#' @export
#'
add_err_to_cast_tab <- function (main     = ".", 
                                 settings = directory_settings( ), 
                                 cast_tab = NULL) {


  return_if_null(cast_tab)
  if (is.null(cast_tab$obs)) {

    cast_tab <- add_obs_to_cast_tab(main     = main,
                                    settings = settings,
                                    cast_tab = cast_tab)
  }

  cast_tab$error <- cast_tab$estimate - cast_tab$obs
  cast_tab

}

#' @rdname add-to-cast-tab
#'
#' @export
#'
add_covered_to_cast_tab <- function (main     = ".", 
                                     settings = directory_settings( ), 
                                     cast_tab = NULL) {


  return_if_null(cast_tab)
  if (is.null(cast_tab$obs)) {

    cast_tab <- add_obs_to_cast_tab(main     = main,
                                    settings = settings,
                                    cast_tab = cast_tab)

  }

  cast_tab$covered <- cast_tab$obs >= cast_tab$lower_pi & cast_tab$obs <= cast_tab$upper_pi 
  cast_tab$covered[is.na(cast_tab$obs)] <- NA
  cast_tab

}

#' @rdname add-to-cast-tab
#'
#' @export
#'
add_obs_to_cast_tab <- function (main     = ".", 
                                 settings = directory_settings( ),
                                 cast_tab = NULL) {

  return_if_null(cast_tab)

  cast_tab$obs   <- NA
  cast_dataset   <- gsub("dm_", "", gsub("_interp", "", cast_tab$dataset))
  ucast_dataset  <- unique(cast_dataset)
  ncast_datasets <- length(ucast_dataset)

  for (j in 1:ncast_datasets) {

    obs <- read_rodents_table(main           = main, 
                              settings       = settings,
                              dataset = ucast_dataset[j])


    matches <- which(cast_dataset == ucast_dataset[j])
    nmatches <- length(matches)
    obs_cols <- gsub("NA.", "NA", colnames(obs))

    for (i in 1:nmatches) {

      spot        <- matches[i]
      obs_moon    <- which(obs$newmoonnumber == cast_tab$newmoonnumber[spot])
      obs_species <- which(obs_cols == cast_tab$species[spot])

      if (length(obs_moon) == 1 & length(obs_species) == 1) {

        cast_tab$obs[spot] <- obs[obs_moon, obs_species]

      }

    }

  }

  cast_tab 

}

