#' @title Evaluate Forecasts
#'
#' @description Evaluate forecasts in the directory, based on id(s). \cr
#'   Current metrics include raw error (which can be used to calculate root mean squared error; RMSE), coverage, log score, and continuous rank probability score (CRPS).
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param cast_id,cast_ids \code{integer} (or integer \code{numeric}) value(s) representing the casts of interest for evaluating, as indexed within the \code{forecasts} subdirectory. See the casts metadata file (\code{casts_metadata.csv}) for summary information. \cr
#'  \code{cast_id} can only be a single value, whereas \code{cast_ids} can be multiple.
#'
#' @return A \code{data.frame} of all cast evaluations at the observation (newmoon) level, \code{\link[base]{invisible}}-ly..
#'
#' @name evaluate forecasts
#'
#' @export
#'
evaluate_casts <- function (main      = ".", 
                            cast_ids  = NULL) {

  settings <- read_directory_settings(main = main)

  casts_to_evaluate <- select_casts(main     = main, 
                                    cast_ids = cast_ids)

  cast_ids  <- casts_to_evaluate$cast_id
  ncast_ids <- length(cast_ids)

  if (NROW(casts_to_evaluate) == 0) {

    stop("no casts available for request")

  }

  out <- named_null_list(element_names = cast_ids)

  messageq("Evaluating casts ...\n", quiet = settings$quiet)

  for (i in 1:ncast_ids) {

    out[[i]] <- evaluate_cast(main    = main,
                              cast_id = cast_ids[i])

  }

  out_flat <- data.frame(cast_id = names(out)[1], 
                         out[[1]])

  if (ncast_ids > 1) {

    for (i in 2:ncast_ids) { 

      out_flat <- rbind(out_flat, 
                        data.frame(cast_id = names(out)[i], 
                                   out[[i]]))
      
    }

  }

  messageq("... done.\n", quiet = quiet)

  write_data(x            = out_flat,
             main         = main,
             subdirectory = settings$subdirectories$forecasts,
             save         = settings$save,
             filename     = settings$files$cast_evaluations,
             quiet        = settings$quiet)

}

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
evaluate_cast <- function (main     = ".", 
                           cast_id  = NULL) {

  settings <- read_directory_settings(main = main)

  return_if_null(x = cast_id)

  cast_tab <- read_cast_tab(main           = main, 
                            cast_id        = cast_id)
  cast_meta <- read_cast_metadata(main     = main, 
                                  cast_id  = cast_id)
  cast_tab <- add_obs_to_cast_tab(main     = main,  
                                  cast_tab = cast_tab)

  cast_tab$covered <- cast_tab$obs >= cast_tab$lower_pi & cast_tab$obs <= cast_tab$upper_pi 
  cast_tab$error   <- cast_tab$estimate - cast_tab$obs
  cast_tab$logs    <- NA
  cast_tab$crps    <- NA

  if (cast_meta$model_controls$scoring_family == "normal") {

    can_score     <- !is.na(cast_tab$obs)

    cast_obs  <- cast_tab$obs[can_score]

    cast_mean   <- cast_tab$estimate[can_score]
    cast_sd   <- (cast_tab$upper_pi[can_score] - cast_tab$estimate[can_score]) / 1.96


    cast_tab$logs[can_score] <- logs(y      = cast_obs,
                                     family = cast_meta$model_controls$scoring_family,
                                     mean   = cast_mean,
                                     sd     = cast_sd)
    cast_tab$crps[can_score] <- crps(y      = cast_obs,
                                     family = cast_meta$model_controls$scoring_family,
                                     mean   = cast_mean,
                                     sd     = cast_sd)

  } else if (cast_meta$model_controls$scoring_family == "poisson") {

    can_score     <- !is.na(cast_tab$obs)

    cast_obs  <- cast_tab$obs[can_score]

    cast_lambda  <- cast_tab$estimate[can_score]


    cast_tab$logs[can_score] <- logs(y      = cast_obs,
                                     family = cast_meta$model_controls$scoring_family,
                                     lambda = cast_lambda)
    cast_tab$crps[can_score] <- crps(y      = cast_obs,
                                     family = cast_meta$model_controls$scoring_family,
                                     lambda = cast_lambda)

  } else if (cast_meta$model_controls$scoring_family == "nbinom") {

    # mu:   mean
    # size: dispersion
    # var:  variance
    # sd:   standard deviation
    #  var  = sd ^ 2
    #  var  = mu + mu^2 / size
    #  size = mu^2 / (var - mu)

    can_score <- !is.na(cast_tab$obs)

    cast_obs  <- cast_tab$obs[can_score]

    cast_mu   <- cast_tab$estimate[can_score]
    cast_sd   <- (cast_tab$upper_pi[can_score] - cast_tab$estimate[can_score]) / 1.96
    cast_var  <- (cast_sd) ^ 2
    cast_size <- (cast_mu ^ 2) / (cast_var - cast_mu)

    cast_tab$logs[can_score] <- logs(y      = cast_obs,
                                     family = cast_meta$model_controls$scoring_family,
                                     mu     = cast_mu,
                                     size   = cast_size)

    cast_tab$crps[can_score] <- crps(y      = cast_obs,
                                     family = cast_meta$model_controls$scoring_family,
                                     mu     = cast_mu,
                                     size   = cast_size)

  } else if (cast_meta$model_controls$scoring_family == "sample") {

    model_cast  <- read_model_cast(main         = main, 
                                   settings     = settings,
                                   cast_id      = cast_id)

    can_score   <- !is.na(cast_tab$obs)

    cast_obs    <- cast_tab$obs[can_score]
    cast_sample <- t(model_cast$sample[ , can_score])

    cast_tab$logs[can_score] <- logs_sample(y   = cast_obs,
                                            dat = cast_sample)

    cast_tab$crps[can_score] <- crps_sample(y   = cast_obs,
                                            dat = cast_sample)

  }

  cast_tab

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
                                 cast_tab = NULL) {

  return_if_null(cast_tab)

  dataset <- cast_tab$dataset[1]
  species <- cast_tab$species[1]

  cast_tab$obs   <- NA

  obs <- read_rodents_table(main     = main, 
                            dataset  = dataset)

  obs_cols <- gsub("NA.", "NA", colnames(obs))

  cast_tab$obs <- obs[match(cast_tab$newmoonnumber, obs$newmoonnumber), species]

  cast_tab 

}

