#' @title Actually forecast or hindcast
#' 
#' @description Given the data and models are prepared accordingly, run the
#'   forecast or hindcast(s), (optionally) add an ensemble, combine the 
#'   results into the predictions folder.
#'
#' @param options_cast casting options list
#'
#' @return nothing
#'
#' @export
#'
cast <- function(options_cast = cast_options()){

  if (!options_cast$quiet){
    cat("Running models", "\n")
  }
  sapply(models_to_cast(options_cast), source)

  if (!options_cast$quiet){
    cat("Compiling forecasts", "\n")
  }
  predictions_dir <- sub_path(options_cast$tree, "predictions")
  combined <- combine_forecasts(options_cast)

  if (options_cast$ensemble){
    if (!options_cast$quiet){
      cat("Creating ensemble model", "\n")
    }
    ensemble <- add_ensemble(options_cast)
  }
}



