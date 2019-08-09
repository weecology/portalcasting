#' @title Fill a forecasting directory with basic components.
#'
#' @description Fill the forecasting directory with basic components.
#'
#' @details Arguments input directly here take precedence over those in the 
#'  \code{downloads} \code{list}.
#'
#' @param downloads \code{list} or \code{list} of \code{list}s containing
#'  inputs to \code{\link{download}} for each download to be put into the 
#'  raw subdirectory. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the files and whether they were moved or not.
#' 
#' @param raw_path_predictions \code{character} value indicating the 
#'  folder path within the \code{raw} subdirectory but above the files. For
#'  example, the standard portalcasting directory downloads the historic 
#'  prediction files into \code{"raw\portalPredictions\predictions"}, so
#'  \code{raw_location_predictions = "portalPredictions\predictions"} (as
#'  \code{"raw/"} is implied). 
#'
#' @param overwrite \code{logical} indicator of whether or not any existing
#'  predictions files should be overwritten with the filling.
#'
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{\link{prefab_models}}, which retrieves the 
#'  prefab model names. 
#'
#' @param model_controls Script-writing controls for models not in the prefab
#'  set (\code{c("AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")}).
#'  See \code{\link{model_script_controls}} for details.
#'
#' @return All \code{fill_} functions return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'  create_dir()
#'  fill_dir()
#'  fill_raw()
#'  fill_predictions()
#'  fill_models()
#'  }
#'
#' @export
#'
fill_dir <- function(models = prefab_models(), model_controls = NULL,
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     raw_path_predictions = "portalPredictions/predictions",
                     main = ".", quiet = FALSE, verbose = FALSE, 
                     overwrite = TRUE, cleanup = TRUE){
  fill_raw(downloads, main, quiet, cleanup)
  fill_predictions(raw_path_predictions, main, quiet, verbose, overwrite)
  fill_models(models, main, quiet, model_controls, overwrite)
}

#' @rdname fill_dir
#'
#' @export
#'
fill_models <- function(models = prefab_models(), main = ".", quiet = FALSE, 
                        model_controls = NULL, overwrite = TRUE){
  return_if_null(models)
  controls <- model_script_controls(models, model_controls)
  messageq("Adding models to models subdirectory:", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    model <- models[i]
    ctrl <- controls[[model]]
    m <- write_model(model, ctrl$covariates, ctrl$lag, main, quiet, overwrite)
  } 
}

#' @rdname fill_dir
#'
#' @export
#'
fill_predictions <- function(raw_path_predictions = NULL, main = ".", 
                             quiet = FALSE, verbose = FALSE, 
                             overwrite = TRUE){
  messageq("filling predictions folder", quiet)
  local_raw_folder <- paste0("raw/", raw_path_predictions)
  raw_folder <- file_paths(main, local_paths = local_raw_folder)
  pfiles <- list.files(raw_folder)
  raw_files_local <- paste0(local_raw_folder, "/", pfiles)
  raw_files <- file_paths(main, local_paths = raw_files_local)
  final_folder <- sub_paths(main, specific_subs = "predictions")
  fc <- file.copy(raw_files, final_folder, overwrite)
  messageq(fill_predictions_message(pfiles, fc, verbose))
}



#' @rdname fill_dir
#'
#' @export
#'
fill_raw <- function(downloads = zenodo_downloads(c("1215988", "833438")), 
                     main = ".", quiet = FALSE, cleanup = TRUE){
  return_if_null(downloads)
  if(list_depth(downloads) == 1){
    downloads <- list(downloads)
  }
  messageq("Downloading raw files", quiet)
  ndl <- length(downloads)
  for(i in 1:ndl){
    downloads[[i]]$cleanup <- ifnull(downloads[[i]]$cleanup, cleanup)
    downloads[[i]]$main <- ifnull(downloads[[i]]$main, main)
    downloads[[i]]$quiet <- ifnull(downloads[[i]]$quiet, quiet)
    do.call(download, downloads[[i]])
  }
}

#' @title Create the final message for filling predictions
#'
#' @description Create the final message to be used in 
#'  \code{\link{fill_predictions}} that relays the number, and optionally the
#'  specific names, of the files moved.
#'
#' @param files \code{character} vector of the base file names of the files
#'  that may or may not have been moved.
#'
#' @param movedTF \code{logical} vector indicating if each of the files in 
#'  \code{files} has been moved or not.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the files and whether they were moved or not.
#'
#' @return A \code{character} value of the message to be printed (if desired).
#'
#' @examples
#'  fill_predictions_message("xx.csv", TRUE)
#'  fill_predictions_message(c("xx.csv", "yy.R"), c(TRUE, FALSE), 
#'                           verbose = TRUE)
#'
#' @export
#'
fill_predictions_message <- function(files = NULL, movedTF = NULL, 
                                     verbose = FALSE){
  return_if_null(files)
  moved <- files[movedTF]
  not_moved <- files[!movedTF]
  n_moved <- length(moved)
  n_not_moved <- length(not_moved)
  msg <- NULL
  if(verbose){
    if(n_moved > 0){
      msg <- c(msg, "moved:", moved)
    }
    if(n_not_moved > 0){
      msg <- c(msg, "not moved: ", not_moved)
    }  
  }
  c(msg, paste0(n_moved, " predictions files moved, ", n_not_moved, " not"))
}