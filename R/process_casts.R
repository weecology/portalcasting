read_cast_tab <- function(main = ".", cast_id = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(cast_id)
  lpath <- paste0("cast_id_", cast_id, "_cast_tab.csv")
  cpath <- file_path(main, "casts", lpath, arg_checks)
  if(!file.exists(cpath)){
    stop("cast_id does not have a cast_table")
  }
  read.csv(cpath, stringsAsFactors = FALSE) 

}


#' @title Save cast output to files
#'
#' @description Save out the cast abundances and AIC tables from a given 
#'   set of models.
#'
#' @param cast Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the rodents data. Required to be a 
#'   \code{list}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @export
#'
save_cast_output <- function(cast = NULL, main = ".", 
                             quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  cast_meta <- read_cast_metadata(main = main, quiet = quiet, 
                              arg_checks = arg_checks)
  cast_ids <- cast_meta$cast_id
  if(all(is.na(cast_ids))){
    next_cast_id <- 1
  } else{
    next_cast_id <- max(cast_ids) + 1
  }

  dir_config <- cast$metadata$directory_configuration
  pc_version <- dir_config$setup_portalcasting_version
  new_cast_meta <- data.frame(cast_id = next_cast_id,
                              cast_group = cast$metadata$cast_group,
                              cast_date = cast$metadata$cast_date,
                              model = cast$metadata$models,
                              data_set = cast$metadata$data_sets,
                              end_moon = cast$metadata$end_moon,
                              start_moon = cast$metadata$start_moon,
                              lead_time = cast$metadata$lead_time,
                              portalcasting_version = pc_version,
                              QAQC = TRUE, notes = NA)
  cast_meta <- rbind(cast_meta, new_cast_meta)
  meta_path <- file_path(main = main, sub = "casts", 
                         files = "cast_metadata.csv", arg_checks = arg_checks)
  write.csv(cast_meta, meta_path, row.names = FALSE)

  if(!is.null(cast$metadata)){
    meta_filename <- paste0("cast_id_", next_cast_id, "_metadata.yaml")
    meta_path <- file_path(main = main, sub = "casts", files = meta_filename,
                           arg_checks = arg_checks)
    yams <- as.yaml(cast$metadata)
    writeLines(yams, con = meta_path)
  }
  if(!is.null(cast$cast_tab)){
    cast_tab_filename <- paste0("cast_id_", next_cast_id, "_cast_tab.csv") 
    cast_tab_path <- file_path(main = main, sub = "casts", 
                               files = cast_tab_filename,
                               arg_checks = arg_checks)
    write.csv(cast$cast_tab, cast_tab_path, row.names = FALSE)
  }
  if(!is.null(cast$model_fits)){
    model_fits_filename <- paste0("cast_id_", next_cast_id, 
                                  "_model_fits.RData") 
    model_fits_path <- file_path(main = main, sub = "casts", 
                                 files = model_fits_filename,
                                 arg_checks = arg_checks)
    model_fits <- cast$model_fits
    save(model_fits, file = model_fits_path)
  }
  if(!is.null(cast$casts)){
    casts_filename <- paste0("cast_id_", next_cast_id, "_casts.RData") 
    casts_path <- file_path(main = main, sub = "casts", 
                             files = casts_filename, arg_checks = arg_checks)
    casts <- cast$casts
    save(casts, file = casts_path)
  }
  invisible(NULL)
}

