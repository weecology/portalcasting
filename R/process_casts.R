most_recent_cast <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  cast_meta <- read_cast_meta(main = main, arg_checks = arg_checks)
  


}

prep_cast_metadata <- function(main = ".", quiet = TRUE, arg_checks = TRUE){
  check_args(arg_checks)
  meta_path <- file_paths(main, "casts/cast_metadata.csv")
  if(!file.exists(meta_path)){
    messageq("  **creating cast_metadata.csv**", quiet)
    cast_meta <- data.frame(cast_id = 0, cast_group = 0, 
                            cast_date = NA, start_moon = NA, end_moon = NA,
                            lead_time = NA, model = NA, data_set = NA,
                            portalcasting_version = NA,
                            QAQC = FALSE, notes = NA)
    write.csv(cast_meta, meta_path, row.names = FALSE)
  }
  read.csv(meta_path, stringsAsFactors = FALSE)
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
#' @param model \code{character} value of the name of the model.
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
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'
#' @export
#'
save_cast_output <- function(cast = NULL, main = ".", 
                             quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  raw_final_meta_local <- paste0("casts/cast_metadata.csv") 
  meta_path <- file_paths(main, raw_final_meta_local)
  cast_meta <- read.csv(meta_path, stringsAsFactors = FALSE)
  cast_ids <- cast_meta$cast_id
  if(all(is.na(cast_ids))){
    next_cast_id <- 1
  } else{
    next_cast_id <- max(cast_ids) + 1
  }
  new_cast_meta <- data.frame(cast_id = next_cast_id,
                              cast_group = cast$metadata$cast_group,
                              cast_date = cast$metadata$cast_date,
                              model = cast$metadata$models,
                              data_set = cast$metadata$data_sets,
                              end_moon = cast$metadata$end_moon,
                              start_moon = cast$metadata$start_moon,
                              lead_time = cast$metadata$lead_time,
                              portalcasting_version = 
                                 cast$metadata$portalcasting_version,
                              QAQC = TRUE, notes = NA)
  cast_meta <- rbind(cast_meta, new_cast_meta)
  write.csv(cast_meta, meta_path, row.names = FALSE)

  if(!is.null(cast$metadata)){
    meta_filename <- paste0("casts/cast_id_", 
                            next_cast_id, "_metadata.yaml")
    meta_path <- file_paths(main, meta_filename)
    yams <- as.yaml(cast$metadata)
    writeLines(yams, con = meta_path)
  }
  if(!is.null(cast$cast_tab)){
    cast_tab_filename <- paste0("casts/cast_id_", 
                                next_cast_id, "_cast_tab.csv") 
    cast_tab_path <- file_paths(main, cast_tab_filename)
    write.csv(cast$cast_tab, cast_tab_path, row.names = FALSE)
  }
  if(!is.null(cast$model_fits)){
    model_fits_filename <- paste0("casts/cast_id_", 
                                next_cast_id, "_model_fits.RData") 
    model_fits_path <- file_paths(main, model_fits_filename)
    model_fits <- cast$model_fits
    save(model_fits, file = model_fits_path)
  }
  if(!is.null(cast$casts)){
    casts_filename <- paste0("casts/cast_id_", 
                                next_cast_id, "_casts.RData") 
    casts_path <- file_paths(main, casts_filename)
    casts <- cast$casts
    save(casts, file = casts_path)
  }
  invisible(NULL)
}

