

#' @title Combine casts and append them to existing files
#' 
#' @description Combine all new casts (from the tmp subdirectory) into
#'  and append the results to the existing files.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @return \code{list} of [1] \code{"casts"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @export
#'
combine_casts <- function(main = ".", moons = prep_moons(main = main),
                          end_moon = NULL, 
                          cast_date = Sys.Date(), quiet = FALSE){
  messageq("Compiling casts", quiet)
  temp_dir <- sub_paths(main, "tmp")
  pred_dir <- sub_paths(main, "predictions")
  last_moon <- pass_and_call(last_newmoon)
  end_moon <- ifnull(end_moon, last_moon)
  filename_suffix <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  casts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))
  file_ptn <- paste(filename_suffix, "_model_aic.csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)

  aics <- do.call(rbind, lapply(files, read.csv, na.strings = ""))
  
  cast_date <- as.character(cast_date)
  cast_fname <- paste(cast_date, filename_suffix, ".csv", sep = "")
  cast_filename <- file.path(pred_dir, cast_fname)
  aic_fname <- paste(cast_date, filename_suffix, "_model_aic.csv", sep = "")
  model_aic_filename <- file.path(pred_dir, aic_fname)
  append_csv(casts, cast_filename)
  append_csv(aics, model_aic_filename)
  
  list(casts = casts, all_model_aic = aics)
}







#' @title Save cast output to files
#'
#' @description Save out the cast abundances and AIC tables from a given 
#'   set of models.
#'
#' @param all Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "all" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param controls Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "controls" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param model \code{character} value of the name of the model.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @export
#'
save_cast_output <- function(all, controls, model, main){
  metadata <- read_data(main, "metadata");
  temp_dir <- sub_paths(main, "tmp")
  casts <- rbind(all$cast, controls$cast)
  aics <- rbind(all$aic, controls$aic)

  cast_fname <- paste0(model, metadata$cast_type, ".csv")
  cast_path <- file.path(temp_dir, cast_fname)
  write.csv(casts, cast_path, row.names = FALSE)

  aic_fname <- paste0(model, metadata$cast_type, "_model_aic.csv")
  aic_path <- file.path(temp_dir, aic_fname)
  write.csv(aics, aic_path, row.names = FALSE)

}