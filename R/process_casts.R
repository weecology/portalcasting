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