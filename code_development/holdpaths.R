#' @title Determine the path for a specific level of a forecasting directory
#'
#' @description Produce paths for a forecasting directory. \cr \cr
#'  \code{main_path} returns the path for the \code{main} folder. \cr \cr
#'  \code{sub_path} returns the path(s) for the \code{sub} folder(s). \cr \cr
#'  \code{file_path} returns the path(s) for the \code{files}. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param sub,subs \code{character} of the name(s) of the sub component(s) of
#'  the directory tree to get the path of. \code{sub} can only take a single
#'  value, \code{subs} can take a vector of multiple values.
#'
#' @param files \code{character} file path(s) within the \code{main}
#'   level of the portalcasting directory.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @return \code{character} value normalized paths 
#'  (see \code{\link{normalizePath}}) . \cr \cr
#'  \code{main_path} normalized path of the \code{main} folder. \cr \cr
#'  \code{subs_path} normalized paths of the \code{sub} folder(s). \cr \cr
#'  \code{file_path} normalized paths of the \code{files}. 
#' 
#' @examples
#'  \donttest{
#'   create_dir()
#'   main_path()
#'   sub_path()
#'   sub_path(subs = "models")
#'   raw_path()
#'   data_path()
#'   models_path()
#'   fits_path()
#'   casts_path()
#'   tmp_path()
#'   file_path(".", "raw", "PortalData/Rodents/Portal_rodent_species.csv")
#'  }
#'
#' @name paths
#'
NULL

#' @rdname paths
#'
#' @export
#'
main_path <- function(main = "."){

  fpath <- file.path(main)
  normalizePath(fpath, mustWork = FALSE)
}

#' @rdname paths
#'
#' @export
#'
sub_path <- function(main = ".", subs = NULL){
  
  return_if_null(subs)
  fpath <- file.path(main, subs)
  normalizePath(fpath, mustWork = FALSE)
}

#' @rdname paths
#'
#' @export
#'
raw_path <- function(main = "."){
  sub_path(main = main, subs = "raw", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
casts_path <- function(main = "."){
  sub_path(main = main, subs = "casts", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
fits_path <- function(main = "."){
  sub_path(main = main, subs = "fits", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
data_path <- function(main = "."){
  sub_path(main = main, subs = "data", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
models_path <- function(main = "."){
  sub_path(main = main, subs = "models", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
tmp_path <- function(main = "."){
  sub_path(main = main, subs = "tmp", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
file_path <- function(main = ".", sub = NULL, files = NULL, 
                       arg_checks = TRUE){
  
  return_if_null(files)
  sub <- ifnull(sub, "")
  fpath <- file.path(main, sub, files)

  normalizePath(fpath, mustWork = FALSE)
}


#' @title Create a control list for files
#'
#' @description Most users will not want or need to change data file and 
#'  folder names or saving conditions, but it is helpful to have them be 
#'  flexible for certain circumstances, and this function gathers them into a
#'  list for higher-in-the-pipeline functions.
#'
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'  data directory. A standard portalcasting directory downloads the raw data
#'  files into from the PortalData repository, so 
#'  \code{raw_data = "PortalData"}.
#'
#' @param filename_moons \code{character} name of the file for saving the 
#'  moons data.
#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param cleanup \code{logical} indicator of whether or not the tmp files
#'  should be cleaned up.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts later use.
#'
#' @param source_name \code{character} value for the name to give the 
#'  covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
#'
#' @return Named \code{list} of names of the folders and files within the
#'  directory structure as well as saving strategies (directly as input).
#'
#' @export
#'
files_control <- function(directory = "portalPredictions",
                          raw_data = "PortalData",
                          filename_moons = "moon_dates.csv",
                          filename_config = "dir_config.yaml", 
                          filename_cov = "covariates.csv", 
                          filename_cov_casts = "covariate_casts.csv",
                          filename_meta = "metadata.yaml", 
                          save = TRUE, overwrite = TRUE, cleanup = TRUE,
                          source_name = "current_archive",
                          append_cast_csv = TRUE, 
                          arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  list(directory = directory, raw_data = raw_data, 
       filename_moons = filename_moons, filename_cov = filename_cov,
       filename_cov_casts = filename_cov_casts,
       filename_config = filename_config, filename_meta = filename_meta,
       save = save, overwrite = overwrite, cleanup = cleanup,
       source_name = source_name, append_cast_csv = append_cast_csv)

}

normalized_file_path <- function (..., fsep = .Platform$file.sep, 
                                  winslash = "\\", mustWork = NA) {
    normalizePath(path = file.path(..., fsep = fsep), winslash = winslash, 
        mustWork = mustWork)
}