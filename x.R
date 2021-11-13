#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param filename \code{character} value of the path to the directory config YAML.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param downloads_versions \code{character} vector returned from \code{\link{fill_raw}} of the successfully downloaded versions of downloaded data.


files_control <- function(directory = "portalPredictions",
                          raw_data = "PortalData",
                          filename_moons = "moon_dates.csv",
                          filename_config = "dir_config.yaml", 
                          filename_cov = "covariates.csv", 
                          filename_cov_casts = "covariate_casts.csv",
                          filename_meta = "metadata.yaml", 
                          save = TRUE, overwrite = TRUE, cleanup = TRUE,
                          source_name = "current_archive",
                          append_cast_csv = TRUE){
  
  list(directory = directory, raw_data = raw_data, 
       filename_moons = filename_moons, filename_cov = filename_cov,
       filename_cov_casts = filename_cov_casts,
       filename_config = filename_config, filename_meta = filename_meta,
       save = save, overwrite = overwrite, cleanup = cleanup,
       source_name = source_name, append_cast_csv = append_cast_csv)

}


#'
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'                  data directory. A standard portalcasting directory
#'                  downloads the raw data files into from the PortalData 
#'                  repository, so \code{raw_data = "PortalData"}.
#'
#' @param moons \code{character} name of the file for saving the 
#'                        moons data.
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
#' @param source \code{character} value for the name to give the covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
