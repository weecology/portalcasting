
#' @title Prepare the directory options for a portalcasting directory
#'
#' @description Create a list of control options for the directory set-up
#'
#' @param base name of the base directory where the forecasting 
#'   directory should exist (will be created if it doesn't)
#'
#' @param main name of the portalcasting directory
#'
#' @param subs names of the portalcasting subdirectories
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return list of settings controlling the directory structure creeatio
#'
#' @export
#'
dir_options <- function(base = "~", main = "forecasting", subs = subdirs(),
                        quiet = FALSE){
  tree <- dirtree(base, main, subs)
  list(tree = tree, quiet = quiet)
}

#' @title Prepare the data options for moon data
#'
#' @description Create a list of control options for the moon data
#'
#' @param n_future_moons integer value for the number of future moons to add
#'
#' @param fdate date from which future is defined, typically today's date.
#'   Required if \code{n_future_moons > 0}
#'
#' @param append_missing_to_raw logical indicating if the raw data should be
#'   updated by appending the missing moon dates
#'
#' @param save logical if the data should be saved out
#'
#' @param filename the name of the file for the saving.
#'
#' @param tree directory tree
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return control options list for moons
#'
#' @export
#'
moons_options <- function(n_future_moons = 12, fdate = today(), 
                          append_missing_to_raw = TRUE, save = TRUE,
                          filename = "moons.csv", tree = dirtree(), 
                          quiet = FALSE){
  list(n_future_moons = n_future_moons, fdate = fdate, 
       append_missing_to_raw = append_missing_to_raw, save = save,
       filename = filename, tree = tree, quiet = quiet)
}

#' @title Prepare the data options for rodent data
#'
#' @description Create a list of control options for the rodent data
#'
#' @param type "all" or "controls"
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param drop_spp species names to drop from the table
#'
#' @param min_plots minimum number of plots surveyed for a survey to be used
#'
#' @param level for get_rodent_data, automatically set by type
#'
#' @param treatment for get_rodent_data, automatically set by type
#'
#' @param length for get_rodent_data, automatically set by type
#'
#' @param output for get_rodent_data, automatically set by type
#'
#' @param save logical if the data should be saved out
#'
#' @param filename the name of the file for the saving, automatically set by
#'   type
#'
#' @param tree directory tree
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return control options list for rodents
#'
#' @export
#'
rodents_options <- function(type = NULL, start = 217, drop_spp = "PI", 
                            min_plots = 24, level = "Site", 
                            treatment = NULL, length = "all",
                            output = "abundance", save = TRUE, 
                            filename = "all.csv", tree = dirtree(), 
                            quiet = FALSE){
  if (!is.null(type)){
    if (type == "all"){
      level <- "Site"
      length <- "all"
      output <- "abundance"
      filename <- "all.csv"
    }
    if (type == "controls"){
      level <- "Treatment"
      length <- "Longterm"
      output <- "abundance"
      treatment <- "control"
      filename <- "controls.csv"
    }  
  }
  list(start = start, drop_spp = drop_spp, min_plots = min_plots, 
       level = level, length = length, output = output, save = save,
       filename = filename, treatment = treatment, tree = tree, quiet = quiet)
}


#' @title Prepare the data options for covariates data
#'
#' @description Create a list of control options for the covariates data
#'
#' @param historical logical indicator whether or historical covariates are
#'   to be included
#'
#' @param forecasts logical indicator whether or forecasted covariates are
#'   to be included
#'
#' @param fdate date from which future is defined, typically today's date.
#'
#' @param yr year of today
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param lead_time number of moons into the future the rodents are forecast
#'
#' @param min_lag the minimum lag time used in any model
#'
#' @param fcast_nms newmoon numbers to be forecast (for the rodents, i.e. 
#'   pre-lag)
#'
#' @param nfcnm number of forecast newmoons
#'
#' @param append_fcast_csv logical if the new forecast should be appended to
#'   the historical forecasts for the purposes of hindcasting
#'
#' @param hist_fcast_file name of the file where the historical 
#'   covariate forecasts are held
#'
#' @param source_name character value for the name to give the covariaate
#'   forecast. Currently is "current_archive". Previous to "current_archive",
#'   the data were retroactively filled in and are given the source name
#'   "retroactive"
#'
#' @param save logical if the data should be saved out
#'
#' @param filename the name of the file for the saving
#'
#' @param tree directory tree
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return control options list for covariates
#'
#' @export
#'
covariates_options <- function(historical = TRUE, forecasts = TRUE, 
                               fdate = today(), 
                               yr = as.numeric(format(today(), "%Y")),
                               start = 217, lead_time = 12,
                               min_lag = 6, fcast_nms = NULL, nfcnm = 0,
                               append_fcast_csv = TRUE, 
                               hist_fcast_file = "covariate_forecasts.csv",
                               source_name = "current_archive",
                               save = TRUE, filename = "covariates.csv", 
                               tree = dirtree(), quiet = FALSE){
  list(historical = historical, forecasts = forecasts, fdate = fdate,
       yr = yr, start = start, lead_time = lead_time, min_lag = min_lag, 
       fcast_nms = fcast_nms, nfcnm = nfcnm, 
       append_fcast_csv = append_fcast_csv, hist_fcast_file = hist_fcast_file,
       source_name = source_name, save = save, filename = filename, 
       tree = tree, quiet = quiet)
}

#' @title Prepare the data options for a portalcasting directory
#'
#' @description Create a list of lists of control options for filling the data
#'   directory
#'
#' @param base name of the base directory where the forecasting 
#'   directory should exist (will be created if it doesn't)
#'
#' @param main name of the portalcasting directory
#'
#' @param subs names of the portalcasting subdirectories
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @param fdate date from which future is defined, typically today's date.
#'
#' @param append_missing_to_raw logical indicating if the missing moon dates 
#'   should be appended to the raw data file (should be TRUE to allow the
#'   imported portalr functions to work properly)
#'
#' @param m_save logical if the moons data should be saved out
#'
#' @param m_filename the name of the file for saving moons data
#'
#'
#' @param type "all" or "controls"
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param drop_spp species names to drop from the table
#'
#' @param min_plots minimum number of plots surveyed for a survey to be used
#'
#' @param level for get_rodent_data, automatically set by type
#'
#' @param treatment for get_rodent_data, automatically set by type
#'
#' @param length for get_rodent_data, automatically set by type
#'
#' @param output for get_rodent_data, automatically set by type
#'
#' @param r_save logical if the data should be saved out
#'
#' @param r_filename the name of the file to save the rodent data in
#'
#'
#' @param historical logical indicator whether or historical covariates are
#'   to be included
#'
#' @param forecasts logical indicator whether or forecasted covariates are
#'   to be included
#'
#' @param yr year of today
#'
#' @param lead_time number of moons into the future the rodents are forecast
#'
#' @param min_lag the minimum lag time used in any model
#'
#' @param fcast_nms newmoon numbers to be forecast for covariates
#'
#' @param nfcnm number of forecast newmoons for covariates
#'
#' @param append_fcast_csv logical if the new forecast should be appended to
#'   the historical forecasts for the purposes of hindcasting
#'
#' @param hist_fcast_file name of the file where the historical 
#'   covariate forecasts are held
#'
#' @param source_name character value for the name to give the covariaate
#'   forecast. Currently is "current_archive". Previous to "current_archive",
#'   the data were retroactively filled in and are given the source name
#'   "retroactive"
#'
#' @param c_save logical if the covariate data should be saved out
#'
#' @param c_filename the name of the covariate file for the saving
#'
#'
#' @param models names of model scripts to include
#'
#' @return list of control options lists
#'
#' @export
#'
data_options <- function(base = "~", main = "forecasting", subs = subdirs(),
                         quiet = FALSE, fdate = today(), 
                         append_missing_to_raw = TRUE, m_save = TRUE, 
                         m_filename = "moons.csv", type = NULL, start = 217,
                         drop_spp = "PI", min_plots = 24, level = "Site", 
                         treatment = NULL, length = "all",
                         output = "abundance", r_save = TRUE, 
                         r_filename = "all.csv",
                         historical = TRUE, forecasts = TRUE, 
                         yr = as.numeric(format(today(), "%Y")),
                         lead_time = 12, min_lag = 6, 
                         fcast_nms = NULL, nfcnm = 0,
                         append_fcast_csv = TRUE, 
                         hist_fcast_file = "covariate_forecasts.csv",
                         source_name = "current_archive",
                         c_save = TRUE, c_filename = "covariates.csv",
                         models = models()){

  tree <- dirtree(base, main, subs)
  list(
    moons = moons_options(n_future_moons = lead_time, fdate = fdate,
                          append_missing_to_raw = append_missing_to_raw,
                          save = m_save, filename = m_filename,
                          quiet = quiet, tree = tree),
    rodents = rodents_options(type = type, start = start, 
                              drop_spp = drop_spp, min_plots = min_plots, 
                              level = level, treatment = treatment, 
                              length = length, output = output,
                              save = r_save, filename = r_filename,
                              quiet = quiet, tree = tree),
    covariates = covariates_options(historical = historical, 
                                    forecasts = forecasts, fdate = fdate, 
                                    yr = yr, start = start,
                                    lead_time = lead_time, 
                                    min_lag = min_lag, 
                                    fcast_nms = fcast_nms, nfcnm = nfcnm,
                                    append_fcast_csv = append_fcast_csv, 
                                    hist_fcast_file = hist_fcast_file,
                                    source_name = source_name,
                                    save = c_save, filename = c_filename,
                                    quiet = quiet, tree = tree)
      )
}



#' @title Prepare all of the options for creating a portalcasting directory
#'
#' @description Create a list of lists (of lists) of control options for the 
#'   directory set-up
#'
#' @param base name of the base directory where the forecasting 
#'   directory should exist (will be created if it doesn't)
#'
#' @param main name of the portalcasting directory
#'
#' @param subs names of the portalcasting subdirectories
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @param fdate date from which future is defined, typically today's date.
#'
#' @param append_missing_to_raw logical indicating if the missing moon dates 
#'   should be appended to the raw data file (should be TRUE to allow the
#'   imported portalr functions to work properly)
#'
#' @param m_save logical if the moons data should be saved out
#'
#' @param m_filename the name of the file for saving moons data
#'
#'
#' @param type "all" or "controls"
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param drop_spp species names to drop from the table
#'
#' @param min_plots minimum number of plots surveyed for a survey to be used
#'
#' @param level for get_rodent_data, automatically set by type
#'
#' @param treatment for get_rodent_data, automatically set by type
#'
#' @param length for get_rodent_data, automatically set by type
#'
#' @param output for get_rodent_data, automatically set by type
#'
#' @param r_save logical if the data should be saved out
#'
#' @param r_filename the name of the file to save the rodent data in
#'
#'
#' @param historical logical indicator whether or historical covariates are
#'   to be included
#'
#' @param forecasts logical indicator whether or forecasted covariates are
#'   to be included
#'
#' @param yr year of today
#'
#' @param lead_time number of moons into the future the rodents are forecast
#'
#' @param min_lag the minimum lag time used in any model
#'
#' @param fcast_nms newmoon numbers to be forecast for covariates
#'
#' @param nfcnm number of forecast newmoons for covariates
#'
#' @param append_fcast_csv logical if the new forecast should be appended to
#'   the historical forecasts for the purposes of hindcasting
#'
#' @param hist_fcast_file name of the file where the historical 
#'   covariate forecasts are held
#'
#' @param source_name character value for the name to give the covariaate
#'   forecast. Currently is "current_archive". Previous to "current_archive",
#'   the data were retroactively filled in and are given the source name
#'   "retroactive"
#'
#' @param c_save logical if the covariate data should be saved out
#'
#' @param c_filename the name of the covariate file for the saving
#'
#'
#' @param models names of model scripts to include
#'
#' @return list of control options lists
#'
#' @export
#'
all_options <- function(base = "~", main = "forecasting", subs = subdirs(), 
                        quiet = FALSE, fdate = today(), 
                        append_missing_to_raw = TRUE, m_save = TRUE, 
                        m_filename = "moons.csv", type = NULL, start = 217,
                        drop_spp = "PI", min_plots = 24, level = "Site", 
                        treatment = NULL, length = "all",
                        output = "abundance", r_save = TRUE, 
                        r_filename = "all.csv",
                        historical = TRUE, forecasts = TRUE, 
                        yr = as.numeric(format(today(), "%Y")),
                        lead_time = 12, min_lag = 6, 
                        fcast_nms = NULL, nfcnm = 0,
                        append_fcast_csv = TRUE, 
                        hist_fcast_file = "covariate_forecasts.csv",
                        source_name = "current_archive",
                        c_save = TRUE, c_filename = "covariates.csv",
                        models = models()){

  list(
    options_dir = dir_options(base = base, main = main, subs = subs,
                              quiet = quiet),
    options_data = data_options(base = base, main = main, subs = subs,
                                quiet = quiet, fdate = fdate,
                                append_missing_to_raw = append_missing_to_raw,
                                m_save = m_save, m_filename = m_filename,
                                type = type, start = start, 
                                drop_spp = drop_spp, min_plots = min_plots, 
                                level = level, treatment = treatment, 
                                length = length, output = output,
                                r_save = r_save, r_filename = r_filename,
                                historical = historical, 
                                forecasts = forecasts, yr = yr, 
                                lead_time = lead_time, min_lag = min_lag, 
                                fcast_nms = fcast_nms, nfcnm = nfcnm,
                                append_fcast_csv = append_fcast_csv, 
                                hist_fcast_file = hist_fcast_file,
                                source_name = source_name,
                                c_save = c_save, c_filename = c_filename)
  )

}