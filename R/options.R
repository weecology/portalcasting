#' @title Prepare options lists for a portalcasting directory
#'
#' @description Suite of functions used to generate options \code{list}s 
#'  that control the setup and use of a portalcasting directory: \cr \cr
#'  \code{all_options} creates an \code{all_options} \code{list} that is a
#'  full hierarchy of portalcasting options \code{list}s:
#'  \code{options_dir}, \code{options_PortalData}, \code{options_data}, 
#'  \code{options_predictions}, \code{options_models}, and 
#'  \code{options_cast}.
#'
#' @param base \code{character} name of the base folder where the directory 
#'   should or does exist. Default \code{"."} (working directory). Will be 
#'   created if it doesn't exist.
#'
#' @param main \code{character} name of the main folder within the base that
#'   contains the subdirectories. Default \code{""} (no main level included). 
#'
#' @param subs \code{character} vector naming the specific subdirectories
#'   within the portalcasting directory tree. Default \code{subdirs(type = 
#'   "portalcasting")} sets the subdirectories as \code{"predictions"}, 
#'   \code{"models"}, \code{"PortalData"}, \code{"data"}, and \code{"tmp"}. 
#'   It is generally not advised to change the subdirectories. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param cast_date \code{Date} from which future is defined, typically 
#'   today's date (using \code{\link{today}}).
#'
#' @param append_missing_to_raw \code{logical} indicator dictating if the 
#'   missing moon dates should be appended to the raw data file (should be 
#'   \code{TRUE} to allow the imported portalr functions to work properly).
#'
#' @param m_save \code{logical} indicator controlling if the moons data should 
#'   be saved out.
#'
#' @param m_filename \code{character} name of the file for saving moons data.
#'
#' @param tmnt_type Treatment type: \code{"all"} or \code{"controls"}.
#'
#' @param start \code{integer} (or integer \code{numeric}) newmoon number of 
#'   the first sample to be included. Default value is \code{217}, 
#'   corresponding to \code{1995-01-01}.
#'
#' @param end \code{integer} (or integer \code{numeric}) newmoon number of the
#'   last sample to be included. Default value is \code{NULL}, which equates
#'   to the most recently included sample. If \code{cast_type} is 
#'   \code{"hindcasts"} and \code{end} is \code{NULL}, default becomes 
#'   \code{490:403}, corresponding to \code{2017-01-01} back to
#'   \code{2010-01-01}.
#'
#' @param hind_step \code{integer} (or integer \code{numeric}) iteration 
#'   parameter used to work across the input values for \code{end}. Default is
#'   \code{1}, which is likely all it should be run using.
#'
#' @param drop_spp \code{character}-valued vector of species names to drop 
#'   from the forecasting.
#'
#' @param min_plots \code{integer} (or integer \code{numeric}) of the minimum 
#'   number of plots surveyed for a survey to be used.
#'
#' @param min_traps \code{integer} (or integer \code{numeric}) of the minimum 
#'   number of traps trapped for a plot to be used.
#'
#' @param level \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param treatment \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param plots \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param output \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param r_save \code{logical} value indicating if the rodent data should be 
#'   saved out.
#'
#' @param r_filename \code{character} name of the file to save the rodent data
#'   in.
#'
#' @param cov_hist \code{logical} indicator of whether or not historical 
#'   covariates are to be included.
#'
#' @param cov_fcast \code{logical} indicator whether or not forecasted 
#'   covariates are to be included.
#'
#' @param yr \code{numeric} value of the year of today's date.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) number of moons
#'   into the future the rodents are to be forecast.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'   covariate lag time used in any model.
#'
#' @param fcast_nms \code{integer} (or integer \code{numeric}) vector of 
#'   newmoon numbers to be forecast for covariates.
#'
#' @param nfcnm \code{integer} (or integer \code{numeric}) number of forecast 
#'   newmoons for covariates.
#'
#' @param append_fcast_csv \code{logical} indicator controlling if the new 
#'   forecast should be appended to the historical forecasts for the purposes 
#'   of hindcasting.
#'
#' @param hist_fcast_file \code{character} name of the file where the 
#'   historical covariate forecasts are held.
#'
#' @param source_name \code{character} value for the name to give the 
#'   covariaate forecast. Currently is \code{"current_archive"}. Previous to
#'   \code{"current_archive"}, the data were retroactively filled in and are 
#'   given the source name \code{"retroactive"}.
#'
#' @param c_save \code{logical} indicator for if the covariate data should be 
#'   saved out.
#'
#' @param c_filename \code{character} value for the name of the covariate file 
#'   for the saving if \code{c_save} is \code{TRUE}.
#'
#' @param cast_type -cast type: \code{"forecasts"} or \code{"hindcasts"}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param meta_save \code{logical} indicator for if the metadata should be
#'   saved out
#'
#' @param meta_filename \code{character} value for the name of the metadata 
#'   file for the saving if \code{c_save} is \code{TRUE}.
#'
#' @param download_existing_predictions \code{logical} indicator for if the 
#'   existing predictions files should be retrieved from the 
#'   \href{https://github.com/weecology/portalPredictions}{portalPredictions 
#'   repo}.
#'
#' @param models \code{character} vector of class \code{models} of the names 
#'   of models to include in a cast of the pipeline.
#'
#' @param ensemble \code{logical} indicator of whether to create an ensemble
#'   model.
#'
#' @param version \code{character} values of the version number or 
#'   \code{"latest"} (default) for the Portal Data to be download.
#'
#' @param from_zenodo \code{logical} indicator of whether or not the Portal 
#'   Data should come from Zenodo (come from GitHub if not).
#'
#' @param to_cleanup \code{character} vector of subdirectory names to
#'   cleanup after completion of casting.
#'
#' @return \code{all_options}: class-\code{all_options} \code{list} of control
#'   options lists (\code{options_dir} generated by \code{dir_options}, 
#'   \code{options_PortalData} generated by 
#'   \code{PortalData_options}, \code{options_data} generated by 
#'   \code{data_options}, \code{options_predictions} generated by
#'   \code{predictions_options}, \code{options_models} generated by
#'   \code{models_options}, and \code{options_cast} generated by 
#'   \code{cast_options}.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' all_options()
#' dir_options()
#' PortalData_options()
#' data_options()
#' moons_options()
#' rodents_options()
#' covariates_options()
#' metadata_options()
#' predictions_options()
#' models_options()
#' cast_options()
#' }
#'
#' @export
#'
all_options <- function(base = ".", main = "", 
                        subs = subdirs(subs_type = "portalcasting"), 
                        quiet = FALSE, cast_date = today(), 
                        append_missing_to_raw = TRUE, m_save = TRUE, 
                        m_filename = "moons.csv", tmnt_type = NULL,
                        start = 217, end = NULL, hind_step = 1, 
                        drop_spp = "PI", min_plots = 24, min_traps = 1,
                        level = "Site", treatment = NULL, plots = "all",
                        output = "abundance", r_save = TRUE, 
                        r_filename = "all.csv",
                        cov_hist = TRUE, cov_fcast = TRUE, 
                        yr = as.numeric(format(today(), "%Y")),
                        lead_time = 12, min_lag = 6, 
                        fcast_nms = NULL, nfcnm = 0,
                        append_fcast_csv = TRUE, 
                        hist_fcast_file = "covariate_forecasts.csv",
                        source_name = "current_archive",
                        c_save = TRUE, c_filename = "covariates.csv",
                        cast_type = "forecasts",
                        confidence_level = 0.9, meta_save = TRUE, 
                        meta_filename = "metadata.yaml",
                        download_existing_predictions = FALSE,
                        models = model_names(set = "prefab"), ensemble = TRUE,
                        version = "latest", from_zenodo = TRUE,
                        to_cleanup = c("tmp", "PortalData")){

  check_argsX()
  if (is.null(end) & cast_type == "hindcasts"){
    end <- 490:403
  }
  list(
    options_dir = 
      dir_options(base = base, main = main, subs = subs, quiet = quiet, 
                  to_cleanup = to_cleanup),
    options_PortalData = 
      PortalData_options(base = base, main = main, subs = subs, quiet = quiet,
                         version = version, from_zenodo = from_zenodo),
    options_data = 
      data_options(base = base, main = main, subs = subs, quiet = quiet, 
                   cast_date = cast_date,
                   append_missing_to_raw = append_missing_to_raw,
                   m_save = m_save, m_filename = m_filename,
                   tmnt_type = tmnt_type, start = start, end = end, 
                   hind_step = hind_step, drop_spp = drop_spp, 
                   min_plots = min_plots, level = level, 
                   treatment = treatment, plots = plots, output = output,
                   r_save = r_save, r_filename = r_filename,
                   cov_hist = cov_hist, cov_fcast = cov_fcast, yr = yr, 
                   lead_time = lead_time, min_lag = min_lag, 
                   fcast_nms = fcast_nms, nfcnm = nfcnm,
                   append_fcast_csv = append_fcast_csv, 
                   hist_fcast_file = hist_fcast_file,
                   source_name = source_name, c_save = c_save, 
                   c_filename = c_filename, cast_type = cast_type,
                   confidence_level = confidence_level,  
                   meta_save = meta_save, meta_filename = meta_filename),
    options_predictions = 
      predictions_options(base = base, main = main, subs = subs,
                download_existing_predictions = download_existing_predictions, 
                quiet = quiet),
    options_models = 
      models_options(base = base, main = main, subs = subs, quiet = quiet, 
                     models = models),
    options_cast = 
      cast_options(base = base, main = main, subs = subs, quiet = quiet, 
                   models = models, cast_type = cast_type, 
                   cast_date = cast_date, ensemble = ensemble, start = start, 
                   end = end, hind_step = hind_step, min_plots = min_plots, 
                   min_traps = min_traps)
  ) %>%
  classy(c("all_options", "list"))

}

#' @rdname all_options
#'
#' @description \code{dir_options} creates a \code{dir_options} \code{list} of
#'   control options for the directory set-up.
#'
#' @return \code{dir_options}: a \code{dir_options} \code{list} of options for 
#'   the directory structure creation.
#'
#' @export
#'
dir_options <- function(base = ".", main = "", 
                        subs = subdirs(subs_type = "portalcasting"),
                        quiet = FALSE, to_cleanup = c("tmp", "PortalData")){
  check_argsX()
  tree <- dirtree(base, main, subs)
  list(tree = tree, quiet = quiet, to_cleanup = to_cleanup) %>%
  classy(c("dir_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{PortalData_options} creates a \code{PortalData_options}
#'   \code{list} of options for filling the PortalData subdirectory.
#'
#' @return \code{PortalData_options}: a \code{PortalData_options} \code{list} 
#'   of settings controlling the population of the PortalData subdirectory.
#'
#' @export
#'
PortalData_options <- function(base = ".", main = "", 
                               subs = subdirs(subs_type = "portalcasting"), 
                               quiet = FALSE,
                               version = "latest", from_zenodo = TRUE){
  check_argsX()
  tree <- dirtree(base, main, subs)
  list(tree = tree, version = version, from_zenodo = from_zenodo, 
              quiet = quiet) %>%
  classy(c("PortalData_options", "list"))
}


#' @rdname all_options
#'
#' @description \code{data_options} creates a \code{data_options} \code{list} 
#'   of \code{list}s of options for filling the data subdirectory.
#'
#' @return \code{data_options}: \code{data_options} \code{list} of control 
#'   options \code{list}s (\code{moons} generated by \code{moons_options}, 
#'   \code{rodents} generated by \code{rodents_options}, \code{covariates},
#'   generated by \code{covariates_options}, and \code{metadata} generated by
#'   \code{metadata_options}, as well as items \code{cast_type}, \code{tree},
#'   and \code{quiet} for simple use with all or ancillary data..
#'
#' @export
#'
data_options <- function(base = ".", main = "", 
                         subs = subdirs(subs_type = "portalcasting"),
                         quiet = FALSE, cast_date = today(), 
                         append_missing_to_raw = TRUE, m_save = TRUE, 
                         m_filename = "moons.csv", tmnt_type = NULL, 
                         start = 217, end = NULL, hind_step = 1, 
                         drop_spp = "PI", min_plots = 24, 
                         level = "Site", treatment = NULL, plots = "all",
                         output = "abundance", r_save = TRUE, 
                         r_filename = "all.csv",
                         cov_hist = TRUE, cov_fcast = TRUE, 
                         yr = as.numeric(format(today(), "%Y")),
                         lead_time = 12, min_lag = 6, 
                         fcast_nms = NULL, nfcnm = 0,
                         append_fcast_csv = TRUE, 
                         hist_fcast_file = "covariate_forecasts.csv",
                         source_name = "current_archive",
                         c_save = TRUE, c_filename = "covariates.csv",
                         cast_type = "forecasts",
                         confidence_level = 0.9, 
                         meta_save = TRUE, meta_filename = "metadata.yaml"){
  check_argsX()
  if (is.null(end) & cast_type == "hindcasts"){
    end <- 490:403
  }
  tree <- dirtree(base, main, subs)
  list(
    moons = 
      moons_options(n_future_moons = lead_time, cast_date = cast_date,
                    append_missing_to_raw = append_missing_to_raw,
                    save = m_save, filename = m_filename, quiet = quiet, 
                    tree = tree),
    rodents =
      rodents_options(cast_type = cast_type, tmnt_type = tmnt_type, 
                      start = start, end = end, hind_step = hind_step, 
                      drop_spp = drop_spp, min_plots = min_plots, 
                      level = level, treatment = treatment, plots = plots, 
                      output = output, save = r_save, filename = r_filename,
                      quiet = quiet, tree = tree),
    covariates =
      covariates_options(cast_type = cast_type, cov_hist = cov_hist, 
                         cov_fcast = cov_fcast, cast_date = cast_date, 
                         yr = yr, start = start, end = end, 
                         hind_step = hind_step, lead_time = lead_time, 
                         min_lag = min_lag, fcast_nms = fcast_nms, 
                         nfcnm = nfcnm,
                         append_fcast_csv = append_fcast_csv, 
                         hist_fcast_file = hist_fcast_file,
                         source_name = source_name, save = c_save, 
                         filename = c_filename, quiet = quiet, tree = tree),
    metadata =
      metadata_options(cast_date = cast_date, cast_type = cast_type,
                       confidence_level = confidence_level,
                       lead_time = lead_time, save = meta_save,
                       filename = meta_filename, quiet = quiet, tree = tree),
    cast_type = cast_type, 
    quiet = quiet, 
    tree = tree) %>%
  classy(c("data_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{moons_options} creates a \code{moons_options} 
#'   \code{list}of options for the moons data.
#'
#' @param n_future_moons \code{integer} (or integer \code{numeric}) value for 
#'   the number of future moons to add.
#'
#' @param save \code{logical} indicator if the specific data should be saved 
#'   out.
#'
#' @param filename \code{character} name of the file for the saving.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @return \code{moons_options}: a \code{moons_options} \code{list} of 
#'   settings controlling the moon data creation.
#'
#' @export
#'
moons_options <- function(n_future_moons = 12, cast_date = today(), 
                          append_missing_to_raw = TRUE, save = TRUE,
                          filename = "moons.csv", tree = dirtree(), 
                          quiet = FALSE){
  check_argsX()                         
  list(n_future_moons = n_future_moons, cast_date = cast_date, 
       append_missing_to_raw = append_missing_to_raw, save = save,
       filename = filename, tree = tree, quiet = quiet, class = "moons") %>%
  classy(c("moons_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{rodents_options} creates a \code{rodents_options} 
#'   \code{list} of options for the rodents data.
#'
#' @return \code{rodents_options}: a \code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation.
#'
#' @export
#'
rodents_options <- function(cast_type = "forecasts", tmnt_type = NULL, 
                            start = 217, end = NULL, hind_step = 1, 
                            drop_spp = "PI", min_plots = 24, level = "Site", 
                            treatment = NULL, plots = "all",
                            output = "abundance", save = TRUE, 
                            filename = "all.csv", tree = dirtree(), 
                            quiet = FALSE){
  check_argsX() 
  if (!is.null(tmnt_type)){
    if (tmnt_type == "all"){
      level <- "Site"
      plots <- "all"
      output <- "abundance"
      filename <- "all.csv"
    }
    if (tmnt_type == "controls"){
      level <- "Treatment"
      plots <- "Longterm"
      output <- "abundance"
      treatment <- "control"
      filename <- "controls.csv"
    }  
  }
  list(cast_type = cast_type, start = start, end = end, 
       hind_step = hind_step, drop_spp = drop_spp, min_plots = min_plots, 
       tmnt_type = tmnt_type, level = level, plots = plots, output = output,
       save = save, filename = filename, treatment = treatment, tree = tree,
       quiet = quiet, class = "rodents") %>%
  classy(c("rodents_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{covariates_options} creates a \code{covariates_options}
#'   \code{list}of options for the covariates data.
#'
#' @return \code{covariates_options}: a \code{covariates_options} \code{list} 
#'   of settings controlling the covariates data creation.
#'
#' @export
#'
covariates_options <- function(cast_type = "forecasts", cov_hist = TRUE, 
                               cov_fcast = TRUE, cast_date = today(), 
                               yr = as.numeric(format(today(), "%Y")),
                               start = 217, end = NULL, hind_step = 1, 
                               lead_time = 12, min_lag = 6, fcast_nms = NULL,
                               nfcnm = 0, append_fcast_csv = TRUE, 
                               hist_fcast_file = "covariate_forecasts.csv",
                               source_name = "current_archive",
                               save = TRUE, filename = "covariates.csv", 
                               tree = dirtree(), quiet = FALSE){
  check_argsX()
  if (is.null(end) & cast_type == "hindcasts"){
    end <- 490:403
  }
  list(cast_type = cast_type, cov_hist = cov_hist, cov_fcast = cov_fcast, 
       cast_date = cast_date, yr = yr, start = start, end = end, 
       hind_step = hind_step, lead_time = lead_time, min_lag = min_lag, 
       fcast_nms = fcast_nms, nfcnm = nfcnm, 
       append_fcast_csv = append_fcast_csv, hist_fcast_file = hist_fcast_file,
       source_name = source_name, save = save, filename = filename, 
       tree = tree, quiet = quiet, class = "covariates") %>%
  classy(c("covariates_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{metadata_options} creates a \code{metadata_options} 
#'   \code{list}of options for the metadata creation.
#'
#' @return \code{metadata_options}: a \code{metadata_options} \code{list}
#'   of settings controlling the metadata creation.
#'
#' @export
#'
metadata_options <- function(cast_date = today(), cast_type = "forecasts",
                             confidence_level = 0.9, lead_time = 12,
                             save = TRUE, filename = "metadata.yaml", 
                             quiet = FALSE, tree = dirtree()){
  check_argsX()
  list(cast_date = cast_date, cast_type = cast_type, 
       confidence_level = confidence_level,lead_time = lead_time, save = save,
       filename = filename, quiet = quiet, tree = tree, 
       class = "metadata") %>%
  classy(c("metadata_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{predictions_options} creates a 
#'   \code{predictions_options} \code{list} of options for populating the 
#'   predictions subdirectory
#'
#' @return \code{predictions_options}: a \code{predictions_options} 
#'   \code{list} of settings controlling the population of the predictions 
#'   subdirectory.
#'
#' @export
#'
predictions_options <- function(base = ".", main = "", 
                                subs = subdirs(subs_type = "portalcasting"), 
                                download_existing_predictions = FALSE,
                                quiet = FALSE){

  check_argsX()
  tree <- dirtree(base, main, subs)
  list(tree = tree, 
       download_existing_predictions = download_existing_predictions, 
       quiet = quiet) %>%
  classy(c("predictions_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{models_options} creates a \code{models_options} 
#'   \code{list} of options for populating the models subdirectory.
#'
#' @return \code{models_options}: a \code{models_options} \code{list} of 
#'   settings controlling the population of the models subdirectory.
#'
#' @export
#'
models_options <- function(base = ".", main = "", 
                           subs = subdirs(subs_type = "portalcasting"),
                           quiet = FALSE, 
                           models = model_names(set = "prefab")){
  check_argsX()
  tree <- dirtree(base, main, subs)
  list(models = models, quiet = quiet, tree = tree) %>%
  classy(c("models_options", "list"))
}

#' @rdname all_options
#'
#' @description \code{cast_options} creates a \code{cast_options} \code{list} 
#'   of control options for running \code{\link{portalcast}}.
#'
#' @return \code{cast_options}: a \code{cast_options} \code{list} of settings 
#'   controlling the running of \code{\link{portalcast}}.
#'
#' @export
#'
cast_options <- function(base = ".", main = "", 
                         subs = subdirs(subs_type = "portalcasting"), 
                         quiet = FALSE, models = model_names(set = "prefab"), 
                         cast_type = "forecasts", cast_date = today(), 
                         ensemble = TRUE, start = 217, end = NULL, 
                         hind_step = 1, min_plots = 24, min_traps = 1){
  check_argsX()
  tree <- dirtree(base, main, subs)
  list(tree = tree, quiet = quiet, models = models, cast_type = cast_type,
       cast_date = cast_date, ensemble = ensemble, start = start, end = end, 
       hind_step = hind_step, min_plots = min_plots, 
       min_traps = min_traps) %>%
  classy(c("cast_options", "list"))
}

