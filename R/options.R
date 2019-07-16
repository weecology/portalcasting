all_options <- function(base = ".", main = "", subs = subdirs(), 
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
                        models = model_names(), 
                        ensemble = TRUE,
                        version = "latest", from_zenodo = TRUE,
                        to_cleanup = c("tmp", "PortalData"),
                        covariate_source = "retroactive",
                        covariate_date_made = "2018-06-05 12:00:00 PST"){

  check_args()
  if (is.null(end) & cast_type == "hindcasts"){
    end <- 490:403
  }
  list(
   
    options_cast = 
      cast_options(base = base, main = main, subs = subs, quiet = quiet, 
                   models = models, cast_type = cast_type, 
                   cast_date = cast_date, ensemble = ensemble, start = start, 
                   end = end, hind_step = hind_step, min_plots = min_plots, 
                   min_traps = min_traps)
  ) %>%
  classy(c("all_options", "list"))

}

cast_options <- function(base = ".", main = "", subs = subdirs(), 
                         quiet = FALSE, models = model_names(), 
                         cast_type = "forecasts", cast_date = today(), 
                         ensemble = TRUE, start = 217, end = NULL, 
                         hind_step = 1, min_plots = 24, min_traps = 1){
  check_args()
  tree <- dirtree(base, main, subs)
  list(tree = tree, quiet = quiet, models = models, cast_type = cast_type,
       cast_date = cast_date, ensemble = ensemble, start = start, end = end, 
       hind_step = hind_step, min_plots = min_plots, 
       min_traps = min_traps) %>%
  classy(c("cast_options", "list"))
}

