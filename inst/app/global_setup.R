settings <- read_directory_settings(main = main)

messageq("Reading in casts metadata and evaluation files ...", quiet = settings$quiet)

  casts_metadata    <- read_casts_metadata(main = main)
  casts_evaluations <- read_casts_evaluations(main = main)

messageq(" ... done.", quiet = settings$quiet)

messageq("Determining available values ...", quiet = settings$quiet)


  forecast_tab_available_species                 <- unique(casts_metadata$species[casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])
  evaluation_tab_available_species               <- unique(casts_evaluations$species[casts_evaluations$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

  forecast_tab_available_models                  <- unique(casts_metadata$model[casts_metadata$model %in% prefab_models( ) & casts_metadata$species %in% forecast_tab_available_species])
  evaluation_tab_available_models                <- unique(casts_evaluations$model[casts_evaluations$model %in% prefab_models( ) & casts_evaluations$species %in% evaluation_tab_available_species])

  forecast_tab_available_datasets                <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( ) & casts_metadata$species %in% forecast_tab_available_species & casts_metadata$model %in% forecast_tab_available_models])
  evaluation_tab_available_datasets              <- unique(casts_evaluations$dataset[casts_evaluations$dataset %in% prefab_datasets( ) & casts_evaluations$species %in% evaluation_tab_available_species & casts_evaluations$model %in% evaluation_tab_available_models])

  forecast_tab_available_historic_end_newmoons   <- unique(casts_metadata$historic_end_newmoon[casts_metadata$species %in% forecast_tab_available_species & casts_metadata$model %in% forecast_tab_available_models & casts_metadata$dataset %in% forecast_tab_available_datasets])
  evaluation_tab_available_historic_end_newmoons <- unique(casts_evaluations$historic_end_newmoon[casts_evaluations$dataset %in% evaluation_tab_available_datasets & casts_evaluations$species %in% evaluation_tab_available_species & casts_evaluations$model %in% evaluation_tab_available_models])

  evaluation_tab_available_newmoons              <- unique(casts_evaluations$newmoon[casts_evaluations$dataset %in% evaluation_tab_available_datasets & casts_evaluations$species %in% evaluation_tab_available_species & casts_evaluations$model %in% evaluation_tab_available_models & casts_evaluations$historic_end_newmoon %in% evaluation_tab_available_historic_end_newmoons & !is.na(casts_evaluations$obs)])



messageq(" ... done.", quiet = settings$quiet)

