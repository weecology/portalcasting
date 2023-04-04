context(desc = "webapp server functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


  casts_metadata    <- read_casts_metadata(main = main3)
  casts_evaluations <- read_casts_evaluations(main = main3)
  casts_evaluations <- casts_evaluations[!is.na(casts_evaluations$obs), ]

  initial_forecast_tab_available_species                       <- unique(casts_metadata$species[casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])
  initial_evaluation_tab_available_species                     <- unique(casts_evaluations$species[casts_evaluations$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

  initial_forecast_tab_available_models                        <- unique(casts_metadata$model[casts_metadata$model %in% prefab_models( ) & casts_metadata$species %in% initial_forecast_tab_available_species])
  initial_evaluation_tab_available_models                      <- unique(casts_evaluations$model[casts_evaluations$model %in% prefab_models( ) & casts_evaluations$species %in% initial_evaluation_tab_available_species])

  initial_forecast_tab_available_datasets                      <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( ) & casts_metadata$species %in% initial_forecast_tab_available_species & casts_metadata$model %in% initial_forecast_tab_available_models])
  initial_evaluation_tab_available_datasets                    <- unique(casts_evaluations$dataset[casts_evaluations$dataset %in% prefab_datasets( ) & casts_evaluations$species %in% initial_evaluation_tab_available_species & casts_evaluations$model %in% initial_evaluation_tab_available_models])

  initial_forecast_tab_available_historic_end_newmoonnumbers   <- unique(casts_metadata$historic_end_newmoonnumber[casts_metadata$species %in% initial_forecast_tab_available_species & casts_metadata$model %in% initial_forecast_tab_available_models & casts_metadata$dataset %in% initial_forecast_tab_available_datasets])
  initial_evaluation_tab_available_historic_end_newmoonnumbers <- unique(casts_evaluations$historic_end_newmoonnumber[casts_evaluations$dataset %in% initial_evaluation_tab_available_datasets & casts_evaluations$species %in% initial_evaluation_tab_available_species & casts_evaluations$model %in% initial_evaluation_tab_available_models])

  initial_evaluation_tab_available_newmoonnumbers              <- unique(casts_evaluations$newmoon[casts_evaluations$dataset %in% initial_evaluation_tab_available_datasets & casts_evaluations$species %in% initial_evaluation_tab_available_species & casts_evaluations$model %in% initial_evaluation_tab_available_models & casts_evaluations$historic_end_newmoonnumber %in% initial_evaluation_tab_available_historic_end_newmoonnumbers])

  initial_forecast_tab_selected_model                         <- "AutoArima"
  initial_evaluation_tab_selected_model                       <- "AutoArima"

  initial_forecast_tab_selected_species                       <- "DM"
  initial_evaluation_tab_selected_species                     <- "DM"

  initial_forecast_tab_selected_dataset                       <- "controls"
  initial_evaluation_tab_selected_dataset                     <- "controls"

  initial_forecast_tab_selected_historic_end_newmoonnumber    <- max(initial_forecast_tab_available_historic_end_newmoonnumbers)
  initial_evaluation_tab_selected_historic_end_newmoonnumber  <- max(initial_evaluation_tab_available_historic_end_newmoonnumbers)

  initial_evaluation_tab_selected_newmoonnumber               <- max(initial_evaluation_tab_available_newmoonnumbers)




test_that(desc = "initial reactive values are setup properly", {


  expect_is(initial_reactive_values(main = main3), "reactivevalues")


})