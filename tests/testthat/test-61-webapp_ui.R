context(desc = "webapp ui functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

casts_metadata    <- read_casts_metadata(main = main3)
casts_evaluations <- read_casts_evaluations(main = main3)


  forecast_tab_available_species                 <- unique(casts_metadata$species[casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])
  evaluation_tab_available_species               <- unique(casts_evaluations$species[casts_evaluations$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

  forecast_tab_available_models                  <- unique(casts_metadata$model[casts_metadata$model %in% prefab_models( ) & casts_metadata$species %in% forecast_tab_available_species])
  evaluation_tab_available_models                <- unique(casts_evaluations$model[casts_evaluations$model %in% prefab_models( ) & casts_evaluations$species %in% evaluation_tab_available_species])

  forecast_tab_available_datasets                <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( ) & casts_metadata$species %in% forecast_tab_available_species & casts_metadata$model %in% forecast_tab_available_models])
  evaluation_tab_available_datasets              <- unique(casts_evaluations$dataset[casts_evaluations$dataset %in% prefab_datasets( ) & casts_evaluations$species %in% evaluation_tab_available_species & casts_evaluations$model %in% evaluation_tab_available_models])

  forecast_tab_available_historic_end_newmoons   <- unique(casts_metadata$historic_end_newmoon[casts_metadata$species %in% forecast_tab_available_species & casts_metadata$model %in% forecast_tab_available_models & casts_metadata$dataset %in% forecast_tab_available_datasets])
  evaluation_tab_available_historic_end_newmoons <- unique(casts_evaluations$historic_end_newmoon[casts_evaluations$dataset %in% evaluation_tab_available_datasets & casts_evaluations$species %in% evaluation_tab_available_species & casts_evaluations$model %in% evaluation_tab_available_models])

  evaluation_tab_available_newmoons              <- unique(casts_evaluations$newmoon[casts_evaluations$dataset %in% evaluation_tab_available_datasets & casts_evaluations$species %in% evaluation_tab_available_species & casts_evaluations$model %in% evaluation_tab_available_models & casts_evaluations$historic_end_newmoon %in% evaluation_tab_available_historic_end_newmoons & !is.na(casts_evaluations$obs)])


test_that(desc = "portal_forecast_ui builds a ui", {

  expect_message(ui <- portal_forecast_ui(main = main3))
  expect_is(ui, "list")

})

test_that(desc = "panels build tags", {

  expect_silent(mp <- page_main_panel(main = main3))
  expect_is(mp, "shiny.tag")

  expect_silent(pt <- page_title_panel())
  expect_is(pt, "shiny.tag.list")

  expect_silent(st <- page_subtitle_panel())
  expect_is(st, "shiny.tag")

})


test_that(desc = "static tabs build tags", {

  expect_silent(at <- about_tab())
  expect_is(at, "shiny.tag")

  expect_silent(mt <- models_tab())
  expect_is(mt, "shiny.tag")

  expect_silent(pt <- profiles_tab())
  expect_is(pt, "shiny.tag")

})


test_that(desc = "forecast tabs build tags", {

  expect_silent(ft <- forecast_tab(main = main3))
  expect_is(ft, "shiny.tag")

  expect_silent(ftcr <- forecast_tab_input_selection_checks_row())
  expect_is(ftcr, "shiny.tag")

  expect_silent(ftr <- forecast_tab_input_selection_row(main = main3))
  expect_is(ftr, "shiny.tag")

  expect_silent(ftrs <- forecast_tab_input_selection_row_species(main = main3))
  expect_is(ftrs, "shiny.tag")

  expect_silent(ftrm <- forecast_tab_input_selection_row_model(main = main3))
  expect_is(ftrm, "shiny.tag")

  expect_silent(ftrd <- forecast_tab_input_selection_row_dataset(main = main3))
  expect_is(ftrd, "shiny.tag")

  expect_silent(ftrh <- forecast_tab_input_selection_row_historic_end_newmoonnumber(main = main3))
  expect_is(ftrh, "shiny.tag")

})


test_that(desc = "evaluation tabs build tags", {

  expect_silent(ft <- evaluation_tab(main = main3))
  expect_is(ft, "shiny.tag")

  expect_silent(ftcr <- evaluation_tab_input_selection_checks_row())
  expect_is(ftcr, "shiny.tag")

  expect_silent(ftr <- evaluation_tab_input_selection_row(main = main3))
  expect_is(ftr, "shiny.tag")

  expect_silent(ftrs <- evaluation_tab_input_selection_row_species())
  expect_is(ftrs, "shiny.tag")

  expect_silent(ftrm <- evaluation_tab_input_selection_row_model())
  expect_is(ftrm, "shiny.tag")

  expect_silent(ftrd <- evaluation_tab_input_selection_row_dataset())
  expect_is(ftrd, "shiny.tag")

  expect_silent(ftrh <- evaluation_tab_input_selection_row_historic_end_newmoonnumber(main = main3))
  expect_is(ftrh, "shiny.tag")

})





