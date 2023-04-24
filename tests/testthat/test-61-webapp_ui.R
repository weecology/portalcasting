context(desc = "webapp ui functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "ui functions can build a functioning ui off the global list", {

  skip_on_cran() 

  expect_silent(gl <- global_list(main = main3))
  expect_is(gl, "list")


  expect_silent(ptp <- title_panel( ))
  expect_is(ptp, "shiny.tag.list")


  expect_silent(ptp <- main_panel(global = gl))
  expect_is(ptp, "shiny.tag")

  expect_silent(psp <- subtitle_panel( ))
  expect_is(psp, "shiny.tag")

  expect_silent(ft <- forecast_tab(global = gl)) 
  expect_is(ft, "shiny.tag")
  expect_silent(et <- evaluation_tab(global = gl)) 
  expect_is(et, "shiny.tag")

  expect_silent(cr <- forecast_tab_input_selection_checks_row( )) 
  expect_is(cr, "shiny.tag")
  expect_silent(cr <- evaluation_tab_input_selection_checks_row( )) 
  expect_is(cr, "shiny.tag")

  expect_silent(tt <- forecast_tab_input_selection_row_species(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- forecast_tab_input_selection_row_dataset(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- forecast_tab_input_selection_row_model(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- forecast_tab_input_selection_row_historic_end_newmoonnumber(global = gl))
  expect_is(tt, "shiny.tag")

  expect_silent(tt <- evaluation_tab_input_selection_row_species(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- evaluation_tab_input_selection_row_dataset(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- evaluation_tab_input_selection_row_model(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- evaluation_tab_input_selection_row_historic_end_newmoonnumber(global = gl))
  expect_is(tt, "shiny.tag")
  expect_silent(tt <- evaluation_tab_input_selection_row_newmoonnumber(global = gl))
  expect_is(tt, "shiny.tag")

  expect_silent(tt <- write_models_tab_html(main = main2))
  expect_is(tt, "character")
  expect_silent(tt <- write_rodents_profiles_tab_html(main = main2))
  expect_is(tt, "character")


  expect_silent(bs <- app_theme())
  expect_is(bs, "bs_theme")

  expect_silent(at <- about_tab(global = gl))
  expect_is(at, "shiny.tag")

  expect_silent(mt <- models_tab(global = gl))
  expect_is(mt, "shiny.tag")

  expect_silent(pt <- rodents_profiles_tab(global = gl))
  expect_is(pt, "shiny.tag")

  expect_silent(pf <- app_ui(global = gl))
  expect_is(pt, "shiny.tag")


  gll <- global_list(main = main3)
  gll$forecasts_metadata    <- NULL
  gll$forecasts_evaluations <- NULL
  gll$covariates            <- NULL

  expect_silent(ft <- forecast_tab(global = gll)) 
  expect_is(ft, "shiny.tag")
  expect_silent(et <- evaluation_tab(global = gll)) 
  expect_is(et, "shiny.tag")
  expect_silent(et <- covariates_tab(global = gll)) 
  expect_is(et, "shiny.tag")
  


})



test_that(desc = "ui functions can build a functioning ui off the global list of a sandbox", {

  skip_on_cran() 

  expect_message(gl <- global_list(main = main2))
  expect_is(gl, "list")

  expect_silent(ft <- forecast_tab(global = gl)) 
  expect_is(ft, "shiny.tag")
  expect_silent(et <- evaluation_tab(global = gl)) 
  expect_is(et, "shiny.tag")
  expect_silent(ct <- covariates_tab(global = gl)) 
  expect_is(ct, "shiny.tag")


  expect_silent(gl <- global_list(main = main3))
  expect_is(gl, "list")

  expect_silent(ft <- forecast_tab(global = gl)) 
  expect_is(ft, "shiny.tag")
  expect_silent(et <- evaluation_tab(global = gl)) 
  expect_is(et, "shiny.tag")
  expect_silent(ct <- covariates_tab(global = gl)) 
  expect_is(ct, "shiny.tag")

})



