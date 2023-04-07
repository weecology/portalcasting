context(desc = "webapp ui functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "ui functions can build a functioning ui off the global list", {

  skip_on_cran() 

  expect_silent(gl <- global_list(main = main3))
  expect_is(gl, "list")


  expect_silent(ptp <- page_title_panel( ))
  expect_is(ptp, "shiny.tag.list")

  expect_silent(psp <- page_subtitle_panel( ))
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
  expect_silent(tt <- write_rodent_profiles_tab_html(main = main2))
  expect_is(tt, "character")


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


  expect_message(gl <- global_list(main = main3))
  expect_is(gl, "list")

  expect_silent(ft <- forecast_tab(global = gl)) 
  expect_is(ft, "shiny.tag")
  expect_silent(et <- evaluation_tab(global = gl)) 
  expect_is(et, "shiny.tag")
  expect_silent(ct <- covariates_tab(global = gl)) 
  expect_is(ct, "shiny.tag")

})


