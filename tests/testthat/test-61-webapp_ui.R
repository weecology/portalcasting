context(desc = "webapp ui functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))


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

  expect_silent(ftrs <- forecast_tab_input_selection_row_species())
  expect_is(ftrs, "shiny.tag")

  expect_silent(ftrm <- forecast_tab_input_selection_row_model())
  expect_is(ftrm, "shiny.tag")

  expect_silent(ftrd <- forecast_tab_input_selection_row_dataset())
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


test_that(desc = "lists functions build lists", {

  expect_silent(ft <- historic_end_newmoonnumber_list(main = main3))
  expect_is(ft, "integer")

  expect_silent(ft <- model_list())
  expect_is(ft, "character")

  expect_silent(ft <- species_list())
  expect_is(ft, "character")

})



