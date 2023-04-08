context(desc = "webapp server functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "server functions work off of global list", {

  skip_on_cran() 

  expect_silent(gl <- global_list(main = main3))
  expect_is(gl, "list")

  expect_silent(irv <- initial_reactive_values(global = gl))
  expect_is(irv, "reactivevalues")

  expect_silent(io <- initial_output(main = main3, rv = irv, output = list()))
  expect_is(io, "list")

})

#testServer(portal_forecast_server, {

#  expect_silent(global <- global_list(main = main3))

#  session$setInputs(forecast_tab_species                      = global$initial_forecast_tab_selected_species, 
#                    forecast_tab_dataset                      = global$initial_forecast_tab_selected_dataset,
#                    forecast_tab_model                        = global$initial_forecast_tab_selected_model,
#                    forecast_tab_historic_end_newmoonnumber   = global$initial_forecast_tab_selected_historic_end_newmoonnumber,
#                    evaluation_tab_species                    = global$initial_evaluation_tab_selected_species,
#                    evaluation_tab_dataset                    = global$initial_evaluation_tab_selected_dataset,
#                    evaluation_tab_model                      = global$initial_evaluation_tab_selected_model,
#                    evaluation_tab_historic_end_newmoonnumber = global$initial_evaluation_tab_selected_historic_end_newmoonnumber,
#                    evaluation_tab_newmoonnumber              = global$initial_evaluation_tab_selected_newmoonnumber)

#  expect_equal(output$forecast_tab_species, as.vector(global$initial_forecast_tab_selected_species))
#  expect_equal(output$forecast_tab_dataset, as.vector(global$initial_forecast_tab_selected_dataset))
#  expect_equal(output$forecast_tab_model, as.vector(global$initial_forecast_tab_selected_model))
#  expect_equal(output$forecast_tab_historic_end_newmoonnumber, as.character(global$initial_forecast_tab_selected_historic_end_newmoonnumber))

#  expect_equal(output$evaluation_tab_species, as.vector(global$initial_evaluation_tab_selected_species))
#  expect_equal(output$evaluation_tab_dataset, as.vector(global$initial_evaluation_tab_selected_dataset))
#  expect_equal(output$evaluation_tab_model, as.vector(global$initial_evaluation_tab_selected_model))
#  expect_equal(output$evaluation_tab_historic_end_newmoonnumber, as.character(global$initial_evaluation_tab_selected_historic_end_newmoonnumber))
#  expect_equal(output$evaluation_tab_newmoonnumber, as.character(global$initial_evaluation_tab_selected_newmoonnumber))


#})
