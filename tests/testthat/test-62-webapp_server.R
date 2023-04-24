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

  expect_silent(io <- initial_output(main = main3, global = gl, rv = irv, output = list()))
  expect_is(io, "list")

  rv <- list(forecast_tab_species                       = gl$initial_forecast_tab_selected_species,
             forecast_tab_dataset                       = gl$initial_forecast_tab_selected_dataset,
             forecast_tab_model                         = gl$initial_forecast_tab_selected_model,
             forecast_tab_historic_end_newmoonnumber    = gl$initial_forecast_tab_selected_historic_end_newmoonnumber,
             evaluation_tab_species                     = gl$initial_evaluation_tab_selected_species,
             evaluation_tab_dataset                     = gl$initial_evaluation_tab_selected_dataset,
             evaluation_tab_model                       = gl$initial_evaluation_tab_selected_model,
             evaluation_tab_historic_end_newmoonnumber  = gl$initial_evaluation_tab_selected_historic_end_newmoonnumber,
             evaluation_tab_newmoonnumber               = gl$initial_evaluation_tab_selected_newmoonnumber)


  expect_silent(urv <- update_reactive_values(event = "forecast_tab", rv = rv, input = rv))
  expect_is(urv, "list")
  expect_silent(urv2 <- update_reactive_values(event = "evaluation_tab", rv = rv, input = rv))
  expect_is(urv2, "list")


  expect_silent(uo <- update_output(main = main3, global = gl, event = "forecast_tab", rv = rv, input = rv, output = io))
  expect_is(uo, "list")
  expect_silent(uo2 <- update_output(main = main3, global = gl, event = "evaluation_tab", rv = rv, input = rv, output = io))
  expect_is(uo2, "list")


  global <<- global_list(main = main3)

  rows_in <- global$forecasts_evaluations$newmoonnumber == global$initial_evaluation_tab_selected_newmoonnumber &
             global$forecasts_evaluations$species == global$initial_evaluation_tab_selected_species &
             global$forecasts_evaluations$model == global$initial_evaluation_tab_selected_model &
             global$forecasts_evaluations$dataset == global$initial_evaluation_tab_selected_dataset 


  global$initial_evaluation_tab_selected_historic_end_newmoonnumber <<- max(global$forecasts_evaluations$historic_end_newmoonnumber[rows_in])

  testServer(app_server, {

    session$setInputs(forecast_tab_species                       = global$initial_forecast_tab_selected_species,
                      forecast_tab_dataset                       = global$initial_forecast_tab_selected_dataset,
                      forecast_tab_model                         = global$initial_forecast_tab_selected_model,
                      forecast_tab_historic_end_newmoonnumber    = global$initial_forecast_tab_selected_historic_end_newmoonnumber,
                      evaluation_tab_species                     = global$initial_evaluation_tab_selected_species,
                      evaluation_tab_dataset                     = global$initial_evaluation_tab_selected_dataset,
                      evaluation_tab_model                       = global$initial_evaluation_tab_selected_model,
                      evaluation_tab_historic_end_newmoonnumber  = global$initial_evaluation_tab_selected_historic_end_newmoonnumber,
                      evaluation_tab_newmoonnumber               = global$initial_evaluation_tab_selected_newmoonnumber)

    output <- initial_output(main = main3, rv = rv, output = output)

    expect_equal(output$forecast_tab_species, as.character(global$initial_forecast_tab_selected_species))
    expect_equal(output$forecast_tab_dataset, global$initial_forecast_tab_selected_dataset)
    expect_equal(output$forecast_tab_model, as.character(global$initial_forecast_tab_selected_model))
    expect_equal(output$forecast_tab_historic_end_newmoonnumber, as.character(global$initial_forecast_tab_selected_historic_end_newmoonnumber))
    expect_equal(output$evaluation_tab_species, as.character(global$initial_evaluation_tab_selected_species))
    expect_equal(output$evaluation_tab_dataset, global$initial_evaluation_tab_selected_dataset)
    expect_equal(output$evaluation_tab_model, as.character(global$initial_evaluation_tab_selected_model))
    expect_equal(output$evaluation_tab_historic_end_newmoonnumber, as.character(global$initial_evaluation_tab_selected_historic_end_newmoonnumber))
    expect_equal(output$evaluation_tab_newmoonnumber, as.character(global$initial_evaluation_tab_selected_newmoonnumber))


    output <- update_output(main   = main3, 
                            event  = "forecast_tab", 
                            rv     = rv, 
                            input  = input, 
                            output = output)

    expect_is(output, "shinyoutput")
    output <- update_output(main   = main3, 
                            event  = "evaluation_tab", 
                            rv     = rv, 
                            input  = input, 
                            output = output)
    expect_is(output, "shinyoutput")

    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "forecast_tab_species", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "forecast_tab_dataset", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "forecast_tab_model", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "forecast_tab_historic_end_newmoonnumber", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)

    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "evaluation_tab_species", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "evaluation_tab_dataset", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "evaluation_tab_model", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "evaluation_tab_historic_end_newmoonnumber", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)
    expect_equal(event_reaction(main = main3,
                                global = global,
                                event = "evaluation_tab_newmoonnumber", 
                                rv = rv, 
                                input = input, 
                                output = output, 
                                session = session), NULL)

    expect_equal(names(output$forecast_tab_ts_plot), c("src", "width", "height", "alt", "coordmap"))
    expect_equal(names(output$forecast_tab_ss_plot), c("src", "width", "height", "alt", "coordmap"))
    expect_equal(names(output$evaluation_tab_sp_plot), c("src", "width", "height", "alt", "coordmap"))
    expect_equal(names(output$evaluation_tab_RMSE_plot), c("src", "width", "height", "alt", "coordmap"))
    expect_equal(names(output$covariates_tab_ndvi_plot), c("src", "width", "height", "alt", "coordmap"))
    expect_equal(names(output$covariates_tab_precip_plot), c("src", "width", "height", "alt", "coordmap"))
    expect_equal(names(output$covariates_tab_temp_plot), c("src", "width", "height", "alt", "coordmap"))

  })

  rm(global, envir = .GlobalEnv)

})



