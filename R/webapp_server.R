
#' @title Generate the Server Code for the Web App
#' 
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param rv [`reactiveValues`][shiny::reactiveValues] `list` for the UI.
#'
#' @param input `input` `list` for the UI.
#'
#' @param output `output` `list` for the UI.
#'
#' @param event `character` value of the server event. \cr
#'        Options include `"forecast_tab_species"`, `"forecast_tab_dataset"`, `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`, `"evaluation_tab_species"`, `"evaluation_tab_dataset"`, `"evaluation_tab_model"`, and `"evaluation_tab_historic_end_newmoonnumber"`)
#'
#' @param session Environment for the UI.
#'
#' @param global A `list` of global values for the app.
#'
#' @return 
#'   `portal_forecast_server`: an observer reference class object (see [`observeEvent`][shiny::observeEvent] and [`observe`][shiny::observe]). \cr
#'   `initial_reactive_values`: a [`reactiveValues`][shiny::reactiveValues] `list`. \cr
#'   `initial_output`: an `output` `list`. \cr
#'   `event_reaction`: updates the `rv`, `output`, and `input` `list`s, but does not return them, per se. \cr
#'   `update_reactive_values`: a [`reactiveValues`][shiny::reactiveValues] `list`. \cr
#'   `update_output`: an `output` `list`. \cr
#'   `update_input`: updates the `input` `list`, but does not return it. 
#'
#' @name portalcasting app server
#'
NULL



#' @rdname portalcasting-app-server
#'
#' @export
#'
portal_forecast_server <- function (input, 
                                    output, 
                                    session) {

  rv     <- initial_reactive_values(global = global)

  output <- initial_output(main   = main,
                           rv     = rv, 
                           output = output)

  observeEvent(eventExpr   = input$forecast_tab_species,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "forecast_tab_species", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$forecast_tab_dataset,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "forecast_tab_dataset", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$forecast_tab_model,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "forecast_tab_model", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$forecast_tab_historic_end_newmoonnumber,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "forecast_tab_historic_end_newmoonnumber", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))

  observeEvent(eventExpr   = input$evaluation_tab_species,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "evaluation_tab_species", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$evaluation_tab_dataset,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "evaluation_tab_dataset", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$evaluation_tab_model,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "evaluation_tab_model", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$evaluation_tab_historic_end_newmoonnumber,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "evaluation_tab_historic_end_newmoonnumber", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))
  observeEvent(eventExpr   = input$evaluation_tab_newmoonnumber,
               handlerExpr = event_reaction(main    = main, 
                                            global  = global,
                                            event   = "evaluation_tab_newmoonnumber", 
                                            rv      = rv, 
                                            input   = input, 
                                            output  = output, 
                                            session = session))

}


#' @rdname portalcasting-app-server
#'
#' @export
#'
initial_reactive_values <- function (global = global_list( )) {

  reactiveValues(forecast_tab_species                      = global$initial_forecast_tab_selected_species, 
                 forecast_tab_dataset                      = global$initial_forecast_tab_selected_dataset,
                 forecast_tab_model                        = global$initial_forecast_tab_selected_model,
                 forecast_tab_historic_end_newmoonnumber   = global$initial_forecast_tab_selected_historic_end_newmoonnumber,
                 evaluation_tab_species                    = global$initial_evaluation_tab_selected_species,
                 evaluation_tab_dataset                    = global$initial_evaluation_tab_selected_dataset,
                 evaluation_tab_model                      = global$initial_evaluation_tab_selected_model,
                 evaluation_tab_historic_end_newmoonnumber = global$initial_evaluation_tab_selected_historic_end_newmoonnumber,
                 evaluation_tab_newmoonnumber              = global$initial_evaluation_tab_selected_newmoonnumber)

}


#' @rdname portalcasting-app-server
#'
#' @export
#'
initial_output <- function (main = ".",
                            rv, 
                            output) {

  output$forecast_tab_species                      <- renderText(rv$forecast_tab_species)
  output$forecast_tab_dataset                      <- renderText(rv$forecast_tab_dataset)
  output$forecast_tab_model                        <- renderText(rv$forecast_tab_model)
  output$forecast_tab_historic_end_newmoonnumber   <- renderText(rv$forecast_tab_historic_end_newmoonnumber)
  output$forecast_tab_ts_plot                      <- renderPlot(plot_forecast_ts(main                          = main,
                                                                                  dataset                       = rv$forecast_tab_dataset,
                                                                                  species                       = rv$forecast_tab_species,
                                                                                  historic_end_newmoonnumber    = rv$forecast_tab_historic_end_newmoonnumber,
                                                                                  model                         = rv$forecast_tab_model))
  output$forecast_tab_ss_plot                      <- renderPlot(plot_forecast_point(main                       = main,
                                                                                     dataset                    = rv$forecast_tab_dataset,
                                                                                     highlight_sp               = rv$forecast_tab_species,
                                                                                     historic_end_newmoonnumber = rv$forecast_tab_historic_end_newmoonnumber,
                                                                                     model                      = rv$forecast_tab_model))

  output$evaluation_tab_species                    <- renderText(rv$evaluation_tab_species)
  output$evaluation_tab_dataset                    <- renderText(rv$evaluation_tab_dataset)
  output$evaluation_tab_model                      <- renderText(rv$evaluation_tab_model)
  output$evaluation_tab_historic_end_newmoonnumber <- renderText(rv$evaluation_tab_historic_end_newmoonnumber)
  output$evaluation_tab_newmoonnumber              <- renderText(rv$evaluation_tab_newmoonnumber)

  output$evaluation_tab_sp_plot                    <- renderPlot(plot_forecast_point(main                            = main,
                                                                                     dataset                         = rv$evaluation_tab_dataset,
                                                                                     highlight_sp                    = rv$evaluation_tab_species,
                                                                                     model                           = rv$evaluation_tab_model,
                                                                                     historic_end_newmoonnumber      = rv$evaluation_tab_historic_end_newmoonnumber,
                                                                                     newmoonnumber                   = rv$evaluation_tab_newmoonnumber,
                                                                                     with_census                     = TRUE))
  output$evaluation_tab_RMSE_plot                  <- renderPlot(plot_forecasts_cov_RMSE(main                        = main,
                                                                                         datasets                    = rv$evaluation_tab_dataset,
                                                                                         species                     = rv$evaluation_tab_species,
                                                                                         models                      = rv$evaluation_tab_model,
                                                                                         historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))

  output$covariates_tab_ndvi_plot                  <- renderPlot(plot_covariates(main    = main,
                                                                                 to_plot = "ndvi"))
  output$covariates_tab_precip_plot                <- renderPlot(plot_covariates(main    = main,
                                                                                 to_plot = c("precipitation", "warm_precip")))
  output$covariates_tab_temp_plot                  <- renderPlot(plot_covariates(main    = main,
                                                                                 to_plot = c("mintemp", "meantemp", "maxtemp")))

  output 

}

#' @rdname portalcasting-app-server
#'
#' @export
#'
event_reaction <- function (main,
                            global,
                            event, 
                            rv, 
                            input, 
                            output, 
                            session) {

  rv     <- update_reactive_values(event = event, 
                                   rv         = rv, 
                                   input      = input)

  output <- update_output(main       = main, 
                          event = event, 
                          rv         = rv, 
                          input      = input, 
                          output     = output)

  update_input(global     = global,
               event = event, 
               rv         = rv, 
               input      = input, 
               session    = session)

}

#' @rdname portalcasting-app-server
#'
#' @export
#'
update_reactive_values <- function (event, 
                                    rv, 
                                    input) {

  if (grepl("forecast_tab", event)) {

    rv$forecast_tab_species                    <- input$forecast_tab_species
    rv$forecast_tab_dataset                    <- input$forecast_tab_dataset
    rv$forecast_tab_model                      <- input$forecast_tab_model
    rv$forecast_tab_historic_end_newmoonnumber <- input$forecast_tab_historic_end_newmoonnumber

  }
  if (grepl("evaluation_tab", event)) {

    rv$evaluation_tab_species                    <- input$evaluation_tab_species
    rv$evaluation_tab_dataset                    <- input$evaluation_tab_dataset
    rv$evaluation_tab_model                      <- input$evaluation_tab_model
    rv$evaluation_tab_historic_end_newmoonnumber <- input$evaluation_tab_historic_end_newmoonnumber
    rv$evaluation_tab_newmoonnumber              <- input$evaluation_tab_newmoonnumber

  }


  rv

}


#' @rdname portalcasting-app-server
#'
#' @export
#'
update_output <- function (main, 
                           event,
                           rv, 
                           input, 
                           output) {


  if (grepl("forecast_tab", event)) {

    output$forecast_tab_species                    <- renderText(rv$forecast_tab_species)
    output$forecast_tab_dataset                    <- renderText(rv$forecast_tab_dataset)
    output$forecast_tab_model                      <- renderText(rv$forecast_tab_model)
    output$forecast_tab_historic_end_newmoonnumber <- renderText(rv$forecast_tab_historic_end_newmoonnumber)
    output$forecast_tab_ts_plot                    <- renderPlot(plot_forecast_ts(main                          = main,
                                                                              dataset                       = rv$forecast_tab_dataset,
                                                                              species                       = rv$forecast_tab_species,
                                                                              historic_end_newmoonnumber    = rv$forecast_tab_historic_end_newmoonnumber,
                                                                              model                         = rv$forecast_tab_model))
    output$forecast_tab_ss_plot                    <- renderPlot(plot_forecast_point(main                       = main,
                                                                                 dataset                    = rv$forecast_tab_dataset,
                                                                                 highlight_sp               = rv$forecast_tab_species,
                                                                                 historic_end_newmoonnumber = rv$forecast_tab_historic_end_newmoonnumber,
                                                                                 model                      = rv$forecast_tab_model))

  }
  if (grepl("evaluation_tab", event)) {

    output$evaluation_tab_species                    <- renderText(rv$evaluation_tab_species)
    output$evaluation_tab_dataset                    <- renderText(rv$evaluation_tab_dataset)
    output$evaluation_tab_model                      <- renderText(rv$evaluation_tab_model)
    output$evaluation_tab_historic_end_newmoonnumber <- renderText(rv$evaluation_tab_historic_end_newmoonnumber)
    output$evaluation_tab_newmoonnumber              <- renderText(rv$evaluation_tab_newmoonnumber)

    output$evaluation_tab_sp_plot                    <- renderPlot(plot_forecast_point(main                            = main,
                                                                                   dataset                         = rv$evaluation_tab_dataset,
                                                                                   highlight_sp                    = rv$evaluation_tab_species,
                                                                                   model                           = rv$evaluation_tab_model,
                                                                                   historic_end_newmoonnumber      = rv$evaluation_tab_historic_end_newmoonnumber,
                                                                                   newmoonnumber                   = rv$evaluation_tab_newmoonnumber,
                                                                                   with_census                     = TRUE))
    output$evaluation_tab_RMSE_plot                  <- renderPlot(plot_forecasts_cov_RMSE(main                        = main,
                                                                                       datasets                    = rv$evaluation_tab_dataset,
                                                                                       species                     = rv$evaluation_tab_species,
                                                                                       models                      = rv$evaluation_tab_model,
                                                                                       historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))

  }


  output

}


#' @rdname portalcasting-app-server
#'
#' @export
#'
update_input <- function (global,
                          event, 
                          rv, 
                          input, 
                          session) {

  if (event == "forecast_tab_species") {

    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))

  }
  if (event == "forecast_tab_dataset") {

    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))

  }
  if (event == "forecast_tab_model") {

    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))

  }
  if (event == "forecast_tab_historic_end_newmoonnumber") {

    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))

  }
  if (event == "evaluation_tab_species") {

    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(global     = global,
                                                          event = event,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(global       = global,
                                                        event   = event,
                                                        rv           = rv))

  }
  if (event == "evaluation_tab_dataset") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(global     = global,
                                                          event = event,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(global       = global,
                                                        event   = event,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))

  }
  if (event == "evaluation_tab_model") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(global     = global,
                                                          event = event,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(global       = global,
                                                        event   = event,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))

  }
  if (event == "evaluation_tab_historic_end_newmoonnumber") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(global     = global,
                                                          event = event,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(global       = global,
                                                        event   = event,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))

  }

  if (event == "evaluation_tab_newmoonnumber") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(global     = global,
                                                          event = event,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(global       = global,
                                                        event   = event,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(global     = global,
                                                   event = event,
                                                   rv         = rv),
                      selected = selected_species(global      = global,
                                                  event  = event,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(global     = global,
                                                    event = event,
                                                    rv         = rv),
                      selected = selected_dataset(global       = global,
                                                  event   = event,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(global     = global,
                                                  event = event,
                                                  rv         = rv),
                      selected = selected_model(global       = global,
                                                event   = event,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(global     = global,
                                                                       event = event,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(global       = global,
                                                                     event   = event,
                                                                     rv           = rv))

  }

}


