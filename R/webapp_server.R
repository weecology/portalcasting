
#' @title Generate the Server Code for the Web App
#' 
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param rv [`reactiveValues`][shiny::reactiveValues] `list` for the UI.
#'
#' @param input `input` `list` for the UI.
#'
#' @param output `output` `list` for the GenEst GUI.
#'
#' @param event_name `character` value of the server event. \cr
#'   Options include `"forecast_tab_species"`, `"forecast_tab_dataset"`, `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`, `"evaluation_tab_species"`, `"evaluation_tab_dataset"`, `"evaluation_tab_model"`, and `"evaluation_tab_historic_end_newmoonnumber"`)
#'
#' @param session Environment for the GenEst GUI.
#'
#' @return 
#'   `portal_forecast_server`: an observer reference class object (see [`observeEvent`][shiny::observeEvent] and [`observe`][shiny::observe]). \cr \cr
#'   `initial_reactive_values`: a [`reactiveValues`][shiny::reactiveValues] `list`. \cr \cr
#'   `initial_output`: an `output` `list`. \cr \cr
#'   `event_reaction`: updates the `rv`, `output`, and `input` `list`s, but does not return them, per se. \cr \cr
#'   `update_reactive_values`: a [`reactiveValues`][shiny::reactiveValues] `list`. \cr \cr
#'   `update_output`: an `output` `list`. \cr \cr
#'   `update_input`: updates the `input` `list`, but does not return it. \cr \cr
#'
#' @name web app server
#'
NULL



#' @rdname web-app-server
#'
#' @export
#'
portal_forecast_server <- function (main = ".",
                                    input, 
                                    output, 
                                    session) {

  rv     <- initial_reactive_values(main = main)

  output <- initial_output(main   = main,
                           rv     = rv, 
                           output = output)

  observeEvent(eventExpr   = input$forecast_tab_species,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "forecast_tab_species", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$forecast_tab_dataset,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "forecast_tab_dataset", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$forecast_tab_model,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "forecast_tab_model", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$forecast_tab_historic_end_newmoonnumber,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "forecast_tab_historic_end_newmoonnumber", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))

  observeEvent(eventExpr   = input$evaluation_tab_species,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "evaluation_tab_species", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$evaluation_tab_dataset,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "evaluation_tab_dataset", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$evaluation_tab_model,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "evaluation_tab_model", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$evaluation_tab_historic_end_newmoonnumber,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "evaluation_tab_historic_end_newmoonnumber", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))
  observeEvent(eventExpr   = input$evaluation_tab_newmoonnumber,
               handlerExpr = event_reaction(main       = main, 
                                            event_name = "evaluation_tab_newmoonnumber", 
                                            rv         = rv, 
                                            input      = input, 
                                            output     = output, 
                                            session    = session))

}


#' @rdname web-app-server
#'
#' @export
#'
initial_reactive_values <- function (main = ".") {

  reactiveValues(forecast_tab_species                      = initial_forecast_tab_selected_species, 
                 forecast_tab_dataset                      = initial_forecast_tab_selected_dataset,
                 forecast_tab_model                        = initial_forecast_tab_selected_model,
                 forecast_tab_historic_end_newmoonnumber   = initial_forecast_tab_selected_historic_end_newmoonnumber,
                 evaluation_tab_species                    = initial_evaluation_tab_selected_species,
                 evaluation_tab_dataset                    = initial_evaluation_tab_selected_dataset,
                 evaluation_tab_model                      = initial_evaluation_tab_selected_model,
                 evaluation_tab_historic_end_newmoonnumber = initial_evaluation_tab_selected_historic_end_newmoonnumber,
                 evaluation_tab_newmoonnumber              = initial_evaluation_tab_selected_newmoonnumber)

}


#' @rdname web-app-server
#'
#' @export
#'
initial_output <- function (main = ".",
                            rv, 
                            output) {

  output$forecast_tab_species                    <- renderText(rv$forecast_tab_species)
  output$forecast_tab_dataset                    <- renderText(rv$forecast_tab_dataset)
  output$forecast_tab_model                      <- renderText(rv$forecast_tab_model)
  output$forecast_tab_historic_end_newmoonnumber <- renderText(rv$forecast_tab_historic_end_newmoonnumber)
  output$forecast_tab_ts_plot                    <- renderPlot(plot_cast_ts(main                          = main,
                                                                            dataset                       = rv$forecast_tab_dataset,
                                                                            species                       = rv$forecast_tab_species,
                                                                            historic_end_newmoonnumber    = rv$forecast_tab_historic_end_newmoonnumber,
                                                                            model                         = rv$forecast_tab_model))
  output$forecast_tab_ss_plot                    <- renderPlot(plot_cast_point(main                       = main,
                                                                               dataset                    = rv$forecast_tab_dataset,
                                                                               highlight_sp               = rv$forecast_tab_species,
                                                                               historic_end_newmoonnumber = rv$forecast_tab_historic_end_newmoonnumber,
                                                                               model                      = rv$forecast_tab_model))

  output$evaluation_tab_species                    <- renderText(rv$evaluation_tab_species)
  output$evaluation_tab_dataset                    <- renderText(rv$evaluation_tab_dataset)
  output$evaluation_tab_model                      <- renderText(rv$evaluation_tab_model)
  output$evaluation_tab_historic_end_newmoonnumber <- renderText(rv$evaluation_tab_historic_end_newmoonnumber)
  output$evaluation_tab_newmoonnumber              <- renderText(rv$evaluation_tab_newmoonnumber)

  output$evaluation_tab_sp_plot                    <- renderPlot(plot_cast_point(main                            = main,
                                                                                 dataset                         = rv$evaluation_tab_dataset,
                                                                                 highlight_sp                    = rv$evaluation_tab_species,
                                                                                 model                           = rv$evaluation_tab_model,
                                                                                 historic_end_newmoonnumber      = rv$evaluation_tab_historic_end_newmoonnumber,
                                                                                 newmoonnumber                   = rv$evaluation_tab_newmoonnumber,
                                                                                 with_census                     = TRUE))
  output$evaluation_tab_RMSE_plot                  <- renderPlot(plot_casts_cov_RMSE(main                        = main,
                                                                                     datasets                    = rv$evaluation_tab_dataset,
                                                                                     species                     = rv$evaluation_tab_species,
                                                                                     models                      = rv$evaluation_tab_model,
                                                                                     historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))

  output 

}

#' @rdname web-app-server
#'
#' @export
#'
event_reaction <- function (main,
                            event_name, 
                            rv, 
                            input, 
                            output, 
                            session) {

  rv     <- update_reactive_values(event_name = event_name, 
                                   rv         = rv, 
                                   input      = input)

  output <- update_output(main       = main, 
                          event_name = event_name, 
                          rv         = rv, 
                          input      = input, 
                          output     = output)

  update_input(event_name = event_name, 
               rv         = rv, 
               input      = input, 
               session    = session)

}

#' @rdname web-app-server
#'
#' @export
#'
update_reactive_values <- function (event_name, 
                                    rv, 
                                    input) {

  if (grepl("forecast_tab", event_name)) {

    rv$forecast_tab_species                    <- input$forecast_tab_species
    rv$forecast_tab_dataset                    <- input$forecast_tab_dataset
    rv$forecast_tab_model                      <- input$forecast_tab_model
    rv$forecast_tab_historic_end_newmoonnumber <- input$forecast_tab_historic_end_newmoonnumber

  }
  if (grepl("evaluation_tab", event_name)) {

    rv$evaluation_tab_species                    <- input$evaluation_tab_species
    rv$evaluation_tab_dataset                    <- input$evaluation_tab_dataset
    rv$evaluation_tab_model                      <- input$evaluation_tab_model
    rv$evaluation_tab_historic_end_newmoonnumber <- input$evaluation_tab_historic_end_newmoonnumber
    rv$evaluation_tab_newmoonnumber              <- input$evaluation_tab_newmoonnumber

  }


  rv

}


#' @rdname web-app-server
#'
#' @export
#'
update_output <- function (main, 
                           event_name,
                           rv, 
                           input, 
                           output) {


  if (grepl("forecast_tab", event_name)) {

    output$forecast_tab_species                    <- renderText(rv$forecast_tab_species)
    output$forecast_tab_dataset                    <- renderText(rv$forecast_tab_dataset)
    output$forecast_tab_model                      <- renderText(rv$forecast_tab_model)
    output$forecast_tab_historic_end_newmoonnumber <- renderText(rv$forecast_tab_historic_end_newmoonnumber)
    output$forecast_tab_ts_plot                    <- renderPlot(plot_cast_ts(main                          = main,
                                                                              dataset                       = rv$forecast_tab_dataset,
                                                                              species                       = rv$forecast_tab_species,
                                                                              historic_end_newmoonnumber    = rv$forecast_tab_historic_end_newmoonnumber,
                                                                              model                         = rv$forecast_tab_model))
    output$forecast_tab_ss_plot                    <- renderPlot(plot_cast_point(main                       = main,
                                                                                 dataset                    = rv$forecast_tab_dataset,
                                                                                 highlight_sp               = rv$forecast_tab_species,
                                                                                 historic_end_newmoonnumber = rv$forecast_tab_historic_end_newmoonnumber,
                                                                                 model                      = rv$forecast_tab_model))

  }
  if (grepl("evaluation_tab", event_name)) {

    output$evaluation_tab_species                    <- renderText(rv$evaluation_tab_species)
    output$evaluation_tab_dataset                    <- renderText(rv$evaluation_tab_dataset)
    output$evaluation_tab_model                      <- renderText(rv$evaluation_tab_model)
    output$evaluation_tab_historic_end_newmoonnumber <- renderText(rv$evaluation_tab_historic_end_newmoonnumber)
    output$evaluation_tab_newmoonnumber              <- renderText(rv$evaluation_tab_newmoonnumber)

    output$evaluation_tab_sp_plot                    <- renderPlot(plot_cast_point(main                            = main,
                                                                                   dataset                         = rv$evaluation_tab_dataset,
                                                                                   highlight_sp                    = rv$evaluation_tab_species,
                                                                                   model                           = rv$evaluation_tab_model,
                                                                                   historic_end_newmoonnumber      = rv$evaluation_tab_historic_end_newmoonnumber,
                                                                                   newmoonnumber                   = rv$evaluation_tab_newmoonnumber,
                                                                                   with_census                     = TRUE))
    output$evaluation_tab_RMSE_plot                  <- renderPlot(plot_casts_cov_RMSE(main                        = main,
                                                                                       datasets                    = rv$evaluation_tab_dataset,
                                                                                       species                     = rv$evaluation_tab_species,
                                                                                       models                      = rv$evaluation_tab_model,
                                                                                       historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))

  }


  output

}


#' @rdname web-app-server
#'
#' @export
#'
update_input <- function (event_name, 
                          rv, 
                          input, 
                          session) {

  if (event_name == "forecast_tab_species") {

    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))

  }
  if (event_name == "forecast_tab_dataset") {

    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))

  }
  if (event_name == "forecast_tab_model") {

    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
                                                  rv           = rv))

  }
  if (event_name == "forecast_tab_historic_end_newmoonnumber") {

    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "forecast_tab_species", 
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "forecast_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))

  }
  if (event_name == "evaluation_tab_species") {

    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name  = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(event_name = event_name,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(event_name   = event_name,
                                                        rv           = rv))

  }
  if (event_name == "evaluation_tab_dataset") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
                                                  rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(event_name = event_name,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(event_name   = event_name,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))

  }
  if (event_name == "evaluation_tab_model") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(event_name = event_name,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(event_name   = event_name,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name  = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name  = event_name,
                                                  rv          = rv))

  }
  if (event_name == "evaluation_tab_historic_end_newmoonnumber") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(event_name = event_name,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(event_name   = event_name,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name  = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name  = event_name,
                                                    rv        = rv),
                      selected = selected_dataset(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))

  }

  if (event_name == "evaluation_tab_newmoonnumber") {

    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_newmoonnumber", 
                      choices  = available_newmoonnumbers(event_name = event_name,
                                                          rv         = rv),
                      selected = selected_newmoonnumber(event_name   = event_name,
                                                        rv           = rv))
    updateSelectInput(session  = session, 
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name  = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name  = event_name,
                                                    rv        = rv),
                      selected = selected_dataset(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_historic_end_newmoonnumber", 
                      choices  = available_historic_end_newmoonnumbers(event_name = event_name,
                                                                       rv         = rv),
                      selected = selected_historic_end_newmoonnumber(event_name   = event_name,
                                                                     rv           = rv))

  }

}


