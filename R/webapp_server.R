
#' @title Generate the Server Code for the Web App
#' 
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param input `input` list for the UI.
#'
#' @param output `output` list for the GenEst GUI.
#'
#' @param session Environment for the GenEst GUI.
#'
#' @return 
#'   * \code{portal_forecast_server}: An observer reference class object (see [`shiny::observeEvent`] and [`shiny::observe`]).
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

  output <- initial_output(rv     = rv, 
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

}





