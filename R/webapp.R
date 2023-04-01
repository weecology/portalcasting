portal_forecast_ui <- function (main = ".") {

  models_rmd <- system.file(...     = "app", 
                            ...     = "models.Rmd",
                            package = "portalcasting")
  render(models_rmd)

  profiles_r <- system.file(...     = "app", 
                            ...     = "profile_html.R",
                            package = "portalcasting")
  source(profiles_r)


  fluidPage(page_title_panel( ),
            page_subtitle_panel( ),
            page_main_panel( ))


}



page_main_panel <- function ( ) {

  mainPanel(tabsetPanel(forecast_tab( ),
                        evaluation_tab( ),
                        about_tab( ),
                        models_tab( ),
                        profiles_tab( )))


}

evaluation_tab <- function ( ) {


  tabPanel(title = "Evaluation", 
           br( ), 
           evaluation_tab_input_selection_row( ), 
           h2("Most recent observation vs. forecasts"),
           evaluation_tab_input_selection_checks_row( ),   # used for checking reactive inputs in dev
           plotOutput("evaluation_tab_sp_plot"),
           br( ),
           h2("Model Coverage & RMSE (last 3 years of forecasts)"),
 #          plotOutput("evaluation_tab_RMSE_plot"),
           br( ))

}


evaluation_tab_input_selection_checks_row <- function ( ) {

  fluidRow(textOutput("evaluation_tab_species"), 
           textOutput("evaluation_tab_dataset"), 
           textOutput("evaluation_tab_model"), 
           textOutput("evaluation_tab_historic_end_newmoonnumber"))

}

evaluation_tab_input_selection_row <- function ( ) {


  fluidRow(evaluation_tab_input_selection_row_species( ),
           evaluation_tab_input_selection_row_dataset( ),
           evaluation_tab_input_selection_row_model( ),
           evaluation_tab_input_selection_row_historic_end_newmoonnumber( ))



}


evaluation_tab_input_selection_row_species <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_species",
                     label    = "Species",
                     choices  = species_list( ),
                     selected = "DM"))

}

evaluation_tab_input_selection_row_dataset <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_dataset",
                     label    = "Dataset",
                     choices  = prefab_datasets( ),
                     selected = "controls"))

}


evaluation_tab_input_selection_row_model <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_model",
                     label    = "Model",
                     choices  = model_list(),
                     selected = "AutoArima"))

}


evaluation_tab_input_selection_row_historic_end_newmoonnumber <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_historic_end_newmoonnumber",
                     label    = "Origin Newmoon",
                     choices  = historic_end_newmoonnumber_list( ),
                     selected = 560))

}



forecast_tab <- function ( ) {


  tabPanel(title = "Forecast", 
           br( ), 
           forecast_tab_input_selection_row( ), 
          # forecast_tab_input_selection_checks_row( ),   # used for checking reactive inputs in dev
           plotOutput("forecast_tab_ts_plot"),
           br( ),
           plotOutput("forecast_tab_ss_plot"),
           br( ))

}


forecast_tab_input_selection_checks_row <- function ( ) {

  fluidRow(textOutput("forecast_tab_species"), 
           textOutput("forecast_tab_dataset"), 
           textOutput("forecast_tab_model"), 
           textOutput("forecast_tab_historic_end_newmoonnumber"))

}

forecast_tab_input_selection_row <- function ( ) {


  fluidRow(forecast_tab_input_selection_row_species( ),
           forecast_tab_input_selection_row_dataset( ),
           forecast_tab_input_selection_row_model( ),
           forecast_tab_input_selection_row_historic_end_newmoonnumber( ))



}


forecast_tab_input_selection_row_species <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_species",
                     label    = "Species",
                     choices  = species_list( ),
                     selected = "DM"))

}

forecast_tab_input_selection_row_dataset <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_dataset",
                     label    = "Dataset",
                     choices  = prefab_datasets( ),
                     selected = "controls"))

}


forecast_tab_input_selection_row_model <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_model",
                     label    = "Model",
                     choices  = model_list(),
                     selected = "AutoArima"))

}


forecast_tab_input_selection_row_historic_end_newmoonnumber <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_historic_end_newmoonnumber",
                     label    = "Origin Newmoon",
                     choices  = historic_end_newmoonnumber_list( ),
                     selected = 566))

}


historic_end_newmoonnumber_list <- function ( ) {

# how do we get these from external? how can we pass main in?
  525:566

}

model_list <- function ( ) {

  out <- prefab_models()
  names(out) <- unlist(mapply(getElement, prefab_model_controls(), "metadata")["print_name", ])

  out
}

species_list <- function ( ) {

  out <- rodent_species(set = "forecasting", type = "code", total = TRUE)
  names(out) <- rodent_species(set = "forecasting", type = "Latin", total = TRUE)

  out
}


about_tab <- function ( ) {

  tabPanel(title = "About",
           includeMarkdown("about.md")) 

}

models_tab <- function ( ) {

  tabPanel(title = "Models",
           includeHTML("models.html")) 

}

profiles_tab <- function ( ) {

  tabPanel(title = "Rodent Profiles",
           includeHTML("profile.html")) 

}

page_title_panel <- function ( ) {

  app_title <- "Portal Project Forecasting"
  titlePanel(title = app_title)

}

page_subtitle_panel <- function ( ) {

  href <- a(href   = "http://portal.weecology.org", 
                     "The Portal Project", 
            target = "_blank")

  p(HTML(text = paste0("Forecasts for the population and community dynamics of ", href, ".")))

}




portal_forecast_server <- function (main = "~/prod", input, output, session) {


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




initial_reactive_values <- function (main = ".") {

  casts_metadata    <- read_casts_metadata(main    = main)
  casts_evaluations <- read_casts_evaluations(main = main)

  reactiveValues(forecast_tab_species                      = "DM", 
                 forecast_tab_dataset                      = "all", 
                 forecast_tab_model                        = "AutoArima",
                 forecast_tab_historic_end_newmoonnumber   = max(casts_metadata$historic_end_newmoonnumber),
                 evaluation_tab_species                    = "DM",
                 evaluation_tab_dataset                    = "all", 
                 evaluation_tab_model                      = "AutoArima",
                 evaluation_tab_historic_end_newmoonnumber = 560, # max(casts_evaluations$historic_end_newmoonnumber), # not sure how best to auto select
                 casts_metadata                            = casts_metadata,
                 casts_evaluations                         = casts_evaluations)

}

initial_output <- function (rv, output) {

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

  output$evaluation_tab_sp_plot                    <- renderPlot(plot_cast_point(main                            = main,
                                                                                 dataset                         = rv$evaluation_tab_dataset,
                                                                                 highlight_sp                    = rv$evaluation_tab_species,
                                                                                 model                           = rv$evaluation_tab_model,
                                                                                 historic_end_newmoonnumber      = rv$evaluation_tab_historic_end_newmoonnumber,
                                                                                 with_census                     = TRUE))
  output$evaluation_tab_RMSE_plot                  <- renderPlot(plot_casts_cov_RMSE(main                        = main,
                                                                                     dataset                     = rv$evaluation_tab_dataset,
                                                                                     species                     = rv$evaluation_tab_species,
                                                                                     model                       = rv$evaluation_tab_model,
                                                                                     historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))
  output 

}



event_reaction <- function (main, event_name, rv, input, output, session) {

  rv     <- update_reactive_values(main       = main, 
                                   event_name = event_name, 
                                   rv         = rv, 
                                   input      = input)

  output <- update_output(main       = main, 
                          event_name = event_name, 
                          rv         = rv, 
                          input      = input, 
                          output     = output)


  update_input(main       = main, 
               event_name = event_name, 
               rv         = rv, 
               input      = input, 
               output     = output, 
               session    = session)


}


update_reactive_values <- function (main, event_name, rv, input) {

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

  }


  rv

}



update_output <- function (main, event_name, rv, input, output) {

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

    output$evaluation_tab_sp_plot                    <- renderPlot(plot_cast_point(main                            = main,
                                                                                   dataset                         = rv$evaluation_tab_dataset,
                                                                                   highlight_sp                    = rv$evaluation_tab_species,
                                                                                   model                           = rv$evaluation_tab_model,
                                                                                   historic_end_newmoonnumber      = rv$evaluation_tab_historic_end_newmoonnumber,
                                                                                   with_census                     = TRUE))
    output$evaluation_tab_RMSE_plot                  <- renderPlot(plot_casts_cov_RMSE(main                        = main,
                                                                                       dataset                     = rv$evaluation_tab_dataset,
                                                                                       species                     = rv$evaluation_tab_species,
                                                                                       model                       = rv$evaluation_tab_model,
                                                                                       historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))


  }


  output

}



update_input <- function (main, event_name, rv, input, output, session) {

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
                                                   rv          = rv))
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
                      selected = selected_dataset(event_name  = event_name,
                                                  rv          = rv))

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
                      selected = selected_dataset(event_name  = event_name,
                                                  rv          = rv))
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
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name   = event_name,
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
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name = event_name,
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
                      inputId  = "evaluation_tab_species", 
                      choices  = available_species(event_name = event_name,
                                                   rv         = rv),
                      selected = selected_species(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_dataset", 
                      choices  = available_datasets(event_name = event_name,
                                                    rv         = rv),
                      selected = selected_dataset(event_name  = event_name,
                                                  rv          = rv))
    updateSelectInput(session  = session,
                      inputId  = "evaluation_tab_model", 
                      choices  = available_models(event_name = event_name,
                                                  rv         = rv),
                      selected = selected_model(event_name   = event_name,
                                                rv           = rv))

  }

}




selected_historic_end_newmoonnumber <- function (event_name, 
                                                 rv) {

  available <- available_historic_end_newmoonnumbers(event_name = event_name,
                                                     rv         = rv)
  
  if (grepl("forecast_tab", event_name)) {

    historic_end_newmoonnumber <- rv$forecast_tab_historic_end_newmoonnumber

  }

  if (grepl("evaluation_tab", event_name)) {

    historic_end_newmoonnumber <- rv$evaluation_tab_historic_end_newmoonnumber

  }

  if (!(historic_end_newmoonnumber %in% available)) {

    historic_end_newmoonnumber <- available[1]

  }  

  historic_end_newmoonnumber_list()[historic_end_newmoonnumber_list() %in% historic_end_newmoonnumber]

}


available_historic_end_newmoonnumbers <- function (event_name, 
                                                   rv) {

  if (grepl("forecast_tab", event_name)) {

    all_possible <- unique(rv$casts_metadata$historic_end_newmoonnumber)

    possible <- unique(rv$casts_metadata$historic_end_newmoonnumber[rv$casts_metadata$model   == rv$forecast_tab_model &
                                                                    rv$casts_metadata$species == rv$forecast_tab_species &
                                                                    rv$casts_metadata$dataset == rv$forecast_tab_dataset])
  }

  if (grepl("evaluation_tab", event_name)) {

    all_possible <- unique(rv$casts_evaluations$historic_end_newmoonnumber)

    possible <- unique(rv$casts_evaluations$historic_end_newmoonnumber[rv$casts_evaluations$model   == rv$evaluation_tab_model &
                                                                       rv$casts_evaluations$species == rv$evaluation_tab_species &
                                                                       rv$casts_evaluations$dataset == rv$evaluation_tab_dataset])

  }

  historic_end_newmoonnumbers <- possible[possible %in% all_possible]
  historic_end_newmoonnumber_list()[historic_end_newmoonnumber_list() %in% historic_end_newmoonnumbers]

}






selected_species <- function (event_name, 
                              rv) {

  available <- available_species(event_name = event_name,
                                 rv         = rv)
  
  if (grepl("forecast_tab", event_name)) {

    species   <- rv$forecast_tab_species

  }

  if (grepl("evaluation_tab", event_name)) {

    species   <- rv$evaluation_tab_species

  }

  if (!(species %in% available)) {

    species <- available[1]

  }  

  species_list()[species_list() %in% species]

}


available_species <- function (event_name, 
                               rv) {

  if (grepl("forecast_tab", event_name)) {

    all_possible <- unique(rv$casts_metadata$species[rv$casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

    possible <- unique(rv$casts_metadata$species[rv$casts_metadata$model                      == rv$forecast_tab_model &
                                                 rv$casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                                 rv$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

  }

  if (grepl("evaluation_tab", event_name)) {

    all_possible <- unique(rv$casts_evaluations$species[rv$casts_evaluations$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

    possible <- unique(rv$casts_evaluations$species[rv$casts_evaluations$model                      == rv$evaluation_tab_model &
                                                    rv$casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                    rv$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber])

  }

  species <- possible[possible %in% all_possible]
  species_list()[species_list() %in% species]

}



selected_dataset <- function (event_name, 
                              rv) {

  available <- available_datasets(event_name = event_name,
                                  rv        = rv)


  if (grepl("forecast_tab", event_name)) {


    dataset   <- rv$forecast_tab_dataset

  }

  if (grepl("evaluation_tab", event_name)) {

    dataset   <- rv$evaluation_tab_dataset

  }

  if (!(dataset %in% available)) {

    dataset <- available[1]

  }  

  dataset

}

available_datasets <- function (main,
                                event_name, 
                                rv) {

  if (grepl("forecast_tab", event_name)) {

    all_possible <- unique(rv$casts_metadata$dataset[rv$casts_metadata$dataset %in% prefab_datasets()])

    possible <- unique(rv$casts_metadata$dataset[rv$casts_metadata$model                      == rv$forecast_tab_model &
                                                 rv$casts_metadata$species                    == rv$forecast_tab_species &
                                                 rv$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

  }

  if (grepl("evaluation_tab", event_name)) {

    all_possible <- unique(rv$casts_evaluations$dataset[rv$casts_evaluations$dataset %in% prefab_datasets()])

    possible <- unique(rv$casts_evaluations$dataset[rv$casts_evaluations$model                      == rv$evaluation_tab_model &
                                                    rv$casts_evaluations$species                    == rv$evaluation_tab_species &
                                                    rv$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber])


  }

  possible[possible %in% all_possible]

}





selected_model <- function (event_name, 
                            rv) {

  available <- available_models(event_name = event_name, 
                                rv        = rv)

  if (grepl("forecast_tab", event_name)) {

    model     <- rv$forecast_tab_model

  }

  if (grepl("evaluation_tab", event_name)) {

    model     <- rv$evaluation_tab_model

  }

  if (!(model %in% available)) {

    model <- available[1]

  }  

  model_list()[model_list() %in% model]

}

available_models <- function (event_name, 
                              rv) {

  if (grepl("forecast_tab", event_name)) {

    all_possible <- unique(rv$casts_metadata$model[rv$casts_metadata$model %in% prefab_models()])

    possible <- unique(rv$casts_metadata$model[rv$casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                               rv$casts_metadata$species                    == rv$forecast_tab_species &
                                               rv$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])
  }

  if (grepl("evaluation_tab", event_name)) {

    all_possible <- unique(rv$casts_evaluations$model[rv$casts_evaluations$model %in% prefab_models()])

    possible <- unique(rv$casts_evaluations$model[rv$casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                  rv$casts_evaluations$species                    == rv$evaluation_tab_species &
                                                  rv$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber])

  }

  models <- possible[possible %in% all_possible]
  model_list()[model_list() %in% models]

}



#' @title Launch the Portal Forecast Web Application
#' 
#' @description Launches a local version of the web application by running \code{\link[shiny]{runApp}} pointed to the \code{app} subdirectory in the local \code{portalcasting} package folder.
#'
#' @export
#'
run_web_app <- function( ) {

  app_directory <- system.file(...     = "app", 
                               package = "portalcasting")

  runApp(appDir         = app_directory,
         launch.browser = TRUE)


}