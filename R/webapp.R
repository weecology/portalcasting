







historic_end_newmoonnumber_list <- function (main = ".") {

  unique(read_casts_metadata(main = main)$historic_end_newmoonnumber)

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
                                                                                     datasets                    = rv$evaluation_tab_dataset,
                                                                                     species                     = rv$evaluation_tab_species,
                                                                                     models                      = rv$evaluation_tab_model,
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
                                                                                       datasets                    = rv$evaluation_tab_dataset,
                                                                                       species                     = rv$evaluation_tab_species,
                                                                                       models                      = rv$evaluation_tab_model,
                                                                                       historic_end_newmoonnumbers = rv$evaluation_tab_historic_end_newmoonnumber))

  }


  output

}



update_input <- function (main, event_name, rv, input, session) {

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




#' @rdname web-app
#'
#' @export
#'
run_web_app <- function(main = ".") {

  app_directory <- system.file(...     = "app", 
                               package = "portalcasting")

  runApp(appDir         = app_directory,
         launch.browser = TRUE)


}



#' @title Build and Launch the Portal Forecast Web Application
#' 
#' @description Constructs and launches a local version of the web application by running [`shiny::runApp`] pointed to the `app` subdirectory in the local `portalcasting` package folder.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @name web app
#'
NULL
