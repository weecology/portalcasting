#' @title Build and Launch the Portal Forecast Web Application
#' 
#' @description Constructs and launches a local version of the web application by running [`shiny::runApp`] pointed to the `app` subdirectory in the local `portalcasting` package folder.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @name web app
#'
NULL

#' @rdname web-app
#'
#' @export
#'
run_web_app <- function(main = ".") {

  app_directory <- system.file(...     = "app", 
                               package = "portalcasting")

  models_rmd <- system.file(...     = "app", 
                            ...     = "models.Rmd",
                            package = "portalcasting")
  render(models_rmd)

  profiles_r <- system.file(...     = "app", 
                            ...     = "profile_html.R",
                            package = "portalcasting")
  source(profiles_r)

  file.copy(from      = list.files(app_directory, full.names = TRUE),
            to        = file.path(main),
            recursive = TRUE,
            overwrite = TRUE)

  runApp(appDir         = file.path(main),
         launch.browser = TRUE)


}


#' @title Helper Functions for the Web App
#' 
#' @description Construct vectors of available choices and make selections.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param event_name `character` value of the server event. \cr
#'   Options include `"forecast_tab_species"`, `"forecast_tab_dataset"`, `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`, `"evaluation_tab_species"`, `"evaluation_tab_dataset"`, `"evaluation_tab_model"`, and `"evaluation_tab_historic_end_newmoonnumber"`)
#'
#' @param rv [`reactiveValues`][shiny::reactiveValues] `list` for the UI.
#'
#' @param `global` A `list` of global values for the app.
#'
#' @name web app helpers
#'
NULL


#' @rdname web-app-helpers
#'
#' @export
#'
available_newmoonnumbers <- function (global     = global_list( ),
                                      event_name, 
                                      rv         = NULL) {

  if (event_name == "initial_evaluation_tab") {

    avail_newmoonnumbers  <- global$initial_evaluation_tab_available_newmoonnumbers

  } else if (grepl("evaluation_tab_", event_name)) {


    possible <- unique(casts_evaluations$newmoonnumber[casts_evaluations$species                    == rv$evaluation_tab_species &
                                                       casts_evaluations$model                      == rv$evaluation_tab_model &
                                                       casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                       casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                       !is.na(casts_evaluations$obs)])

    avail_newmoonnumbers <- possible[possible %in% global$initial_evaluation_tab_available_newmoonnumbers]

  }

  avail_newmoonnumbers

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_newmoonnumber <- function (global     = global_list( ),
                                    event_name,
                                    rv         = NULL) {

  available <- available_newmoonnumbers(global     = global,
                                        event_name = event_name,
                                        rv         = rv)

  if (grepl("evaluation_tab_", event_name)) {

    select_newmoonnumber <- rv$evaluation_tab_newmoonnumber

  }

  if (!(select_newmoonnumber %in% available)) {

    select_newmoonnumber <- available[1]

  }  

  available[available == select_newmoonnumber]

}



#' @rdname web-app-helpers
#'
#' @export
#'
available_historic_end_newmoonnumbers <- function (global     = global_list( ),
                                                   event_name, 
                                                   rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_historic_end_newmoonnumbers  <- global$initial_forecast_tab_available_historic_end_newmoonnumbers

  } else if (event_name == "initial_evaluation_tab") {

    avail_historic_end_newmoonnumbers  <- global$initial_evaluation_tab_available_historic_end_newmoonnumbers

  } else if (grepl("forecast_tab_", event_name)) {

    possible <- unique(casts_metadata$historic_end_newmoonnumber[casts_metadata$species == rv$forecast_tab_species &
                                                                 casts_metadata$model   == rv$forecast_tab_model &
                                                                 casts_metadata$dataset == rv$forecast_tab_dataset])

    avail_historic_end_newmoonnumbers <- possible[possible %in% global$initial_forecast_tab_available_historic_end_newmoonnumbers]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(casts_evaluations$historic_end_newmoonnumber[casts_evaluations$species       == rv$evaluation_tab_species &
                                                                    casts_evaluations$model         == rv$evaluation_tab_model &
                                                                    casts_evaluations$dataset       == rv$evaluation_tab_dataset & 
                                                                    casts_evaluations$newmoonnumber == rv$evaluation_tab_newmoonnumber])

    avail_historic_end_newmoonnumbers <- possible[possible %in% global$initial_evaluation_tab_available_historic_end_newmoonnumbers]

  }


  avail_historic_end_newmoonnumbers

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_historic_end_newmoonnumber <- function (global     = global_list( ),
                                                 event_name,
                                                 rv         = NULL) {

  available <- available_historic_end_newmoonnumbers(global     = global,
                                                     event_name = event_name,
                                                     rv         = rv)

  if (grepl("forecast_tab_", event_name)) {

    select_historic_end_newmoonnumber   <- rv$forecast_tab_historic_end_newmoonnumber

  } else if (grepl("evaluation_tab_", event_name)) {

    select_historic_end_newmoonnumber   <- rv$evaluation_tab_historic_end_newmoonnumber

  }

  if (!(select_historic_end_newmoonnumber %in% available)) {

    select_historic_end_newmoonnumber <- available[1]

  }  

  available[available == select_historic_end_newmoonnumber]

}


#' @rdname web-app-helpers
#'
#' @export
#'
available_species <- function (global     = global_list( ),
                               event_name, 
                               rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_species  <- global$initial_forecast_tab_available_species

  } else if (event_name == "initial_evaluation_tab") {

    avail_species  <- global$initial_evaluation_tab_available_species

  } else if (grepl("forecast_tab_", event_name)) {

    possible <- unique(casts_metadata$species[casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                              casts_metadata$model                      == rv$forecast_tab_model &
                                              casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_species <- possible[possible %in% global$initial_forecast_tab_available_species]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(casts_evaluations$species[casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                 casts_evaluations$model                      == rv$evaluation_tab_model &
                                                 casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                 casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber])

    avail_species <- possible[possible %in% global$initial_evaluation_tab_available_species]

  } 



  names(avail_species) <- latin_names[match(avail_species, code_names)]

  avail_species

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_species <- function (global     = global_list( ),
                              event_name,
                              rv         = NULL) {

  available <- available_species(global     = global,
                                 event_name = event_name,
                                 rv         = rv)

 if (grepl("forecast_tab_", event_name)) {

    select_species   <- rv$forecast_tab_species

  } else if (grepl("evaluation_tab_", event_name)) {

    select_species   <- rv$evaluation_tab_species

  }

  if (!(select_species %in% available)) {

    select_species <- available[1]

  }

  available[available == select_species]

}


#' @rdname web-app-helpers
#'
#' @export
#'
available_datasets <- function (global     = global_list( ),
                                event_name, 
                                rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_datasets  <- global$initial_forecast_tab_available_datasets

  } else if (event_name == "initial_evaluation_tab") {

    avail_datasets  <- global$initial_evaluation_tab_available_datasets

  } else if (grepl("forecast_tab_", event_name)) {

    possible <- unique(casts_metadata$dataset[casts_metadata$species                    == rv$forecast_tab_species &
                                              casts_metadata$model                      == rv$forecast_tab_model &
                                              casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_datasets <- possible[possible %in% global$initial_forecast_tab_available_datasets]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(casts_evaluations$dataset[casts_evaluations$species                    == rv$evaluation_tab_species &
                                                 casts_evaluations$model                      == rv$evaluation_tab_model &
                                                 casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                 casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber])

    avail_datasets <- possible[possible %in% global$initial_evaluation_tab_available_datasets]
  }

  avail_datasets

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_dataset <- function (global     = global_list( ),
                              main       = ".",
                              event_name,
                              rv         = NULL) {

  available <- available_datasets(global     = global,
                                  event_name = event_name,
                                  rv         = rv)

 if (grepl("forecast_tab_", event_name)) {

    select_dataset   <- rv$forecast_tab_dataset

  } else if (grepl("evaluation_tab_", event_name)) {

    select_dataset   <- rv$evaluation_tab_dataset

  }

  if (!(select_dataset %in% available)) {

    select_dataset <- available[1]

  }  

  available[available == select_dataset]

}


#' @rdname web-app-helpers
#'
#' @export
#'
available_models <- function (global     = global_list( ),
                              event_name, 
                              rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_models  <- global$initial_forecast_tab_available_models

  } else if (event_name == "initial_evaluation_tab") {

    avail_models  <- global$initial_evaluation_tab_available_models

  } else if (grepl("forecast_tab_", event_name)) {

    possible <- unique(casts_metadata$model[casts_metadata$species                    == rv$forecast_tab_species &
                                            casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                            casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_models <- possible[possible %in% global$initial_forecast_tab_available_models]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(casts_evaluations$model[casts_evaluations$species                    == rv$evaluation_tab_species &
                                               casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                               casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                               casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber ])

    avail_models <- possible[possible %in% global$initial_evaluation_tab_available_models]

  }

  names(avail_models) <- print_name[match(avail_models, model_name)]

  avail_models

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_model <- function (global     = global_list( ),
                            event_name,
                            rv         = NULL) {

  available <- available_models(global     = global,
                                event_name = event_name,
                                rv         = rv)

  if (grepl("forecast_tab_", event_name)) {

    select_model   <- rv$forecast_tab_model

  } else if (grepl("evaluation_tab_", event_name)) {

    select_model   <- rv$evaluation_tab_model

  }

  if (!(select_model %in% available)) {

    select_model <- available[1]

  }  

  available[available == select_model]

}




