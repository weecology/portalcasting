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
#' @name web app helpers
#'
NULL


#' @rdname web-app-helpers
#'
#' @export
#'
available_newmoonnumbers <- function (main       = ".",
                                      event_name, 
                                      rv         = NULL) {

  if (event_name == "initial_evaluation_tab") {

    selected_evaluation_tab_species  <- selected_species(main = main, event_name = "initial_evaluation_tab")
    selected_evaluation_tab_model    <- selected_model(main = main, event_name = "initial_evaluation_tab")
    selected_evaluation_tab_dataset  <- selected_dataset(main = main, event_name = "initial_evaluation_tab")
    
    avail_newmoonnumbers  <- unique(casts_evaluations$newmoonnumber[casts_evaluations$species  == selected_evaluation_tab_species &
                                                                    casts_evaluations$model    == selected_evaluation_tab_model &
                                                                    casts_evaluations$dataset  == selected_evaluation_tab_dataset &
                                                                    !is.na(casts_evaluations$obs)])

  } else if (grepl("evaluation_tab_", event_name)) {

    all_possible <- unique(casts_evaluations$newmoonnumber)

    possible <- unique(casts_evaluations$newmoonnumber[casts_evaluations$species                    == rv$evaluation_tab_species &
                                                       casts_evaluations$model                      == rv$evaluation_tab_model &
                                                       casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                       casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                       !is.na(casts_evaluations$obs)])

    avail_newmoonnumbers <- possible[possible %in% all_possible]

  }

  avail_newmoonnumbers

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_newmoonnumber <- function (main       = ".",
                                    event_name,
                                    default    = NULL,
                                    rv         = NULL) {

  available <- available_newmoonnumbers(main       = main,
                                        event_name = event_name,
                                        rv         = rv)

  if (grepl("initial_", event_name)) {

    default              <- ifnull(default, max(available))
    select_newmoonnumber <- default

  } else if (grepl("evaluation_tab_", event_name)) {

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
available_historic_end_newmoonnumbers <- function (main       = ".",
                                                   event_name, 
                                                   rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_historic_end_newmoonnumbers  <- unique(casts_metadata$historic_end_newmoonnumber)

  } else if (event_name == "initial_evaluation_tab") {

    selected_evaluation_tab_species       <- selected_species(main = main, event_name = "initial_evaluation_tab")
    selected_evaluation_tab_model         <- selected_model(main = main, event_name = "initial_evaluation_tab")
    selected_evaluation_tab_dataset       <- selected_dataset(main = main, event_name = "initial_evaluation_tab")
    selected_evaluation_tab_newmoonnumber <- selected_newmoonnumber(main = main, event_name = "initial_evaluation_tab")
    
    avail_historic_end_newmoonnumbers  <- unique(casts_evaluations$historic_end_newmoonnumber[casts_evaluations$species                      == selected_evaluation_tab_species &
                                                                                              casts_evaluations$model                        == selected_evaluation_tab_model &
                                                                                              casts_evaluations$dataset                      == selected_evaluation_tab_dataset &
                                                                                              casts_evaluations$newmoonnumber                == selected_evaluation_tab_newmoonnumber & 
                                                                                              !is.na(casts_evaluations$obs)])

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(casts_metadata$historic_end_newmoonnumber)

    possible <- unique(casts_metadata$historic_end_newmoonnumber[casts_metadata$species == rv$forecast_tab_species &
                                                                 casts_metadata$model   == rv$forecast_tab_model &
                                                                 casts_metadata$dataset == rv$forecast_tab_dataset])

    avail_historic_end_newmoonnumbers <- possible[possible %in% all_possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    all_possible <- unique(casts_evaluations$historic_end_newmoonnumber)

    possible <- unique(casts_evaluations$historic_end_newmoonnumber[casts_evaluations$species       == rv$evaluation_tab_species &
                                                                    casts_evaluations$model         == rv$evaluation_tab_model &
                                                                    casts_evaluations$dataset       == rv$evaluation_tab_dataset & 
                                                                    casts_evaluations$newmoonnumber == rv$evaluation_tab_newmoonnumber & 
                                                                    !is.na(casts_evaluations$obs)])

    avail_historic_end_newmoonnumbers <- possible[possible %in% all_possible]

  }


  avail_historic_end_newmoonnumbers

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_historic_end_newmoonnumber <- function (main       = ".",
                                                 event_name,
                                                 default    = NULL,
                                                 rv         = NULL) {

  available <- available_historic_end_newmoonnumbers(main       = main,
                                                     event_name = event_name,
                                                     rv         = rv)

  if (grepl("initial_", event_name)) {

    default                           <- ifnull(default, max(available))
    select_historic_end_newmoonnumber <- default

  } else if (grepl("forecast_tab_", event_name)) {

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
available_species <- function (main       = ".",
                               event_name, 
                               rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_species  <- unique(casts_metadata$species)

  } else if (event_name == "initial_evaluation_tab") {

    avail_species  <- unique(casts_evaluations$species)

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(casts_metadata$species[casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

    possible <- unique(casts_metadata$species[casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                              casts_metadata$model                      == rv$forecast_tab_model &
                                              casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_species <- possible[possible %in% all_possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    all_possible <- unique(casts_evaluations$species[casts_evaluations$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

    possible <- unique(casts_evaluations$species[casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                 casts_evaluations$model                      == rv$evaluation_tab_model &
                                                 casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                 casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber & 
                                                 !is.na(casts_evaluations$obs)])

    avail_species <- possible[possible %in% all_possible]

  } 

  latin_names <- rodent_species(set = "forecasting", type = "Latin", total = TRUE)
  code_names  <- rodent_species(set = "forecasting", type = "code", total = TRUE)

  names(avail_species) <- latin_names[match(avail_species, code_names)]

  avail_species

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_species <- function (main       = ".",
                              event_name,
                              default    = "DM",
                              rv         = NULL) {

  available <- available_species(main       = main,
                                 event_name = event_name,
                                 rv         = rv)

  if (grepl("initial_", event_name)) {

    select_species <- default

  } else if (grepl("forecast_tab_", event_name)) {

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
available_datasets <- function (main       = ".",
                                event_name, 
                                rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_datasets  <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( )])

  } else if (event_name == "initial_evaluation_tab") {

    avail_datasets  <- unique(casts_evaluations$dataset[casts_evaluations$dataset %in% prefab_datasets( )])

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( )])

    possible <- unique(casts_metadata$dataset[casts_metadata$species                    == rv$forecast_tab_species &
                                              casts_metadata$model                      == rv$forecast_tab_model &
                                              casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_datasets <- possible[possible %in% all_possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    all_possible <- unique(casts_evaluations$dataset[casts_evaluations$dataset %in% prefab_datasets( )])

    possible <- unique(casts_evaluations$dataset[casts_evaluations$species                    == rv$evaluation_tab_species &
                                                 casts_evaluations$model                      == rv$evaluation_tab_model &
                                                 casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                 casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber & 
                                                 !is.na(casts_evaluations$obs)])

    avail_datasets <- possible[possible %in% all_possible]
  }

  avail_datasets

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_dataset <- function (main       = ".",
                              event_name,
                              default    = "controls",
                              rv         = NULL) {

  available <- available_datasets(main       = main,
                                  event_name = event_name,
                                  rv         = rv)

  if (grepl("initial_", event_name)) {

    select_dataset <- default

  } else if (grepl("forecast_tab_", event_name)) {

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
available_models <- function (main       = ".",
                              event_name, 
                              rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    avail_models  <- unique(casts_metadata$model[casts_metadata$model %in% prefab_models( )])

  } else if (event_name == "initial_evaluation_tab") {

    avail_models  <- unique(casts_evaluations$model[casts_evaluations$model %in% prefab_models( )])

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(casts_metadata$model[casts_metadata$model %in% prefab_models( )])

    possible <- unique(casts_metadata$model[casts_metadata$species                    == rv$forecast_tab_species &
                                            casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                            casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_models <- possible[possible %in% all_possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    all_possible <- unique(casts_evaluations$model[casts_evaluations$model %in% prefab_models( )])

    possible <- unique(casts_evaluations$model[casts_evaluations$species                    == rv$evaluation_tab_species &
                                               casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                               casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                               casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber & 
                                               !is.na(casts_evaluations$obs)])

    avail_models <- possible[possible %in% all_possible]

  }

  print_name <- unlist(mapply(getElement, model_controls(main = main), "metadata")["print_name", ])
  model_name <- unlist(mapply(getElement, model_controls(main = main), "metadata")["name", ])

  names(avail_models) <- print_name[match(avail_models, model_name)]

  avail_models

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_model <- function (main       = ".",
                            event_name,
                            default    = "AutoArima",
                            rv         = NULL) {

  available <- available_models(main       = main,
                                  event_name = event_name,
                                  rv         = rv)

  if (grepl("initial_", event_name)) {

    select_model <- default

  } else if (grepl("forecast_tab_", event_name)) {

    select_model   <- rv$forecast_tab_model

  } else if (grepl("evaluation_tab_", event_name)) {

    select_model   <- rv$evaluation_tab_model

  }

  if (!(select_model %in% available)) {

    select_model <- available[1]

  }  

  available[available == select_model]

}




