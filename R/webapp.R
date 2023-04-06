#' @title Build and Launch the Portal Forecast Web Application
#' 
#' @description `run_web_app` constructs and launches a local version of the web application by running [`shiny::runApp`] pointed to the `app` subdirectory in the local `portalcasting` package folder. \cr \cr
#'              `global_list` creates a list of values that are globally available in an app run.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @return `global_list`: a `list` of values that will be globally available in the app.
#'
#' @name web app
#'
NULL

#' @rdname web-app
#'
#' @export
#'
run_web_app <- function(main = ".") {

  settings <- read_directory_settings(main = main)

  runApp(appDir         = file.path(main, settings$subdirectories$app),
         launch.browser = TRUE)


}


#' @title Helper Functions for the Web App
#' 
#' @description Construct vectors of available choices and make selections.
#'
#' @param event_name `character` value of the server event. \cr
#'   Options include `"initial_forecast_tab"`, `"initial_evaluation_tab"`, `"forecast_tab_species"`, `"forecast_tab_dataset"`, `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`, `"evaluation_tab_species"`, `"evaluation_tab_dataset"`, `"evaluation_tab_model"`, `"evaluation_tab_historic_end_newmoonnumber"`, and `"evaluation_tab_newmoonnumber"`)
#'
#' @param rv [`reactiveValues`][shiny::reactiveValues] `list` for the UI.
#'
#' @param global A `list` of global values for the app.
#'
#' @return `character` or `integer`, depending upon the function.
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


    possible <- unique(global$casts_evaluations$newmoonnumber[global$casts_evaluations$species                    == rv$evaluation_tab_species &
                                                              global$casts_evaluations$model                      == rv$evaluation_tab_model &
                                                              global$casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                              global$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber])

    avail_newmoonnumbers <- global$initial_evaluation_tab_available_newmoonnumbers[global$initial_evaluation_tab_available_newmoonnumbers %in% possible]

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

    possible <- unique(global$casts_metadata$historic_end_newmoonnumber[global$casts_metadata$species == rv$forecast_tab_species &
                                                                        global$casts_metadata$model   == rv$forecast_tab_model &
                                                                        global$casts_metadata$dataset == rv$forecast_tab_dataset])

    avail_historic_end_newmoonnumbers <- global$initial_forecast_tab_available_historic_end_newmoonnumbers[global$initial_forecast_tab_available_historic_end_newmoonnumbers %in% possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(global$casts_evaluations$historic_end_newmoonnumber[global$casts_evaluations$species       == rv$evaluation_tab_species &
                                                                           global$casts_evaluations$model         == rv$evaluation_tab_model &
                                                                           global$casts_evaluations$dataset       == rv$evaluation_tab_dataset & 
                                                                           global$casts_evaluations$newmoonnumber == rv$evaluation_tab_newmoonnumber])

    avail_historic_end_newmoonnumbers <- global$initial_evaluation_tab_available_historic_end_newmoonnumbers[global$initial_evaluation_tab_available_historic_end_newmoonnumbers %in% possible]

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

    possible <- unique(global$casts_metadata$species[global$casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                                     global$casts_metadata$model                      == rv$forecast_tab_model &
                                                     global$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_species <- global$initial_forecast_tab_available_species[global$initial_forecast_tab_available_species %in% possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(global$casts_evaluations$species[global$casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                        global$casts_evaluations$model                      == rv$evaluation_tab_model &
                                                        global$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                        global$casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber])

    avail_species <- global$initial_evaluation_tab_available_species[global$initial_evaluation_tab_available_species %in% possible]

  } 


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

    possible <- unique(global$casts_metadata$dataset[global$casts_metadata$species                    == rv$forecast_tab_species &
                                                     global$casts_metadata$model                      == rv$forecast_tab_model &
                                                     global$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_datasets <- global$initial_forecast_tab_available_datasets[global$initial_forecast_tab_available_datasets %in% possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(global$casts_evaluations$dataset[global$casts_evaluations$species                    == rv$evaluation_tab_species &
                                                        global$casts_evaluations$model                      == rv$evaluation_tab_model &
                                                        global$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                        global$casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber])

    avail_datasets <- global$initial_evaluation_tab_available_datasets[global$initial_evaluation_tab_available_datasets %in% possible]
  }

  avail_datasets

}


#' @rdname web-app-helpers
#'
#' @export
#'
selected_dataset <- function (global     = global_list( ),
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

    possible <- unique(global$casts_metadata$model[global$casts_metadata$species                    == rv$forecast_tab_species &
                                                   global$casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                                   global$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_models <- global$initial_forecast_tab_available_models[global$initial_forecast_tab_available_models %in% possible]

  } else if (grepl("evaluation_tab_", event_name)) {

    possible <- unique(global$casts_evaluations$model[global$casts_evaluations$species                    == rv$evaluation_tab_species &
                                                      global$casts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                      global$casts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                      global$casts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber ])

    avail_models <- global$initial_evaluation_tab_available_models[global$initial_evaluation_tab_available_models %in% possible]

  }

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



#' @rdname web-app
#'
#' @export
#
global_list <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  messageq("Reading in casts metadata and evaluation files ...", quiet = settings$quiet)

  casts_metadata    <- read_casts_metadata(main = main)
  casts_evaluations <- read_casts_evaluations(main = main)
  casts_evaluations <- casts_evaluations[!is.na(casts_evaluations$obs), ]

  if (nrow(casts_metadata) == 0) {

    messageq("  No casts metadata available.", quiet = FALSE)
    casts_metadata <- NULL

  }

  messageq(" ... done.", quiet = settings$quiet)


  messageq("Locating species and model names ... ", quiet = settings$quiet)

  species_print_names  <- rodent_species(path = file.path(main, settings$subdirectories$resources), set = "forecasting", type = "Latin", total = TRUE)
  species_print_names[species_print_names == "total"] <- "Total abundance"
  species_code_names   <- rodent_species(path = file.path(main, settings$subdirectories$resources), set = "forecasting", type = "code", total = TRUE)
  species_names        <- species_code_names
  names(species_names) <- species_print_names

  model_print_names    <- unlist(mapply(getElement, model_controls(main = main), "metadata")["print_name", ])
  model_code_names     <- unlist(mapply(getElement, model_controls(main = main), "metadata")["name", ])
  model_names          <- model_code_names
  names(model_names)   <- model_print_names

  messageq(" ... done.", quiet = settings$quiet)
  
  messageq("Determining initial available values ...", quiet = settings$quiet)

  initial_forecast_tab_available_species                       <- species_names[species_names %in% unique(casts_metadata$species[casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])]
  initial_evaluation_tab_available_species                     <- species_names[species_names %in% unique(casts_evaluations$species[casts_evaluations$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])]

  initial_forecast_tab_available_models                        <- model_names[model_names %in% unique(casts_metadata$model[casts_metadata$model %in% prefab_models( ) & casts_metadata$species %in% initial_forecast_tab_available_species])]
  initial_evaluation_tab_available_models                      <- model_names[model_names %in% unique(casts_evaluations$model[casts_evaluations$model %in% prefab_models( ) & casts_evaluations$species %in% initial_evaluation_tab_available_species])]

  initial_forecast_tab_available_datasets                      <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( ) & casts_metadata$species %in% initial_forecast_tab_available_species & casts_metadata$model %in% initial_forecast_tab_available_models])
  initial_evaluation_tab_available_datasets                    <- unique(casts_evaluations$dataset[casts_evaluations$dataset %in% prefab_datasets( ) & casts_evaluations$species %in% initial_evaluation_tab_available_species & casts_evaluations$model %in% initial_evaluation_tab_available_models])

  initial_forecast_tab_available_historic_end_newmoonnumbers   <- unique(casts_metadata$historic_end_newmoonnumber[casts_metadata$species %in% initial_forecast_tab_available_species & casts_metadata$model %in% initial_forecast_tab_available_models & casts_metadata$dataset %in% initial_forecast_tab_available_datasets])
  initial_evaluation_tab_available_historic_end_newmoonnumbers <- unique(casts_evaluations$historic_end_newmoonnumber[casts_evaluations$dataset %in% initial_evaluation_tab_available_datasets & casts_evaluations$species %in% initial_evaluation_tab_available_species & casts_evaluations$model %in% initial_evaluation_tab_available_models])

  initial_evaluation_tab_available_newmoonnumbers              <- unique(casts_evaluations$newmoonnumber[casts_evaluations$dataset %in% initial_evaluation_tab_available_datasets & casts_evaluations$species %in% initial_evaluation_tab_available_species & casts_evaluations$model %in% initial_evaluation_tab_available_models & casts_evaluations$historic_end_newmoonnumber %in% initial_evaluation_tab_available_historic_end_newmoonnumbers])

  messageq(" ... done.", quiet = settings$quiet)

  messageq("Selecting intial values ...", quiet = settings$quiet)

  initial_forecast_tab_selected_model                         <- model_names[model_names == "AutoArima"]
  initial_evaluation_tab_selected_model                       <- model_names[model_names == "AutoArima"]

  initial_forecast_tab_selected_species                       <- species_names[species_names == "DM"]
  initial_evaluation_tab_selected_species                     <- species_names[species_names == "DM"]

  initial_forecast_tab_selected_dataset                       <- "controls"
  initial_evaluation_tab_selected_dataset                     <- "controls"

  initial_forecast_tab_selected_historic_end_newmoonnumber    <- max(c(0, initial_forecast_tab_available_historic_end_newmoonnumbers))
  initial_evaluation_tab_selected_historic_end_newmoonnumber  <- max(c(0, initial_evaluation_tab_available_historic_end_newmoonnumbers))

  initial_evaluation_tab_selected_newmoonnumber               <- max(c(0, initial_evaluation_tab_available_newmoonnumbers))

  messageq(" ... done.", quiet = settings$quiet)


  list(casts_metadata                                                   = casts_metadata,
       casts_evaluations                                                = casts_evaluations,

       initial_forecast_tab_available_species                           = initial_forecast_tab_available_species,
       initial_evaluation_tab_available_species                         = initial_evaluation_tab_available_species,

       initial_forecast_tab_available_models                            = initial_forecast_tab_available_models,
       initial_evaluation_tab_available_models                          = initial_evaluation_tab_available_models,
       initial_forecast_tab_available_datasets                          = initial_forecast_tab_available_datasets,
       initial_evaluation_tab_available_datasets                        = initial_evaluation_tab_available_datasets,

       initial_forecast_tab_available_historic_end_newmoonnumbers       = initial_forecast_tab_available_historic_end_newmoonnumbers,
       initial_evaluation_tab_available_historic_end_newmoonnumbers     = initial_evaluation_tab_available_historic_end_newmoonnumbers,

       initial_evaluation_tab_available_newmoonnumbers                  = initial_evaluation_tab_available_newmoonnumbers,

       initial_forecast_tab_selected_model                              = initial_forecast_tab_selected_model,
       initial_evaluation_tab_selected_model                            = initial_evaluation_tab_selected_model,

       initial_forecast_tab_selected_species                            = initial_forecast_tab_selected_species,
       initial_evaluation_tab_selected_species                          = initial_evaluation_tab_selected_species,

       initial_forecast_tab_selected_dataset                            = initial_forecast_tab_selected_dataset,
       initial_evaluation_tab_selected_dataset                          = initial_evaluation_tab_selected_dataset,

       initial_forecast_tab_selected_historic_end_newmoonnumber         = initial_forecast_tab_selected_historic_end_newmoonnumber,
       initial_evaluation_tab_selected_historic_end_newmoonnumber       = initial_evaluation_tab_selected_historic_end_newmoonnumber,

       initial_evaluation_tab_selected_newmoonnumber                    = initial_evaluation_tab_selected_newmoonnumber,

       species_names                                                    = species_names,
       model_names                                                      = model_names)

}



