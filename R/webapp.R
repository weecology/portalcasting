#' @title Build and Launch the Portal Forecast Web Application
#' 
#' @description `run_app` constructs and launches a local version of the web application by running [`shiny::runApp`] pointed to the `app` subdirectory in the local `portalcasting` package folder. \cr \cr
#'              `global_list` creates a list of values that are globally available in an app run.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @return `global_list`: a `list` of values that will be globally available in the app. \cr
#'         `run_app` runs the app itself.
#'
#' @name portalcasting app
#'
#' @family shinyapp
#'
#' @aliases web-app app
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "app")
#'    setup_dir(main = main1)
#'
#'    global_list(main = main1)
#'
#'    if (getShinyOption("shiny.launch.browser", FALSE)) {
#'      run_app(main = main1)
#'    }
#'
#'    unlink(main1, recursive = TRUE)
#'  }
#'
NULL

#' @rdname portalcasting-app
#'
#' @export
#'
run_app <- function(main = ".") {

  runApp(appDir = shiny_app_dir(main = main))

}

#' @rdname portalcasting-app
#'
#' @export
#
global_list <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  messageq("Reading in forecasts metadata and evaluation files ...", quiet = !settings$verbose)

  forecasts_metadata    <- read_forecasts_metadata(main = main)
  forecasts_evaluations <- read_forecasts_evaluations(main = main)
  if (!is.null(forecasts_evaluations) && nrow(forecasts_evaluations) > 0) {
    forecasts_evaluations <- forecasts_evaluations[!is.na(forecasts_evaluations$obs), ]
  }
  if (nrow(forecasts_metadata) == 0) {

    messageq("  No forecasts metadata available.", quiet = !settings$verbose)
    forecasts_metadata <- NULL

  } else {

    forecasts_evaluations$historic_end_newmoonnumber <- forecasts_metadata$historic_end_newmoonnumber[match(forecasts_evaluations$forecast_id, forecasts_metadata$forecast_id)]
    forecasts_evaluations$model                      <- forecasts_metadata$model[match(forecasts_evaluations$forecast_id, forecasts_metadata$forecast_id)]
    forecasts_evaluations$dataset                    <- forecasts_metadata$dataset[match(forecasts_evaluations$forecast_id, forecasts_metadata$forecast_id)]
    forecasts_evaluations$species                    <- forecasts_metadata$species[match(forecasts_evaluations$forecast_id, forecasts_metadata$forecast_id)]
  
  }

  messageq(" ... done.", quiet = !settings$verbose)

  messageq("Reading in covariates ...", quiet = !settings$verbose)

  covariates <- tryCatch(read_covariates(main = main),
                         error = function (x) {NULL})

  messageq(" ... done.", quiet = !settings$verbose)


  messageq("Locating species and model names ... ", quiet = !settings$verbose)

  species_names_table  <- rodent_species(path = file.path(main, settings$subdirectories$resources), set = "forecasting", type = "table", total = TRUE)
  species_names        <- species_names_table$code
  names(species_names) <- species_names_table$Latin

  models_print_names    <- unlist(mapply(getElement, models_controls(main = main), "metadata")["print_name", ])
  models_code_names     <- unlist(mapply(getElement, models_controls(main = main), "metadata")["name", ])
  models_names          <- models_code_names
  names(models_names)   <- models_print_names

  messageq(" ... done.", quiet = !settings$verbose)
  
  messageq("Determining initial available values ...", quiet = !settings$verbose)

  initial_forecast_tab_available_species                       <- species_names[species_names %in% unique(forecasts_metadata$species[forecasts_metadata$species %in% species_names_table$code])]
  initial_evaluation_tab_available_species                     <- species_names[species_names %in% unique(forecasts_evaluations$species[forecasts_evaluations$species %in% species_names_table$code])]

  initial_forecast_tab_available_models                        <- models_names[models_names %in% unique(forecasts_metadata$model[forecasts_metadata$model %in% prefab_models( ) & forecasts_metadata$species %in% initial_forecast_tab_available_species])]
  initial_evaluation_tab_available_models                      <- models_names[models_names %in% unique(forecasts_evaluations$model[forecasts_evaluations$model %in% prefab_models( ) & forecasts_evaluations$species %in% initial_evaluation_tab_available_species])]

  initial_forecast_tab_available_datasets                      <- unique(forecasts_metadata$dataset[forecasts_metadata$dataset %in% prefab_datasets( ) & forecasts_metadata$species %in% initial_forecast_tab_available_species & forecasts_metadata$model %in% initial_forecast_tab_available_models])
  initial_evaluation_tab_available_datasets                    <- unique(forecasts_evaluations$dataset[forecasts_evaluations$dataset %in% prefab_datasets( ) & forecasts_evaluations$species %in% initial_evaluation_tab_available_species & forecasts_evaluations$model %in% initial_evaluation_tab_available_models])

  initial_forecast_tab_available_historic_end_newmoonnumbers   <- unique(forecasts_metadata$historic_end_newmoonnumber[forecasts_metadata$species %in% initial_forecast_tab_available_species & forecasts_metadata$model %in% initial_forecast_tab_available_models & forecasts_metadata$dataset %in% initial_forecast_tab_available_datasets])
  initial_evaluation_tab_available_historic_end_newmoonnumbers <- unique(forecasts_evaluations$historic_end_newmoonnumber[forecasts_evaluations$dataset %in% initial_evaluation_tab_available_datasets & forecasts_evaluations$species %in% initial_evaluation_tab_available_species & forecasts_evaluations$model %in% initial_evaluation_tab_available_models])

  initial_evaluation_tab_available_newmoonnumbers              <- unique(forecasts_evaluations$newmoonnumber[forecasts_evaluations$dataset %in% initial_evaluation_tab_available_datasets & forecasts_evaluations$species %in% initial_evaluation_tab_available_species & forecasts_evaluations$model %in% initial_evaluation_tab_available_models & forecasts_evaluations$historic_end_newmoonnumber %in% initial_evaluation_tab_available_historic_end_newmoonnumbers])

  messageq(" ... done.", quiet = !settings$verbose)

  messageq("Selecting intial values ...", quiet = !settings$verbose)

  initial_forecast_tab_selected_model                         <- models_names[models_names == "AutoArima"]
  initial_evaluation_tab_selected_model                       <- models_names[models_names == "AutoArima"]

  initial_forecast_tab_selected_species                       <- species_names[species_names == "DM"]
  initial_evaluation_tab_selected_species                     <- species_names[species_names == "DM"]

  initial_forecast_tab_selected_dataset                       <- "controls"
  initial_evaluation_tab_selected_dataset                     <- "controls"

  initial_forecast_tab_selected_historic_end_newmoonnumber    <- max(c(0, initial_forecast_tab_available_historic_end_newmoonnumbers))
  initial_evaluation_tab_selected_historic_end_newmoonnumber  <- max(c(0, initial_evaluation_tab_available_historic_end_newmoonnumbers))

  initial_evaluation_tab_selected_newmoonnumber               <- max(c(0, initial_evaluation_tab_available_newmoonnumbers))

  messageq(" ... done.", quiet = !settings$verbose)

  about_md_path                                               <- about_md_path(main = main)
  models_html_path                                            <- models_html_path(main = main)
  rodents_profiles_html_path                                  <- rodents_profiles_html_path(main = main)

  list(forecasts_metadata                                               = forecasts_metadata,
       forecasts_evaluations                                            = forecasts_evaluations,

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

       covariates                                                       = covariates,

       species_names                                                    = species_names,
       species_names_table                                              = species_names_table,
       models_names                                                     = models_names,

       about_md_path                                                    = about_md_path,
       models_html_path                                                 = models_html_path,
       rodents_profiles_html_path                                       = rodents_profiles_html_path)

}

#' @title Selection Helper Functions for the Portalcasting App
#' 
#' @description Construct vectors of available choices and make selections, with respect to species, dataset, model, and moons.
#'
#' @param event `character` value of the server event. \cr
#'        Options include `"initial_forecast_tab"`, `"initial_evaluation_tab"`, `"forecast_tab_species"`, `"forecast_tab_dataset"`, `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`, `"evaluation_tab_species"`, `"evaluation_tab_dataset"`, `"evaluation_tab_model"`, `"evaluation_tab_historic_end_newmoonnumber"`, and `"evaluation_tab_newmoonnumber"`)
#'
#' @param rv [`reactiveValues`][shiny::reactiveValues] `list` for the UI.
#'
#' @param global A `list` of global values for the app.
#'
#' @return `character` or `integer`, depending upon the function.
#'
#' @name portalcasting app selection helpers
#'
#' @family shinyapp
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "app_helpers")
#'    setup_dir(main = main1)
#'
#'    gl <- global_list(main = main1)
#'
#'    ft_species                    <- gl$initial_forecast_tab_selected_species
#'    ft_dataset                    <- gl$initial_forecast_tab_selected_species
#'    ft_model                      <- gl$initial_forecast_tab_selected_species
#'    ft_historic_end_newmoonnumber <- gl$initial_forecast_tab_selected_historic_end_newmoonnumber
#'    et_species                    <- gl$initial_forecast_tab_selected_species
#'    et_dataset                    <- gl$initial_forecast_tab_selected_species
#'    et_model                      <- gl$initial_forecast_tab_selected_species
#'    et_historic_end_newmoonnumber <- gl$initial_forecast_tab_selected_historic_end_newmoonnumber
#'    et_newmoonnumber              <- gl$initial_forecast_tab_selected_newmoonnumber
#'
#'    rv <- list(forecast_tab_species                      = ft_species,
#'               forecast_tab_dataset                      = ft_dataset,
#'               forecast_tab_model                        = ft_model,
#'               forecast_tab_historic_end_newmoonnumber   = ft_historic_end_newmoonnumber,
#'               evaluation_tab_species                    = et_dataset,
#'               evaluation_tab_dataset                    = et_species,
#'               evaluation_tab_model                      = et_model,
#'               evaluation_tab_historic_end_newmoonnumber = et_historic_end_newmoonnumber,
#'               evaluation_tab_newmoonnumber              = et_newmoonnumber)
#'
#'    available_models(global = gl, 
#'                     event  = "initial_forecast_tab")
#'    selected_model(global   = gl, 
#'                   event    = "initial_forecast_tab", 
#'                   rv         = rv1)
#'
#'    available_datasets(global = gl, 
#'                       event  = "initial_forecast_tab")
#'    selected_dataset(global   = gl, 
#'                     event    = "initial_forecast_tab", 
#'                     rv       = rv1)
#'
#'    available_species(global  = gl, 
#'                      event   = "initial_forecast_tab")
#'    selected_species(global   = gl, 
#'                     event    = "initial_forecast_tab", 
#'                     rv       = rv1)
#'
#'    available_historic_end_newmoonnumbers(global = gl, 
#'                                          event  = "initial_forecast_tab")
#'    selected_historic_end_newmoonnumber(global   = gl, 
#'                                        event    = "initial_forecast_tab", 
#'                                        rv       = rv1)
#'
#'    available_newmoonnumbers(global = gl, 
#'                             event  = "initial_evaluation_tab")
#'    selected_newmoonnumber(global   = gl, 
#'                           event    = "initial_evaluation_tab", 
#'                           rv       = rv1)
#'
#'    unlink(main1, recursive = TRUE)
#'  }
#'
NULL


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
available_newmoonnumbers <- function (global = global_list( ),
                                      event  = "initial_evaluation_tab", 
                                      rv     = NULL) {

  if (event == "initial_evaluation_tab") {

    available <- global$initial_evaluation_tab_available_newmoonnumbers

  } else if (grepl("evaluation_tab_", event)) {

    possible  <- unique(global$forecasts_evaluations$newmoonnumber[global$forecasts_evaluations$species                    == rv$evaluation_tab_species &
                                                                   global$forecasts_evaluations$model                      == rv$evaluation_tab_model &
                                                                   global$forecasts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                                   global$forecasts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber])

    available <- global$initial_evaluation_tab_available_newmoonnumbers[global$initial_evaluation_tab_available_newmoonnumbers %in% possible]

  } else {

    available <- NULL

  }

  available 

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
selected_newmoonnumber <- function (global = global_list( ),
                                    event  = "initial_evaluation_tab", 
                                    rv     = NULL) {

  available <- available_newmoonnumbers(global = global,
                                        event  = event,
                                        rv     = rv)

  if (grepl("evaluation_tab_", event)) {

    selected <- rv$evaluation_tab_newmoonnumber

  } else {

    selected <- NULL

  }

  if (!is.null(selected) && !(selected %in% available)) {

    selected <- available[1]

  }  

  available[available == selected]

}



#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
available_historic_end_newmoonnumbers <- function (global = global_list( ),
                                                   event  = "initial_forecast_tab", 
                                                   rv     = NULL) {

  if (event == "initial_forecast_tab") {

    available <- global$initial_forecast_tab_available_historic_end_newmoonnumbers

  } else if (event == "initial_evaluation_tab") {

    available <- global$initial_evaluation_tab_available_historic_end_newmoonnumbers

  } else if (grepl("forecast_tab_", event)) {

    possible  <- unique(global$forecasts_metadata$historic_end_newmoonnumber[global$forecasts_metadata$species == rv$forecast_tab_species &
                                                                             global$forecasts_metadata$model   == rv$forecast_tab_model &
                                                                             global$forecasts_metadata$dataset == rv$forecast_tab_dataset])

    available <- global$initial_forecast_tab_available_historic_end_newmoonnumbers[global$initial_forecast_tab_available_historic_end_newmoonnumbers %in% possible]

  } else if (grepl("evaluation_tab_", event)) {

    possible  <- unique(global$forecasts_evaluations$historic_end_newmoonnumber[global$forecasts_evaluations$species       == rv$evaluation_tab_species &
                                                                                global$forecasts_evaluations$model         == rv$evaluation_tab_model &
                                                                                global$forecasts_evaluations$dataset       == rv$evaluation_tab_dataset & 
                                                                                global$forecasts_evaluations$newmoonnumber == rv$evaluation_tab_newmoonnumber])

    available <- global$initial_evaluation_tab_available_historic_end_newmoonnumbers[global$initial_evaluation_tab_available_historic_end_newmoonnumbers %in% possible]

  } else {

    available <- NULL

  }


  available

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
selected_historic_end_newmoonnumber <- function (global = global_list( ),
                                                 event  = "initial_forecast_tab",
                                                 rv     = NULL) {

  available <- available_historic_end_newmoonnumbers(global = global,
                                                     event  = event,
                                                     rv     = rv)

  if (grepl("forecast_tab_", event)) {

    selected <- rv$forecast_tab_historic_end_newmoonnumber

  } else if (grepl("evaluation_tab_", event)) {

    selected <- rv$evaluation_tab_historic_end_newmoonnumber

  } else {

    selected <- NULL

  }

  if (!is.null(selected) && !(selected %in% available)) {

    selected <- available[1]

  }  

  available[available == selected]

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
available_species <- function (global = global_list( ),
                               event  = "initial_forecast_tab", 
                               rv     = NULL) {

  if (event == "initial_forecast_tab") {

    available <- global$initial_forecast_tab_available_species

  } else if (event == "initial_evaluation_tab") {

    available <- global$initial_evaluation_tab_available_species

  } else if (grepl("forecast_tab_", event)) {

    possible  <- unique(global$forecasts_metadata$species[global$forecasts_metadata$dataset                    == rv$forecast_tab_dataset &
                                                          global$forecasts_metadata$model                      == rv$forecast_tab_model &
                                                          global$forecasts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    available <- global$initial_forecast_tab_available_species[global$initial_forecast_tab_available_species %in% possible]

  } else if (grepl("evaluation_tab_", event)) {

    possible  <- unique(global$forecasts_evaluations$species[global$forecasts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                             global$forecasts_evaluations$model                      == rv$evaluation_tab_model &
                                                             global$forecasts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                             global$forecasts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber])

    available <- global$initial_evaluation_tab_available_species[global$initial_evaluation_tab_available_species %in% possible]

  } else {

    available <- NULL

  }

  available

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
selected_species <- function (global = global_list( ),
                              event  = "initial_forecast_tab",
                              rv     = NULL) {

  available <- available_species(global = global,
                                 event  = event,
                                 rv     = rv)

 if (grepl("forecast_tab_", event)) {

    selected <- rv$forecast_tab_species

  } else if (grepl("evaluation_tab_", event)) {

    selected <- rv$evaluation_tab_species

  } else {

    selected <- NULL

  }
 
  if (!is.null(selected) && !(selected %in% available)) {

    selected <- available[1]

  }

  available[available == selected]

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
available_datasets <- function (global = global_list( ),
                                event  = "initial_forecast_tab", 
                                rv     = NULL) {

  if (event == "initial_forecast_tab") {

    available <- global$initial_forecast_tab_available_datasets

  } else if (event == "initial_evaluation_tab") {

    available <- global$initial_evaluation_tab_available_datasets

  } else if (grepl("forecast_tab_", event)) {

    possible  <- unique(global$forecasts_metadata$dataset[global$forecasts_metadata$species                    == rv$forecast_tab_species &
                                                          global$forecasts_metadata$model                      == rv$forecast_tab_model &
                                                          global$forecasts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    available <- global$initial_forecast_tab_available_datasets[global$initial_forecast_tab_available_datasets %in% possible]

  } else if (grepl("evaluation_tab_", event)) {

    possible <- unique(global$forecasts_evaluations$dataset[global$forecasts_evaluations$species                    == rv$evaluation_tab_species &
                                                            global$forecasts_evaluations$model                      == rv$evaluation_tab_model &
                                                            global$forecasts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                            global$forecasts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber])

    available <- global$initial_evaluation_tab_available_datasets[global$initial_evaluation_tab_available_datasets %in% possible]

  } else {

    available <- NULL

  }

  available

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
selected_dataset <- function (global = global_list( ),
                              event  = "initial_forecast_tab",
                              rv     = NULL) {

  available <- available_datasets(global = global,
                                  event  = event,
                                  rv     = rv)

 if (grepl("forecast_tab_", event)) {

    selected <- rv$forecast_tab_dataset

  } else if (grepl("evaluation_tab_", event)) {

    selected <- rv$evaluation_tab_dataset

  } else {

    selected <- NULL

  }

  if (!is.null(selected) && !(selected %in% available)) {

    selected <- available[1]

  }  

  available[available == selected]

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
available_models <- function (global = global_list( ),
                              event  = "initial_forecast_tab", 
                              rv     = NULL) {

  if (event == "initial_forecast_tab") {

    available <- global$initial_forecast_tab_available_models

  } else if (event == "initial_evaluation_tab") {

    available <- global$initial_evaluation_tab_available_models

  } else if (grepl("forecast_tab_", event)) {

    possible  <- unique(global$forecasts_metadata$model[global$forecasts_metadata$species                    == rv$forecast_tab_species &
                                                        global$forecasts_metadata$dataset                    == rv$forecast_tab_dataset &
                                                        global$forecasts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    available <- global$initial_forecast_tab_available_models[global$initial_forecast_tab_available_models %in% possible]

  } else if (grepl("evaluation_tab_", event)) {

    possible  <- unique(global$forecasts_evaluations$model[global$forecasts_evaluations$species                    == rv$evaluation_tab_species &
                                                           global$forecasts_evaluations$dataset                    == rv$evaluation_tab_dataset &
                                                           global$forecasts_evaluations$historic_end_newmoonnumber == rv$evaluation_tab_historic_end_newmoonnumber & 
                                                           global$forecasts_evaluations$newmoonnumber              == rv$evaluation_tab_newmoonnumber ])

    available <- global$initial_evaluation_tab_available_models[global$initial_evaluation_tab_available_models %in% possible]

  } else {

    available <- NULL

  }

  available

}


#' @rdname portalcasting-app-selection-helpers
#'
#' @export
#'
selected_model <- function (global = global_list( ),
                            event  = "initial_forecast_tab",
                            rv     = NULL) {

  available <- available_models(global = global,
                                event  = event,
                                rv     = rv)

  if (grepl("forecast_tab_", event)) {

    selected <- rv$forecast_tab_model

  } else if (grepl("evaluation_tab_", event)) {

    selected <- rv$evaluation_tab_model

  } else {

    selected <- NULL

  }

  if (!is.null(selected) && !(selected %in% available)) {

    selected <- available[1]

  }  

  available[available == selected]

}






