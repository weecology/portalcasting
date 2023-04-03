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



available_historic_end_newmoonnumbers <- function (main       = ".",
                                                   event_name, 
                                                   rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    casts_metadata <- read_casts_metadata(main = main)

    avail_historic_end_newmoonnumbers  <- unique(casts_metadata$historic_end_newmoonnumber)

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(rv$casts_metadata$historic_end_newmoonnumber)

    possible <- unique(rv$casts_metadata$historic_end_newmoonnumber[rv$casts_metadata$species == rv$forecast_tab_species &
                                                                    rv$casts_metadata$model   == rv$forecast_tab_model &
                                                                    rv$casts_metadata$dataset == rv$forecast_tab_dataset ])

    avail_historic_end_newmoonnumbers <- possible[possible %in% all_possible]

  }

  avail_historic_end_newmoonnumbers

}


selected_historic_end_newmoonnumber <- function (main       = ".",
                                                 event_name,
                                                 default    = NULL,
                                                 rv         = NULL) {

  available <- available_historic_end_newmoonnumbers(main       = main,
                                                     event_name = event_name,
                                                     rv         = rv)

  if (event_name == "initial_forecast_tab") {

    default                           <- ifnull(default, max(available))
    select_historic_end_newmoonnumber <- default

  } else if (grepl("forecast_tab_", event_name)) {

    select_historic_end_newmoonnumber   <- rv$forecast_tab_historic_end_newmoonnumber

  }

  if (!(select_historic_end_newmoonnumber %in% available)) {

    select_historic_end_newmoonnumber <- available[1]

  }  

  available[available == select_historic_end_newmoonnumber]

}






available_species <- function (main       = ".",
                               event_name, 
                               rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    casts_metadata <- read_casts_metadata(main = main)

    avail_species  <- unique(casts_metadata$species)

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(rv$casts_metadata$species[rv$casts_metadata$species %in% rodent_species(set = "forecasting", type = "code", total = TRUE)])

    possible <- unique(rv$casts_metadata$species[rv$casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                                 rv$casts_metadata$model                      == rv$forecast_tab_model &
                                                 rv$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_species <- possible[possible %in% all_possible]

  }

  latin_names <- rodent_species(set = "forecasting", type = "Latin", total = TRUE)
  code_names  <- rodent_species(set = "forecasting", type = "code", total = TRUE)

  names(avail_species) <- latin_names[match(avail_species, code_names)]

  avail_species

}



selected_species <- function (main       = ".",
                              event_name,
                              default    = "DM",
                              rv         = NULL) {

  available <- available_species(main       = main,
                                 event_name = event_name,
                                 rv         = rv)

  if (event_name == "initial_forecast_tab") {

    select_species <- default

  } else if (grepl("forecast_tab_", event_name)) {

    select_species   <- rv$forecast_tab_species

  }

  if (!(select_species %in% available)) {

    select_species <- available[1]

  }

  available[available == select_species]

}





available_datasets <- function (main       = ".",
                                event_name, 
                                rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    casts_metadata <- read_casts_metadata(main = main)

    avail_datasets  <- unique(casts_metadata$dataset[casts_metadata$dataset %in% prefab_datasets( )])

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(rv$casts_metadata$dataset[rv$casts_metadata$dataset %in% prefab_datasets( )])

    possible <- unique(rv$casts_metadata$dataset[rv$casts_metadata$species                    == rv$forecast_tab_species &
                                                 rv$casts_metadata$model                      == rv$forecast_tab_model &
                                                 rv$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_datasets <- possible[possible %in% all_possible]
  }

  avail_datasets

}

selected_dataset <- function (main       = ".",
                              event_name,
                              default    = "controls",
                              rv         = NULL) {

  available <- available_datasets(main       = main,
                                  event_name = event_name,
                                  rv         = rv)

  if (event_name == "initial_forecast_tab") {

    select_dataset <- default

  } else if (grepl("forecast_tab_", event_name)) {

    select_dataset   <- rv$forecast_tab_dataset

  }

  if (!(select_dataset %in% available)) {

    select_dataset <- available[1]

  }  

  available[available == select_dataset]

}





available_models <- function (main       = ".",
                              event_name, 
                              rv         = NULL) {

  if (event_name == "initial_forecast_tab") {

    casts_metadata <- read_casts_metadata(main = main)

    avail_models  <- unique(casts_metadata$model[casts_metadata$model %in% prefab_models( )])

  } else if (grepl("forecast_tab_", event_name)) {

    all_possible <- unique(rv$casts_metadata$model[rv$casts_metadata$model %in% prefab_models( )])

    possible <- unique(rv$casts_metadata$model[rv$casts_metadata$species                    == rv$forecast_tab_species &
                                               rv$casts_metadata$dataset                    == rv$forecast_tab_dataset &
                                               rv$casts_metadata$historic_end_newmoonnumber == rv$forecast_tab_historic_end_newmoonnumber])

    avail_models <- possible[possible %in% all_possible]
  }

  print_name <- unlist(mapply(getElement, model_controls(main = main), "metadata")["print_name", ])
  model_name <- unlist(mapply(getElement, model_controls(main = main), "metadata")["name", ])

  names(avail_models) <- print_name[match(avail_models, model_name)]

  avail_models

}

selected_model <- function (main       = ".",
                            event_name,
                            default    = "AutoArima",
                            rv         = NULL) {

  available <- available_models(main       = main,
                                  event_name = event_name,
                                  rv         = rv)

  if (event_name == "initial_forecast_tab") {

    select_model <- default

  } else if (grepl("forecast_tab_", event_name)) {

    select_model   <- rv$forecast_tab_model

  }

  if (!(select_model %in% available)) {

    select_model <- available[1]

  }  

  available[available == select_model]

}




