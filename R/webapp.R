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


#' @title Utilities for the Web App
#' 
#' @description Helper functions
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @return
#'   `historic_end_newmoonnumber_list`: `vector` of `integer` newmoonnumbers. \cr \cr
#'   `model_list`: `vector` of `character` models named with print names from controls list. \cr \cr
#'   `species_list`: `vector` of `character` species codes named with their Latin names. 
#'
#' @name web app utilities
#'
NULL






#' @rdname web-app-utilities
#'
#' @export
#'
historic_end_newmoonnumber_list <- function (main = ".") {

  unique(read_casts_metadata(main = main)$historic_end_newmoonnumber)

}

#' @rdname web-app-utilities
#'
#' @export
#'
model_list <- function ( ) {

  out <- prefab_models()
  names(out) <- unlist(mapply(getElement, prefab_model_controls(), "metadata")["print_name", ])

  out
}

#' @rdname web-app-utilities
#'
#' @export
#'
species_list <- function ( ) {

  out <- rodent_species(set = "forecasting", type = "code", total = TRUE)
  names(out) <- rodent_species(set = "forecasting", type = "Latin", total = TRUE)

  out
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






