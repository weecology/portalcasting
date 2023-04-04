
#' @title Generate the User Interface for the Web App
#' 
#' @description `portal_forecast_ui` constructs the user interface (UI) for the web application by updating the static pages (models and rodent profiles) then running [`fluidPage`][shiny::fluidPage] on the UI components. \cr \cr
#'              See `Details` for hierarchy of functions. 
#'
#' @details The UI is hierarchical built as:
#'   * portal_forecast_ui 
#'     * page_title_panel
#'     * page_subtitle_panel
#'     * page_main_panel
#'       * forecast_tab
#'         * forecast_tab_input_selection_row
#'           * forecast_tab_input_selection_row_species
#'           * forecast_tab_input_selection_row_dataset
#'           * forecast_tab_input_selection_row_model
#'           * forecast_tab_input_selection_row_historic_end_newmoonnumber
#'         * forecast_tab_input_selection_checks_row  # commented out, but available for checking reactive inputs in dev 
#'         * plot_cast_ts
#'         * plot_cast_point
#'       * evaluation_tab
#'         * evaluation_tab_input_selection_row
#'           * evaluation_tab_input_selection_row_species
#'           * evaluation_tab_input_selection_row_dataset
#'           * evaluation_tab_input_selection_row_model
#'           * evaluation_tab_input_selection_row_historic_end_newmoonnumber
#'           * evaluation_tab_input_selection_row_newmoonnumber
#'         * evaluation_tab_input_selection_checks_row  # commented out, but available for checking reactive inputs in dev 
#'         * plot_cast_point
#'         * plot_casts_cov_RMSE  
#'       * about_tab
#'         * htmltools::includeMarkdown
#'       * models_tab
#'         * htmltools::includeHTML
#'       * profiles_tab
#'         * htmltools::includeHTML
#'
#' @return A UI definition or component shiny tags.
#'
#' @name web app ui
#'
NULL



#' @rdname web-app-ui
#'
#' @export
#'
portal_forecast_ui <- function ( ) {

  fluidPage(page_title_panel( ),
            page_subtitle_panel( ), 
            page_main_panel( ))


}


#' @rdname web-app-ui
#'
#' @export
#
page_main_panel <- function ( ) {

  mainPanel(tabsetPanel(forecast_tab( ),
                        evaluation_tab( ),
                        about_tab( ),
                        models_tab( ),
                        profiles_tab( )))


}

#' @rdname web-app-ui
#'
#' @export
#'
page_title_panel <- function ( ) {

  app_title <- "Portal Project Forecasting"
  titlePanel(title = app_title)

}

#' @rdname web-app-ui
#'
#' @export
#'
page_subtitle_panel <- function ( ) {

  href <- a(href   = "http://portal.weecology.org", 
                     "The Portal Project", 
            target = "_blank")

  p(HTML(text = paste0("Forecasts for the population and community dynamics of ", href, ".")))

}

#' @rdname web-app-ui
#'
#' @export
#'
about_tab <- function ( ) {

  about_md <- system.file(...     = "app", 
                          ...     = "about.md",
                          package = "portalcasting")

  tabPanel(title = "About",
           includeMarkdown(about_md)) 

}

#' @rdname web-app-ui
#'
#' @export
#'
models_tab <- function ( ) {

  models_html <- system.file(...     = "app", 
                             ...     = "models.html",
                             package = "portalcasting")

  tabPanel(title = "Models",
           includeHTML(models_html)) 

}

#' @rdname web-app-ui
#'
#' @export
#'
profiles_tab <- function ( ) {

  profile_html <- system.file(...     = "app", 
                              ...     = "profile.html",
                              package = "portalcasting")

  tabPanel(title = "Rodent Profiles",
           includeHTML(profile_html)) 

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab <- function ( ) {


  tabPanel(title = "Forecast", 
           br( ), 
           forecast_tab_input_selection_row( ), 
      #    forecast_tab_input_selection_checks_row( ), # used for checking reactive inputs in dev
           plotOutput("forecast_tab_ts_plot"), 
           br( ),
           plotOutput("forecast_tab_ss_plot"),
           br( ))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_checks_row <- function ( ) {

  fluidRow(textOutput("forecast_tab_species"), 
           textOutput("forecast_tab_dataset"), 
           textOutput("forecast_tab_model"), 
           textOutput("forecast_tab_historic_end_newmoonnumber"))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row <- function ( ) {

  fluidRow(forecast_tab_input_selection_row_species( ),
           forecast_tab_input_selection_row_dataset( ),
           forecast_tab_input_selection_row_model( ),
           forecast_tab_input_selection_row_historic_end_newmoonnumber( ))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_species <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_species",
                     label    = "Species",
                     choices  = initial_forecast_tab_available_species,
                     selected = initial_forecast_tab_selected_species))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_dataset <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_dataset",
                     label    = "Dataset",
                     choices  = initial_forecast_tab_available_datasets,
                     selected = initial_forecast_tab_selected_dataset))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_model <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_model",
                     label    = "Model",
                     choices  = initial_forecast_tab_available_models,
                     selected = initial_forecast_tab_selected_model))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_historic_end_newmoonnumber <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_historic_end_newmoonnumber",
                     label    = "Origin Newmoon",
                     choices  = initial_forecast_tab_available_historic_end_newmoonnumbers,
                     selected = initial_forecast_tab_selected_historic_end_newmoonnumber))

}




#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab <- function ( ) {


  tabPanel(title = "Evaluation", 
           br( ), 
           evaluation_tab_input_selection_row( ), 
           h2("Recent Observations vs. Forecasts"),
       #   evaluation_tab_input_selection_checks_row( ),   # used for checking reactive inputs in dev
           plotOutput("evaluation_tab_sp_plot"),
           br( ),
           h2("Model Coverage & RMSE"),
           plotOutput("evaluation_tab_RMSE_plot"),
           br( ))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_checks_row <- function ( ) {

  fluidRow(textOutput("evaluation_tab_species"), 
           textOutput("evaluation_tab_dataset"), 
           textOutput("evaluation_tab_model"), 
           textOutput("evaluation_tab_historic_end_newmoonnumber"), 
           textOutput("evaluation_tab_newmoonnumber"))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row <- function ( ) {


  fluidRow(evaluation_tab_input_selection_row_species( ),
           evaluation_tab_input_selection_row_dataset( ),
           evaluation_tab_input_selection_row_model( ),
           evaluation_tab_input_selection_row_historic_end_newmoonnumber( ),
           evaluation_tab_input_selection_row_newmoonnumber( ))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_species <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_species",
                     label    = "Species",
                     choices  = initial_evaluation_tab_available_species,
                     selected = initial_evaluation_tab_selected_species))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_dataset <- function ( ) {

  column(width = 2,
         selectInput(inputId  = "evaluation_tab_dataset",
                     label    = "Dataset",
                     choices  = initial_evaluation_tab_available_datasets,
                     selected = initial_evaluation_tab_selected_dataset))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_model <- function ( ) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_model",
                     label    = "Model",
                     choices  = initial_evaluation_tab_available_models,
                     selected = initial_evaluation_tab_selected_model))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_historic_end_newmoonnumber <- function ( ) {

  column(width = 2,
         selectInput(inputId  = "evaluation_tab_historic_end_newmoonnumber",
                     label    = "Origin Newmoon",
                     choices  = initial_evaluation_tab_available_historic_end_newmoonnumbers,
                     selected = initial_evaluation_tab_selected_historic_end_newmoonnumber))

}

#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_newmoonnumber <- function ( ) {

  column(width = 2,
         selectInput(inputId  = "evaluation_tab_newmoonnumber",
                     label    = "Target Newmoon",
                     choices  = initial_evaluation_tab_available_newmoonnumbers,
                     selected = initial_evaluation_tab_selected_newmoonnumber))

}
