
#' @title Generate the User Interface for the Web App
#' 
#' @description `portal_forecast_ui` constructs the user interface (UI) for the web application by updating the static pages (models and rodent profiles) then running [`fluidPage`][shiny::fluidPage] on the UI components. \cr \cr
#'              `write_rodent_profiles_tab_html` and `write_model_tab_html` build and write-out static html files for the rodent profiles and models tabs during [`fill_app`]. \cr \cr
#'              The `<_>_href` functions provide simplified calls to hyperlinked texts that are repeatedly used. \cr \cr
#'              See `Details` for hierarchy of functions. 
#'
#' @details The UI is hierarchical built as:
#'   * `portal_forecast_ui` 
#'     * `page_title_panel`
#'     * `page_subtitle_panel`
#'     * `page_main_panel`
#'       * `forecast_tab`
#'         * `forecast_tab_input_selection_row`
#'           * `forecast_tab_input_selection_row_species`
#'           * `forecast_tab_input_selection_row_dataset`
#'           * `forecast_tab_input_selection_row_model`
#'           * `forecast_tab_input_selection_row_historic_end_newmoonnumber`
#'         * `forecast_tab_input_selection_checks_row`  # commented out, but available for checking reactive inputs in dev 
#'         * `plot_cast_ts`
#'         * `plot_cast_point`
#'       * `evaluation_tab`
#'         * `evaluation_tab_input_selection_row`
#'           * `evaluation_tab_input_selection_row_species`
#'           * `evaluation_tab_input_selection_row_dataset`
#'           * `evaluation_tab_input_selection_row_model`
#'           * `evaluation_tab_input_selection_row_historic_end_newmoonnumber`
#'           * `evaluation_tab_input_selection_row_newmoonnumber`
#'         * `evaluation_tab_input_selection_checks_row`  # commented out, but available for checking reactive inputs in dev 
#'         * `plot_cast_point`
#'         * `plot_casts_cov_RMSE`
#'       * `about_tab`
#'         * `htmltools::includeMarkdown`
#'       * `models_tab`
#'         * `htmltools::includeHTML`
#'       * `profiles_tab`
#'         * `htmltools::includeHTML`
#'       * `covariates_tab`
#'         * `data_sources_section`
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param text `character` value of the text used in [`htmltools::a`].
#'
#' @param global A `list` of global values for the app.
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
portal_forecast_ui <- function (global = global_list( )) {

  fluidPage(page_title_panel( ),
            page_subtitle_panel( ), 
            page_main_panel(global = global))


}


#' @rdname web-app-ui
#'
#' @export
#
page_main_panel <- function (global = global_list( )) {

  mainPanel(tabsetPanel(forecast_tab(global = global),
                        evaluation_tab(global = global),
                        about_tab( ),
                        models_tab( ),
                        profiles_tab( ),
                        covariates_tab(global = global)))


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

  p(HTML(text = paste0("Forecasts for the population and community dynamics of ", portal_project_href( ), ".")))

}

#' @rdname web-app-ui
#'
#' @export
#'
about_tab <- function ( ) {

  tabPanel(title = "About",
           includeMarkdown("about.md")) 

}

#' @rdname web-app-ui
#'
#' @export
#'
models_tab <- function ( ) {

  tabPanel(title = "Models",
           includeHTML("models.html")) 

}

#' @rdname web-app-ui
#'
#' @export
#'
profiles_tab <- function ( ) {

  tabPanel(title = "Rodent Profiles",
           includeHTML("profile.html")) 

}

#' @rdname web-app-ui
#'
#' @export
#
forecast_tab <- function (global = global_list( )) {

  if (is.null(global$casts_metadata)) {

    tabPanel(title = "Forecast", 
             br( ), 
             HTML("There are not sufficient forecasts to generate plots."),
             br( ))
  } else {  

    tabPanel(title = "Forecast", 
             br( ), 
             forecast_tab_input_selection_row(global = global), 
        #    forecast_tab_input_selection_checks_row( ), # used for checking reactive inputs in dev
             plotOutput("forecast_tab_ts_plot", height = "300px"), 
             br( ),
             plotOutput("forecast_tab_ss_plot"),
             br( ))
  }

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
forecast_tab_input_selection_row <- function (global = global_list( )) {

  fluidRow(forecast_tab_input_selection_row_species(global = global),
           forecast_tab_input_selection_row_dataset(global = global),
           forecast_tab_input_selection_row_model(global = global),
           forecast_tab_input_selection_row_historic_end_newmoonnumber(global = global))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_species <- function (global = global_list( )) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_species",
                     label    = "Species",
                     choices  = global$initial_forecast_tab_available_species,
                     selected = global$initial_forecast_tab_selected_species))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_dataset <- function (global = global_list( )) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_dataset",
                     label    = "Dataset",
                     choices  = global$initial_forecast_tab_available_datasets,
                     selected = global$initial_forecast_tab_selected_dataset))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_model <- function (global = global_list( )) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_model",
                     label    = "Model",
                     choices  = global$initial_forecast_tab_available_models,
                     selected = global$initial_forecast_tab_selected_model))

}


#' @rdname web-app-ui
#'
#' @export
#
forecast_tab_input_selection_row_historic_end_newmoonnumber <- function (global = global_list( )) {

  column(width = 3,
         selectInput(inputId  = "forecast_tab_historic_end_newmoonnumber",
                     label    = "Origin Newmoon",
                     choices  = global$initial_forecast_tab_available_historic_end_newmoonnumbers,
                     selected = global$initial_forecast_tab_selected_historic_end_newmoonnumber))

}




#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab <- function (global = global_list( )) {

  if (is.null(global$casts_evaluations)) {

    tabPanel(title = "Evaluation", 
             br( ), 
             HTML("There are not sufficient evaluated forecasts to generate plots."),
             br( ))
  } else {  

    tabPanel(title = "Evaluation", 
           br( ), 
           evaluation_tab_input_selection_row(global = global), 
       #   evaluation_tab_input_selection_checks_row( ),   # used for checking reactive inputs in dev
           plotOutput("evaluation_tab_sp_plot"),
           br( ),
           plotOutput("evaluation_tab_RMSE_plot", height = "300px"),
           br( ))

  }

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
evaluation_tab_input_selection_row <- function (global = global_list( )) {


  fluidRow(evaluation_tab_input_selection_row_species(global = global),
           evaluation_tab_input_selection_row_dataset(global = global),
           evaluation_tab_input_selection_row_model(global = global),
           evaluation_tab_input_selection_row_historic_end_newmoonnumber(global = global),
           evaluation_tab_input_selection_row_newmoonnumber(global = global))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_species <- function (global = global_list( )) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_species",
                     label    = "Species",
                     choices  = global$initial_evaluation_tab_available_species,
                     selected = global$initial_evaluation_tab_selected_species))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_dataset <- function (global = global_list( )) {

  column(width = 2,
         selectInput(inputId  = "evaluation_tab_dataset",
                     label    = "Dataset",
                     choices  = global$initial_evaluation_tab_available_datasets,
                     selected = global$initial_evaluation_tab_selected_dataset))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_model <- function (global = global_list( )) {

  column(width = 3,
         selectInput(inputId  = "evaluation_tab_model",
                     label    = "Model",
                     choices  = global$initial_evaluation_tab_available_models,
                     selected = global$initial_evaluation_tab_selected_model))

}


#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_historic_end_newmoonnumber <- function (global = global_list( )) {

  column(width = 2,
         selectInput(inputId  = "evaluation_tab_historic_end_newmoonnumber",
                     label    = "Origin Newmoon",
                     choices  = global$initial_evaluation_tab_available_historic_end_newmoonnumbers,
                     selected = global$initial_evaluation_tab_selected_historic_end_newmoonnumber))

}

#' @rdname web-app-ui
#'
#' @export
#
evaluation_tab_input_selection_row_newmoonnumber <- function (global = global_list( )) {

  column(width = 2,
         selectInput(inputId  = "evaluation_tab_newmoonnumber",
                     label    = "Target Newmoon",
                     choices  = global$initial_evaluation_tab_available_newmoonnumbers,
                     selected = global$initial_evaluation_tab_selected_newmoonnumber))

}

#' @rdname web-app-ui
#'
#' @export
#'
covariates_tab <- function (global = global_list( )) {

  if (is.null(global$covariates)) {

    tabPanel(title = "Covariates", 
             br( ), 
             HTML("There are not sufficient covariate data to generate plots."), 
             br( ), 
             br( ), 
             data_sources_section( ),
             br( ))
  } else {  

    tabPanel(title = "Covariates", 
           br( ), 
           data_sources_section( ),
           br( ))

  }

}


#' @rdname web-app-ui
#'
#' @export
#'
data_sources_section <- function ( ) {

  div(h3("Data Sources"),
      br( ),
      h4("Local Weather"),
      p(HTML(text = paste0(portal_project_href( ), " collates on-site ", portal_weather_href( ), " dating back to 1980 in the ", portal_data_href( ), "."))),
      br( ),
      h4(a("Normalized Difference Vegetation Index (NDVI)", href = "https://earthobservatory.nasa.gov/features/MeasuringVegetation/measuring_vegetation_2.php")),
      p(HTML(text = paste0(portal_project_href( ), " also produces site-specific ", portal_ndvi_href( ), " housed in the ", portal_data_href( ), "."))),
      br( ),
      h4("Forecast Weather"),
      p(HTML(text = paste0("We use ",
                           a("downscaled climate forecasts", href = "https://climate.northwestknowledge.net/RangelandForecast/download.php"), " from the ", 
                           a("University of Idaho's Northwest Knowledge Network's", href = "https://www.iids.uidaho.edu/nkn.php"), " API to the ",
                           a("North American Multi-Model Ensemble (NMME)", href = "https://www.cpc.ncep.noaa.gov/products/NMME/"), "."))))


}

#' @rdname web-app-ui
#'
#' @export
#'
portal_project_href <- function (text ="The Portal Project") {

  a(href   = "http://portal.weecology.org", 
    text, 
    target = "_blank")

}

#' @rdname web-app-ui
#'
#' @export
#'
portal_data_href <- function (text = "The Portal Data Repository") {

  a(href = "https://github.com/weecology/PortalData", 
    text)

}

#' @rdname web-app-ui
#'
#' @export
#'
portal_weather_href <- function (text = "weather data") {

  a(href = "https://github.com/weecology/PortalData/tree/main/Weather", 
    text)

}

#' @rdname web-app-ui
#'
#' @export
#'
portal_ndvi_href <- function (text = "NDVI data") {

  a(href = "https://github.com/weecology/PortalData/tree/main/NDVI", 
    text)

}

#' @rdname web-app-ui
#'
#' @export
#
write_rodent_profiles_tab_html <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  profiles_csv <- file.path(main, settings$subdirectories$app, "www", "rodents.csv")

  table_in <- read.csv(profiles_csv)
  nspecies <- nrow(table_in)
  table_rows <- NULL

  for (i in 1:nspecies) {

    table_row <- c('  <tr>', 
                   paste0('   <td style="text-align:left;"> <img src="', table_in$image[i], '" width ="200" alt="', table_in$image_alt_text[i], '"></td>'),
                   paste0('   <td style="text-align:left;"><i>', table_in$scientific_name[i], '</i></td>'),
                   paste0('   <td style="text-align:left;"> ', table_in$common_name[i], ' </td>'),
                   paste0('   <td style="text-align:left;"> ', table_in$species_description[i], ' </td>'),
                   '  </tr>')
    table_rows <- c(table_rows, table_row)

  }

  table_rows <- paste0(table_rows, collapse = "\n")

  html_out <- paste0(

    '<html>
     <head>
     <style>
     table, th, td {
       border: 1px solid lightgray;
       border-collapse: collapse;
     }
     th, td {
       padding: 15px;
     }
     </style>
     </head>
     <body>
     <br>
     <br>
     <table>
      <thead>
       <tr>
        <th style="text-align:left;"> Rodents </th>
        <th style="text-align:left;"> Species </th>
        <th style="text-align:left;"> Common Name </th>
        <th style="text-align:left;"> Description </th>
       </tr>
      </thead>
     <tbody>\n',
     table_rows,
     '\n</tbody>
     </table>
     </body>
     ', collapse = '\n')

  profiles_html <- file.path(main, settings$subdirectories$app, "profile.html")
  write(x    = html_out, 
        file = profiles_html)

  html_out

}

#' @rdname web-app-ui
#'
#' @export
#
write_models_tab_html <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  all_model_controls <- read_model_controls(main = main)

  model_names <- unlist(mapply(getElement, model_controls(main = main), "metadata")["print_name", ])
  nmodels     <- length(model_names)

  model_names_print <- paste(c(model_names[1:(nmodels - 1)], paste0("and ", model_names[nmodels])), collapse = ", ")

  mybib       <- ReadBib(file.path(main, settings$subdirectories$app, "refs.bibtex"))

  model_html  <- NULL
  model_text  <- named_null_list(element_names = model_names)

  for (i in 1:nmodels) {

    model_text[[i]] <- mark(text = all_model_controls[[i]]$metadata$text, format = "html")

    # working here --- need to convert the rmarkdown reference tag format to html

    model_html[i]   <- paste0('<div>
                               <h2>', 
                               model_names[i],
                               '</h2>',
                               model_text[[i]],
                               '</div>')
  }

  models_html <- paste(model_html, collapse = "")

  bib_entries <- NULL#

  references_html <- paste0('<div>
                             <h2>
                             References
                             </h2>',
                             bib_entries,
                             '</div>')

  html_out <- paste0(

    '<html>
     <head>
     <style>

     </style>
     </head>
     <body>

     <br>
     <br>
     <p>
     We currently analyze and forecast rodent data at Portal using ', english(nmodels), ' models: ', model_names_print, '
     <span class="citation">(WeecologyLab 2019)</span>.
     </p>
     <br>', 
     models_html, 
     references_html, 
     '</body>
     ', collapse = '\n')

  file_path <- file.path(main, settings$subdirectories$app, "models.html")
  write(x    = html_out, 
        file = file_path)

  html_out

}