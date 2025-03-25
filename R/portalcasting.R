#' @importFrom arrow read_csv_arrow write_csv_arrow read_parquet write_parquet
#' @importFrom bslib bs_theme
#' @importFrom coda as.mcmc HPDinterval
#' @importFrom english english
#' @importFrom forecast Arima auto.arima ets forecast na.interp
#' @importFrom git2r last_commit
#' @importFrom graphics abline axis mtext par plot points polygon rect text
#' @importFrom grDevices grey rgb
#' @importFrom htmltools a br div h2 h3 h4 HTML includeHTML includeMarkdown p tags
#' @importFrom httr content GET stop_for_status
#' @importFrom hypergeo hypergeo
#' @importFrom jsonlite fromJSON serializeJSON unserializeJSON write_json
#' @importFrom knitr opts_chunk
#' @importFrom markdown mark
#' @importFrom portalr download_observations forecasting_species get_future_newmoons load_datafile na_conformer ndvi return_if_null rodent_species summarize_rodent_data weather
#' @importFrom rmarkdown knitr_options output_format pandoc_options render
#' @importFrom runjags combine.mcmc run.jags runjags.options
#' @importFrom scoringRules crps crps_nbinom crps_norm crps_pois crps_sample logs logs_sample logs_nbinom logs_norm logs_pois 
#' @importFrom shiny column fluidPage fluidRow getShinyOption mainPanel observeEvent onStop plotOutput reactiveValues renderPlot renderText runApp selectInput shinyApp shinyOptions tabPanel tabsetPanel textOutput titlePanel updateSelectInput
#' @importFrom stats AIC as.ts filter frequency lm na.omit predict qnorm quantile rgamma rnorm runif sd
#' @importFrom tscount tsglm
#' @importFrom utils download.file find globalVariables help.search packageDescription read.csv sessionInfo tail unzip write.csv write.table
#' @importFrom viridis viridis
#' @importFrom yaml read_yaml write_yaml
#'
#' @keywords internal
#'
"_PACKAGE"

utils::globalVariables(c("main", "global"))
