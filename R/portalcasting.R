#' @importFrom coda as.mcmc HPDinterval
#' @importFrom english english
#' @importFrom forecast Arima auto.arima ets forecast na.interp
#' @importFrom graphics abline axis mtext par plot points polygon rect text
#' @importFrom grDevices grey rgb
#' @importFrom htmltools a br h2 HTML includeHTML includeMarkdown p tags
#' @importFrom httr content GET stop_for_status
#' @importFrom hypergeo hypergeo
#' @importFrom jsonlite fromJSON serializeJSON unserializeJSON write_json
#' @importFrom knitr opts_chunk
#' @importFrom markdown mark
#' @importFrom portalr download_observations forecasting_species get_future_newmoons load_datafile na_conformer ndvi return_if_null rodent_species summarize_rodent_data weather
#' @importFrom rmarkdown render
#' @importFrom runjags combine.mcmc run.jags runjags.options
#' @importFrom scoringRules crps crps_nbinom crps_norm crps_pois crps_sample logs logs_sample logs_nbinom logs_norm logs_pois 
#' @importFrom shiny column fluidPage fluidRow mainPanel observeEvent plotOutput reactiveValues renderPlot renderText runApp selectInput shinyApp tabPanel tabsetPanel textOutput titlePanel updateSelectInput
#' @importFrom stats AIC as.ts filter frequency lm na.omit predict qnorm quantile rgamma rnorm runif sd
#' @importFrom tscount tsglm
#' @importFrom utils download.file find packageDescription read.csv sessionInfo tail unzip write.csv write.table
#' @importFrom viridis viridis
#' @importFrom yaml read_yaml write_yaml

#' @title Functions for Portalcasting (Forecasting Portal Rodents)
#'
#' @description Functions for continuous analysis and forecasting of [Portal rodent populations](https://portal.weecology.org/), as implemented in the [Portal Predictions repository](https://www.github.com/weecology/portalPredictions) and displayed on the [Portal Forecast website](https://portal.naturecast.org). \cr
#'              Functions in `portalcasting` are designed to be portable, allowing users to set up a fully-functional replica repository for development and testing of new models (a.k.a. [**sandboxing**](https://en.wikipedia.org/wiki/Sandbox_(software_development))) in the same directory configurations, codebase, and datasets as the production pipeline. 
#'
#' @name portalcasting
#'
#' @docType package
#'
#' @keywords package
#'
NULL

