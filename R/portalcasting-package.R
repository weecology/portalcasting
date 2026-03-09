#' @importFrom arrow read_csv_arrow write_csv_arrow
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

#' portalcasting: Model and Forecast Portal Rodent Dynamics
#'
#' @description
#' The portalcasting package provides a complete pipeline for forecasting rodent populations
#' at the Portal Project field site. It enables users to:
#'
#' - Set up and configure forecasting directories
#' - Download and prepare ecological data
#' - Fit statistical and machine learning models
#' - Generate forecasts with uncertainty quantification
#' - Evaluate forecast performance
#' - Visualize results
#' - Deploy an interactive web application
#'
#' @section Main Workflows:
#'
#' **1. Directory Setup:**
#'
#' Create a new forecasting environment with \code{\link{setup_production}}
#' (production mode with fixed settings) or \code{\link{setup_sandbox}}
#' (sandbox mode for experimentation):
#'
#' \preformatted{
#'   # Production setup
#'   setup_production(main = "~/portal_forecasts")
#'
#'   # Sandbox setup
#'   setup_sandbox(main = "~/my_sandbox")
#' }
#'
#' **2. Data Preparation:**
#'
#' The package automatically downloads and prepares data including:
#' - Rodent abundance data via \code{\link{prepare_rodents}}
#' - Environmental covariates via \code{\link{prepare_covariates}}
#' - Lunar data via \code{\link{prepare_newmoons}}
#' - Climate forecasts via \code{\link{download_climate_forecasts}}
#'
#' **3. Model Fitting and Forecasting:**
#'
#' Generate forecasts using the main pipeline function:
#'
#' \preformatted{
#'   # Run forecasts with selected models
#'   portalcast(main = "~/portal_forecasts",
#'              models = c("AutoArima", "ESSS", "nbGARCH"))
#' }
#'
#' Available models include:
#' - Time series models: AutoArima, ESSS
#' - GARCH models: pevGARCH, nbGARCH
#' - Bayesian models via JAGS: jags_RW, jags_logistic
#' - Generalized linear models: NaiveArima, tsGLM
#'
#' **4. Forecast Evaluation:**
#'
#' Evaluate forecast accuracy and performance:
#'
#' \preformatted{
#'   evaluate_forecasts(main = "~/portal_forecasts")
#' }
#'
#' **5. Visualization:**
#'
#' Plot forecasts and diagnostics:
#'
#' \preformatted{
#'   plot_forecast_ts(main = "~/portal_forecasts")
#'   plot_forecast_point(main = "~/portal_forecasts")
#'   plot_forecasts_error_lead(main = "~/portal_forecasts")
#'   plot_covariates(main = "~/portal_forecasts")
#' }
#'
#' **6. Web Application:**
#'
#' Launch an interactive Shiny application:
#'
#' \preformatted{
#'   run_app(main = "~/portal_forecasts")
#' }
#'
#' @section Module Structure:
#'
#' The package is organized into domain-based modules:
#'
#' - **core**: Package-wide constants (\code{R/core/constants.R})
#' - **data**: Data download functions (\code{R/data/data_download.R})
#' - **forecasting**: Forecast storage utilities (\code{R/forecasting/forecast_storage.R})
#' - **models**: JAGS models and metadata preparation (\code{R/models/})
#' - **utils**: Utility functions for messaging, files, data, and packages (\code{R/utils/})
#' - **visualization**: Plotting functions (\code{R/visualization/})
#' - **webapp**: Shiny web application components (\code{R/webapp/})
#'
#' @name portalcasting-package
#' @aliases portalcasting
#'
NULL
