#' @keywords internal
"_PACKAGE"

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
#'   # Evaluate specific forecasts
#'   evaluate_forecasts(main = "~/portal_forecasts",
#'                      forecasts_ids = c(1, 2, 3))
#' }
#'
#' **5. Visualization:**
#' 
#' Create publication-quality plots:
#' 
#' \preformatted{
#'   # Plot forecast time series
#'   plot_forecast_ts(main = "~/portal_forecasts")
#'   
#'   # Plot forecast diagnostics
#'   plot_forecasts_error_lead(main = "~/portal_forecasts")
#'   
#'   # Plot forecast performance metrics
#'   plot_forecasts_cov_RMSE(main = "~/portal_forecasts")
#' }
#'
#' **6. Web Application:**
#' 
#' Launch an interactive Shiny application to explore forecasts:
#' 
#' \preformatted{
#'   run_app(main = "~/portal_forecasts")
#' }
#'
#' @section Directory Structure:
#' 
#' A portalcasting directory contains:
#' 
#' \itemize{
#'   \item \strong{data/}: Raw and processed data files
#'   \item \strong{models/}: Model definitions and metadata
#'   \item \strong{forecasts/}: Forecast outputs and metadata
#'   \item \strong{fits/}: Model fit objects
#'   \item \strong{resources/}: Rodent species information
#'   \item \strong{www/}: Web application assets
#'   \item \strong{directory_configuration.yaml}: Directory metadata
#' }
#'
#' @section Key Functions by Category:
#' 
#' **Directory Management:**
#' \itemize{
#'   \item \code{\link{create_dir}}: Create directory structure
#'   \item \code{\link{setup_production}}: Production environment setup
#'   \item \code{\link{setup_sandbox}}: Sandbox environment setup
#'   \item \code{\link{update_dir}}: Update existing directory
#' }
#'
#' **Data Preparation:**
#' \itemize{
#'   \item \code{\link{prepare_rodents}}: Prepare rodent abundance data
#'   \item \code{\link{prepare_covariates}}: Prepare environmental covariates
#'   \item \code{\link{prepare_newmoons}}: Prepare lunar data
#'   \item \code{\link{prepare_metadata}}: Prepare forecast metadata
#' }
#'
#' **Modeling:**
#' \itemize{
#'   \item \code{\link{portalcast}}: Main forecasting pipeline
#'   \item \code{\link{prefab_models}}: Available model configurations
#'   \item \code{\link{add_new_model}}: Add custom model
#' }
#'
#' **Evaluation:**
#' \itemize{
#'   \item \code{\link{evaluate_forecasts}}: Evaluate forecast accuracy
#'   \item \code{\link{evaluate_forecast}}: Evaluate single forecast
#' }
#'
#' **Visualization:**
#' \itemize{
#'   \item \code{\link{plot_forecast_ts}}: Time series forecast plots
#'   \item \code{\link{plot_forecast_point}}: Point forecast plots
#'   \item \code{\link{plot_forecasts_error_lead}}: Error vs lead time plots
#'   \item \code{\link{plot_forecasts_cov_RMSE}}: Performance diagnostic plots
#'   \item \code{\link{plot_covariates}}: Covariate plots
#' }
#'
#' **Web Application:**
#' \itemize{
#'   \item \code{\link{run_app}}: Launch interactive web app
#' }
#'
#' @section Advanced Features:
#' 
#' **Custom Datasets:**
#' 
#' Add custom rodent datasets:
#' 
#' \preformatted{
#'   add_new_dataset(main = "~/portal_forecasts",
#'                   dataset = "my_dataset",
#'                   controls_filename = "my_controls.yaml")
#' }
#'
#' **Custom Models:**
#' 
#' Add custom forecasting models:
#' 
#' \preformatted{
#'   add_new_model(main = "~/portal_forecasts",
#'                 model = "my_model",
#'                 controls_filename = "my_model_controls.yaml")
#' }
#'
#' **Ensemble Forecasts:**
#' 
#' Combine multiple model forecasts:
#' 
#' \preformatted{
#'   ensemble_forecasts(main = "~/portal_forecasts")
#' }
#'
#' @section Getting Help:
#' 
#' - Package website: \url{https://weecology.github.io/portalcasting/}
#' - GitHub repository: \url{https://github.com/weecology/portalcasting}
#' - Report issues: \url{https://github.com/weecology/portalcasting/issues}
#'
#' @section Citation:
#' 
#' To cite portalcasting in publications, use:
#' 
#' \preformatted{
#'   citation("portalcasting")
#' }
#'
#' @section Related Projects:
#' 
#' - Portal Predictions: \url{https://portal.naturecast.org}
#' - portalr package: Data retrieval for Portal Project
#' - Portal Project: \url{https://portal.weecology.org}
#'
#' @docType package
#' @name portalcasting-package
#' @aliases portalcasting
NULL
