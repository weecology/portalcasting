% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settings.R
\name{directory settings}
\alias{directory settings}
\alias{settings}
\alias{directory_settings}
\alias{time_settings}
\alias{directory_files}
\alias{directory_subdirectories}
\alias{directory_resources}
\alias{production_settings}
\alias{sandbox_settings}
\title{Create a Directory Settings List}
\usage{
directory_settings(
  files = directory_files(),
  subdirectories = directory_subdirectories(),
  resources = directory_resources(),
  time = time_settings(),
  confidence_level = 0.95,
  nsamples = 10000,
  save = TRUE,
  overwrite = TRUE,
  force = FALSE,
  unzip_pause = 30,
  download_timeout = getOption("timeout")
)

time_settings(
  timeseries_start = as.Date("1995-01-01"),
  origin = Sys.Date(),
  forecast_date = Sys.Date(),
  lead_time = 365,
  max_lag = 365,
  lag_buffer = 60,
  lead_time_buffer = 30
)

directory_files(
  directory_configuration = "directory_configuration.yaml",
  app = "app.R",
  newmoons = "newmoons.csv",
  covariates = "covariates.csv",
  datasets_controls = "datasets_controls.yaml",
  models_controls = "models_controls.yaml",
  forecasts_evaluations = "forecasts_evaluations.csv",
  forecasts_results = "forecasts_results.csv",
  forecasts_metadata = "forecasts_metadata.csv",
  metadata = "metadata.yaml",
  about_md = "about.md",
  models_html = "models.html",
  models_rmd = "models.Rmd",
  rodents_profiles_html = "rodents_profiles.html",
  rodents_profiles_csv = "rodents_profiles.csv",
  species_names = "species_names.csv"
)

directory_subdirectories(
  forecasts = "forecasts",
  fits = "fits",
  models = "models",
  resources = "resources",
  data = "data",
  www = "www"
)

directory_resources(
  PortalData = list(source = "github", version = "latest"),
  portalPredictions = list(source = "github", version = NULL),
  climate_forecasts = list(source = "NMME", version = as.character(Sys.Date()), data =
    list(mintemp = "tasmin", meantemp = "tasmean", maxtemp = "tasmax", precipitation =
    "pr"))
)

production_settings(download_timeout = max(getOption("timeout"), 600))

sandbox_settings()
}
\arguments{
\item{files}{\code{list} of \code{character} names of standard files, see \code{\link{directory_files}}.}

\item{subdirectories}{\code{list} of \code{character} names of standard subdirectories, see \code{\link{directory_subdirectories}}.}

\item{resources}{\code{character} name for the resources subdirectory.}

\item{time}{\code{list} of time settings, see \code{\link{time_settings}}.}

\item{confidence_level}{\code{numeric} confidence level used in summarizing model output. Must be between \code{0} and \code{1}.}

\item{nsamples}{\code{integer} (or integer \code{numeric}) number of samples used to summarizing model output of sample-based estimates.}

\item{save}{\code{logical} indicator controlling if the output should be saved out.}

\item{overwrite}{\code{logical} indicator of whether or not file writing should occur even if a local copy already exists.}

\item{force}{\code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).}

\item{unzip_pause}{Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delay.}

\item{download_timeout}{Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base:options]{base::options}}.}

\item{timeseries_start}{\code{Date} after which historic samples are included in the timeseries fit. Default value is \code{1995-01-01}, corresponding to moon 217.}

\item{origin}{\code{Date} forecast origin. Default is today's date (set using \code{\link{Sys.Date}}).}

\item{forecast_date}{\code{Date} of when the forecasts are occurring. Default is today's date (set using \code{\link{Sys.Date}}).}

\item{lead_time}{\code{integer} (or integer \code{numeric}) value for the number of calendar days forward a forecast will cover. \cr
As of version 0.51.0, default is now \code{365}, which when divided by 29.5 (duration of a lunar month), gives 13. The previous value was previously 12. We are now using 13 to align with the timestep being a lunar month, and 13 lunar months covers a full calendar year.}

\item{max_lag}{\code{integer} (or integer \code{numeric}) maximum number of calendar days that any covariate is lagged for prediction in a model. \cr
Default is \code{365} for the logistic covariate models.}

\item{lag_buffer}{\code{integer} (or integer \code{numeric}) additional number of calendar days back in time to add to the maximum lag. \cr
Default value of \code{60} corresponds to two additional lunar months.}

\item{lead_time_buffer}{\code{integer} (or integer \code{numeric}) additional number of calendar days forward in time to forecast. \cr
Default value of \code{30} corresponds to one additional lunar month.}

\item{directory_configuration}{\code{character} name for the directory configuration YAML.}

\item{app}{\code{character} name for the application R file.}

\item{newmoons}{\code{character} name for the lunar data csv.}

\item{covariates}{\code{character} name for the combined historical and forecast covariates csv.}

\item{datasets_controls}{\code{character} name for the YAML of datasets control list(s).}

\item{models_controls}{\code{character} name for the YAML of models controls list(s).}

\item{forecasts_evaluations}{\code{character} name for the forecast evaluations csv.}

\item{forecasts_results}{\code{character} name for the forecast combination results csv.}

\item{forecasts_metadata}{\code{character} name for the forecast metadata csv.}

\item{metadata}{\code{character} name for the Forecast metadata YAML.}

\item{about_md}{\code{character} name for the md file for the about page of the app.}

\item{models_html}{\code{character} name for the html file where the models page will be saved for the app.}

\item{models_rmd}{\code{character} name for the Rmd file for the models page of the app.}

\item{rodents_profiles_html}{\code{character} name for the html file where the rodents profiles page will be saved for the app.}

\item{rodents_profiles_csv}{\code{character} name for the csv file containing content for the rodents profiles table for the app.}

\item{species_names}{\code{character} name for the csv file containing the output from \code{\link[portalr:rodent_species]{portalr::rodent_species}}, with \code{set = "forecasting"}, \code{type = "table"}, and \code{total = TRUE}.}

\item{forecasts}{\code{character} name for the forecasts subdirectory.}

\item{fits}{\code{character} name for the fits subdirectory.}

\item{models}{\code{character} name for the models subdirectory.}

\item{data}{\code{character} name for the data subdirectory.}

\item{www}{\code{character} name for the application www helpers subdirectory.}

\item{PortalData}{\code{list} of \code{source} and \code{version} elements of \code{character} values for the Portal Data download. Default values retrieve the latest data from github}

\item{portalPredictions}{\code{list} of \code{source} and \code{version} elements of \code{character} values for the archive download. Default values point to github, but \code{verison = NULL} indicates no download.}

\item{climate_forecasts}{\code{list} of \code{source}, \code{version}, and \code{data} elements of \code{character} values for the climate forecasts download. Default values retrieve the current day's forecast of min, mean, and max temperature and precipitation from the Northwest Knowledge Network's North American Multi-Model Ensemble (NMME) climate forecasts.}
}
\value{
Named \code{list} of settings for the directory (for \code{directory_settings}) or \code{list} of settings components (for \code{directory_files}, \code{directory_subdirectories}, and \code{directory_resources}).
}
\description{
Compose parts of and complete directory settings controls lists. \cr \cr
Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and \code{directory_files}, \code{directory_subdirectories}, and \code{directory_settings} gather them into a set of lists for pipeline functionality. \cr
\code{directory_resources} dictates the \code{source} and \code{version} (and any other specialized arguments) for each of the raw resources collected for the directory. \cr
\code{time_settings} collates the temporal aspects of the data, and can be used to set up alternate forecast timelines for specialized projects and sandboxing.
}
\examples{
   directory_settings( )
   directory_files( )
   directory_subdirectories( )
   directory_resources( )
   time_settings( )


}
\seealso{
Directory orchestration functions:
\code{\link{directory configuration file}},
\code{\link{directory creation}},
\code{\link{directory filling}},
\code{\link{directory paths}}
}
\concept{customization}
\concept{orchestration}
