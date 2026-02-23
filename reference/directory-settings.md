# Create a Directory Settings List

Compose parts of and complete directory settings controls lists.  
  
Most users will not want or need to change the directory folders and
file names, but it is helpful to have them be flexible for certain
circumstances, and `directory_files`, `directory_subdirectories`, and
`directory_settings` gather them into a set of lists for pipeline
functionality.  
`directory_resources` dictates the `source` and `version` (and any other
specialized arguments) for each of the raw resources collected for the
directory.  
`time_settings` collates the temporal aspects of the data, and can be
used to set up alternate forecast timelines for specialized projects and
sandboxing.

## Usage

``` r
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
```

## Arguments

- files:

  `list` of `character` names of standard files, see `directory_files`.

- subdirectories:

  `list` of `character` names of standard subdirectories, see
  `directory_subdirectories`.

- resources:

  `character` name for the resources subdirectory.

- time:

  `list` of time settings, see `time_settings`.

- confidence_level:

  `numeric` confidence level used in summarizing model output. Must be
  between `0` and `1`.

- nsamples:

  `integer` (or integer `numeric`) number of samples used to summarizing
  model output of sample-based estimates.

- save:

  `logical` indicator controlling if the output should be saved out.

- overwrite:

  `logical` indicator of whether or not file writing should occur even
  if a local copy already exists.

- force:

  `logical` indicator of whether or not existing files or folders (such
  as the archive) should be over-written if an up-to-date copy exists
  (most users should leave as `FALSE`).

- unzip_pause:

  Positive `integer` or integer `numeric` seconds for pausing during
  steps around unzipping that require time delay.

- download_timeout:

  Positive `integer` or integer `numeric` seconds for timeout on
  downloads. Temporarily overrides the `"timeout"` option in
  [`base::options`](https://rdrr.io/r/base/options.html).

- timeseries_start:

  `Date` after which historic samples are included in the timeseries
  fit. Default value is `1995-01-01`, corresponding to moon 217.

- origin:

  `Date` forecast origin. Default is today's date (set using
  [`Sys.Date`](https://rdrr.io/r/base/Sys.time.html)).

- forecast_date:

  `Date` of when the forecasts are occurring. Default is today's date
  (set using [`Sys.Date`](https://rdrr.io/r/base/Sys.time.html)).

- lead_time:

  `integer` (or integer `numeric`) value for the number of calendar days
  forward a forecast will cover.  
  As of version 0.51.0, default is now `365`, which when divided by 29.5
  (duration of a lunar month), gives 13. The previous value was
  previously 12. We are now using 13 to align with the timestep being a
  lunar month, and 13 lunar months covers a full calendar year.

- max_lag:

  `integer` (or integer `numeric`) maximum number of calendar days that
  any covariate is lagged for prediction in a model.  
  Default is `365` for the logistic covariate models.

- lag_buffer:

  `integer` (or integer `numeric`) additional number of calendar days
  back in time to add to the maximum lag.  
  Default value of `60` corresponds to two additional lunar months.

- lead_time_buffer:

  `integer` (or integer `numeric`) additional number of calendar days
  forward in time to forecast.  
  Default value of `30` corresponds to one additional lunar month.

- directory_configuration:

  `character` name for the directory configuration YAML.

- app:

  `character` name for the application R file.

- newmoons:

  `character` name for the lunar data csv.

- covariates:

  `character` name for the combined historical and forecast covariates
  csv.

- datasets_controls:

  `character` name for the YAML of datasets control list(s).

- models_controls:

  `character` name for the YAML of models controls list(s).

- forecasts_evaluations:

  `character` name for the forecast evaluations csv.

- forecasts_results:

  `character` name for the forecast combination results csv.

- forecasts_metadata:

  `character` name for the forecast metadata csv.

- metadata:

  `character` name for the Forecast metadata YAML.

- about_md:

  `character` name for the md file for the about page of the app.

- models_html:

  `character` name for the html file where the models page will be saved
  for the app.

- models_rmd:

  `character` name for the Rmd file for the models page of the app.

- rodents_profiles_html:

  `character` name for the html file where the rodents profiles page
  will be saved for the app.

- rodents_profiles_csv:

  `character` name for the csv file containing content for the rodents
  profiles table for the app.

- species_names:

  `character` name for the csv file containing the output from
  [`portalr::rodent_species`](https://weecology.github.io/portalr/reference/rodent_species.html),
  with `set = "forecasting"`, `type = "table"`, and `total = TRUE`.

- forecasts:

  `character` name for the forecasts subdirectory.

- fits:

  `character` name for the fits subdirectory.

- models:

  `character` name for the models subdirectory.

- data:

  `character` name for the data subdirectory.

- www:

  `character` name for the application www helpers subdirectory.

- PortalData:

  `list` of `source` and `version` elements of `character` values for
  the Portal Data download. Default values retrieve the latest data from
  github

- portalPredictions:

  `list` of `source` and `version` elements of `character` values for
  the archive download. Default values point to github, but
  `verison = NULL` indicates no download.

- climate_forecasts:

  `list` of `source`, `version`, and `data` elements of `character`
  values for the climate forecasts download. Default values retrieve the
  current day's forecast of min, mean, and max temperature and
  precipitation from the Northwest Knowledge Network's North American
  Multi-Model Ensemble (NMME) climate forecasts.

## Value

Named `list` of settings for the directory (for `directory_settings`) or
`list` of settings components (for `directory_files`,
`directory_subdirectories`, and `directory_resources`).

## See also

Directory orchestration functions:
[`directory configuration file`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md),
[`directory creation`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`directory paths`](https://weecology.github.io/portalcasting/reference/directory-paths.md)

## Examples

``` r
   directory_settings( )
   directory_files( )
   directory_subdirectories( )
   directory_resources( )
   time_settings( )
```
