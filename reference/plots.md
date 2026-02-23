# Visualize Portalcasting Data and Forecasts

`plot_forecasts_error_lead`: lots the raw error (estimate - observation)
as a function of lead time across model runs from different forecast
origins for multiple models and multiple species (or total) within a
data set.  
`plot_covariates`: plots an observed timeseries and forecast timeseries
of the covariates used.  
`plot_forecast_ts`: plots an observed timeseries and forecast timeseries
with a prediction interval. Observations that occurred after the
forecast are shown connected directly to the pre-cast observation data
(as the black solid line with points).  
`plot_forecast_point`: plots the point value with confidence interval
for a time point across multiple species. Casts can be selected either
by supplying a `forecast_id` number or any combination of `dataset`,
`model`, and `historic_end_newmoonnumber`, which filter the available
forecasts in unison. This plot type can only handle output from a single
forecast, so if multiple forecasts still remain, the one with the
highest number is selected. To be more certain about forecast selection,
use the `forecast_id` input.  
`plot_forecasts_cov_RMSE`: plots the coverage (fraction of predictions
within the CI) and RMSE (root mean squared error) of each model among
multiple species.

## Usage

``` r
plot_forecasts_error_lead(
  main = ".",
  forecasts_ids = NULL,
  forecasts_evaluations = NULL,
  historic_end_newmoonnumbers = NULL,
  models = NULL,
  datasets = NULL,
  species = NULL
)

plot_forecasts_cov_RMSE(
  main = ".",
  forecasts_metadata = NULL,
  forecasts_ids = NULL,
  forecasts_evaluations = NULL,
  historic_end_newmoonnumbers = NULL,
  models = NULL,
  datasets = NULL,
  species = NULL
)

plot_forecast_point(
  main = ".",
  forecasts_metadata = NULL,
  forecast_id = NULL,
  dataset = NULL,
  model = NULL,
  historic_end_newmoonnumber = NULL,
  species = NULL,
  highlight_sp = NULL,
  newmoonnumber = NULL,
  with_census = FALSE
)

plot_forecast_ts(
  main = ".",
  forecasts_metadata = NULL,
  forecast_id = NULL,
  dataset = NULL,
  model = NULL,
  historic_start_newmoonnumber = NULL,
  historic_end_newmoonnumber = NULL,
  species = NULL
)

plot_covariates(main = ".", to_plot = "ndvi")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- forecasts_evaluations:

  `data.frame` of forecast evaluations, as returned from
  [`evaluate_forecasts`](https://weecology.github.io/portalcasting/reference/evaluate-forecasts.md).
  If `NULL` (default), will try to read via
  [`read_forecasts_evaluations`](https://weecology.github.io/portalcasting/reference/evaluate-forecasts.md).

- species:

  `character` vector of the species code(s) or `"total"` for the total
  across species) to be plotted `NULL` translates to the species defined
  by
  [`forecasting_species`](https://weecology.github.io/portalr/reference/rodent_species.html)
  called by
  [`prefab_species`](https://weecology.github.io/portalcasting/reference/prefabricated-rodents-datasets.md).

- forecasts_metadata:

  `data.frame` of forecast metadata. If `NULL` (default), will try to
  read via
  [`read_forecasts_metadata`](https://weecology.github.io/portalcasting/reference/process-forecast-output.md).

- forecast_id, forecasts_ids:

  `integer` (or integer `numeric`) values representing the forecasts of
  interest for restricting plotting, as indexed within the directory in
  the `casts` sub folder. See the forecasts metadata file
  (`forecasts_metadata.csv`) for summary information. `forecast_id` can
  only be length-1 or `NULL`, whereas `forecasts_ids` is not
  length-restricted.

- dataset, datasets:

  `character` value of the rodent data set(s) to include. `dataset` can
  only be length-1 or `NULL`, whereas `datasets` is not
  length-restricted.

- model, models:

  `character` value(s) of the name of the model to include. Default
  value is `NULL`, which equates to no selection with respect to `model`
  or `models`. `model` can only be length-1 or `NULL`, whereas `models`
  is not length-restricted.

- historic_end_newmoonnumber, historic_end_newmoonnumbers:

  `integer` (or integer `numeric`) newmoon number(s) of the forecast
  origin. Default value is `NULL`, which equates to no selection.
  `historic_end_newmoonnumber` can only be length-1 or `NULL`, whereas
  `historic_end_newmoonnumbers` is not length-restricted.

- highlight_sp:

  `character` vector of the species codes (or `"total"` for the total
  across species) to be highlighted or `NULL` (default) to not highlight
  anything.

- newmoonnumber:

  `integer` (or integer `numeric`) newmoon number for the plot.

- with_census:

  `logical` toggle if the plot should include the observed data
  collected during the predicted census.

- historic_start_newmoonnumber:

  `integer` (or integer `numeric`) newmoon number for the beginning of
  the x-axis of the plot.  
  Does not influence the fit of the models, just the presentation.

- to_plot:

  `character` of the covariate to plot, restricted to column names in
  the covariates table (see
  [`read_covariates`](https://weecology.github.io/portalcasting/reference/read-write-data.md)).

## Value

`NULL`. Plot is generated.

## Details

Casts can be selected either by supplying a `forecast_id` number or any
combination of `dataset`, `model`, and `historic_end_newmoonnumber`,
which filter the available forecasts in unison. This plot type can only
handle output from a single forecast, so if multiple forecasts still
remain, the one with the highest number is selected. To be more certain
about forecast selection, use the `forecast_id` input.  
As of `portalcasting v0.9.0`, the line and bands in `plot_forecast_ts`
and point and bars in `plot_forecast_point` represent the mean and the
95 percent prediction interval.  

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "figures")
   setup_production(main = main1)

   plot_covariates(main = main1)

   portalcast(main = main1, models = "AutoArima")

   ids <- select_forecasts(main     = main3, 
                           species  = c("DM", "PP", "total"),
                           models   = c("AutoArima", "ESSS", "pevGARCH", "nbGARCH", "jags_RW"),
                           datasets = c("all", "controls"))$forecast_id
   nids         <- length(ids)
   nsample_ids  <- 1000
   forecasts_ids <- ids[round(seq(1, nids, length.out = nsample_ids))]
   evaluate_forecasts(main         = main3, 
                      forecasts_ids = forecasts_ids) 

   plot_forecast_ts(main = main1)
   plot_forecast_point(main = main1)
   plot_forecasts_error_lead(main = main1)
   plot_forecasts_cov_RMSE(main    = main1, 
                           models  = "AutoArima", 
                           species = "DM")

   unlink(main1, recursive = TRUE)
} # }
```
