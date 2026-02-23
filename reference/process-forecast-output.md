# Process and Save Forecast Output to Files

Take the model fit and forecast output, process them into savable
objects, and save them to the output folders.  
The forecast metadata file is updated accordingly to track the saved
output.  
`add_observations_to_forecast_table` appends a column of observations to
a forecast's forecast tab. If a model interpolated a data set, it adds
the true (non-interpolated) observations so that model predictions are
all compared to the same data.  
`select_forecasts` determines the forecasts that match user
specifications. Functionally, it is a wrapper on
`read_forecasts_metadata` with filtering for specifications that
provides a simple user interface to the large set of available forecasts
via the metadata.

## Usage

``` r
process_model_output(
  main = ".",
  model_fit = NULL,
  model_forecast,
  model,
  dataset,
  species
)

read_forecast_table(main = ".", forecast_id = NULL)

read_forecasts_tables(main = ".", forecasts_ids = NULL)

add_observations_to_forecast_table(main = ".", forecast_table = NULL)

read_forecast_metadata(main = ".", forecast_id = NULL)

read_model_fit(main = ".", forecast_id = NULL)

read_model_forecast(main = ".", forecast_id = NULL)

select_forecasts(
  main = ".",
  forecasts_metadata = NULL,
  forecasts_ids = NULL,
  forecasts_groups = NULL,
  models = NULL,
  datasets = NULL,
  species = NULL,
  historic_end_newmoonnumbers = NULL
)

read_forecasts_metadata(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- model_fit, model_forecast:

  Output from a model's fit and forecast functions.

- model, models:

  `character` values of the name(s) of the model(s) of interest, as
  indexed within the directory in the `forecasts` sub folder. See the
  forecasts metadata file (`forecasts_metadata.csv`) for summary
  information. If `NULL` (the default), the most recently generated
  forecast's output is read in.  
  `models` can be NULL, one value, or more than one values, `model` can
  only be NULL or one value.

- dataset, datasets:

  `character` vector of the rodent dataset name(s) to include.
  `datasets` can be NULL, one value, or more than one values, `dataset`
  can only be NULL or one value.

- species:

  `character` value of the species codes (or `"total"` for the total
  across species) to include. Default value is `NULL`, which equates to
  no selection with respect to `species`.

- forecast_id, forecasts_ids:

  `integer` (or integer `numeric`) value(s) representing the forecast(s)
  of interest, as indexed within the directory in the `casts` sub
  folder. See the forecasts metadata file (`forecasts_metadata.csv`) for
  summary information. If `NULL` (the default), the most recently
  generated forecast's output is read in.  
  `forecasts_ids` can be NULL, one value, or more than one values,
  `forecast_id` can only be NULL or one value.

- forecast_table:

  A `data.frame` of a cast's output. See `read_forecast_table`.

- forecasts_metadata:

  `data.frame` of forecast metadata. If `NULL` (default), will try to
  read via `read_forecasts_metadata`.

- forecasts_groups:

  `integer` (or integer `numeric`) value of the forecast groups to
  include. Default value is `NULL`, which equates to no selection with
  respect to `forecast_group`.

- historic_end_newmoonnumbers:

  `integer` (or integer `numeric`) newmoon numbers of the forecast
  origin. Default value is `NULL`, which equates to no selection.

## Value

`process_model_output`: relevant elements are saved to external files,
and returned as a `list`.  
`read_forecast_table`: forecast table `data.frame`.  
`read_forecast_tables`: `data.frame` of combined forecast tables.  
`add_observations_to_forecast_table`: forecast table `data.frame` with
an observation column added.  
`read_forecast_metadata`: `list` of `forecast_metadata`.  
`read_model_fit`: forecast output (typically as a `list`).  
`read_model_forecast`: forecast output (typically as a `list`).  
`select_forecasts`: `data.frame` of selected forecasts' metadata.  
`read_forecasts_metadata`: `data.frame` of forecasts' metadata.

## Details

Four model-specific output components are saved and returned: \*
`forecast_metadata`: saved out with
[`write_yaml`](https://yaml.r-lib.org/reference/write_yaml.html). \*
`forecast_tab`: saved using `write_csv_arrow`. \* `model_fit`: saved out
as a serialized `JSON` file via
[`serializeJSON`](https://jeroen.r-universe.dev/jsonlite/reference/serializeJSON.html)
and
[`read_json`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html),
so quite flexible with respect to specific object structure. \*
`model_forecast`: saved out as a serialized `JSON` file via
[`serializeJSON`](https://jeroen.r-universe.dev/jsonlite/reference/serializeJSON.html)
and
[`read_json`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html),
so quite flexible with respect to specific object structure.

## See also

Core forecasting functions:
[`ensemble`](https://weecology.github.io/portalcasting/reference/ensemble.md),
[`evaluate forecasts`](https://weecology.github.io/portalcasting/reference/evaluate-forecasts.md),
[`portalcast()`](https://weecology.github.io/portalcasting/reference/portalcast.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "forecast_output")

   setup_dir(main = main1)
   dataset <- "all"
   species <- "DM"
   model   <- "AutoArima"
 
   abundance      <- prepare_abundance(main    = main1,
                                       dataset = dataset,
                                       species = species,
                                       model   = model)
   model_controls <- models_controls(main      = main1,
                                     models    = model)[[model]]
   metadata       <- read_metadata(main        = main1)
   newmoons       <- read_newmoons(main        = main1)                                        
   covariates     <- read_covariates(main      = main1)
 
   fit_args  <- named_null_list(element_names = names(model_controls$fit$args))
   for (i in 1:length(fit_args)) {
     fit_args[[i]] <- eval(parse(text = model_controls$fit$args[i]))
   }
   model_fit  <- do.call(what = model_controls$fit$fun,
                         args = fit_args)
 
 
   forecast_args  <- named_null_list(element_names = names(model_controls$forecast$args))
   for (i in 1:length(forecast_args)) {
     forecast_args[[i]] <- eval(parse(text = model_controls$forecast$args[i]))
   }
 
   model_forecast <- do.call(what = model_controls$forecast$fun,
                             args = forecast_args)
 
   process_model_output(main           = main1,
                        model_fit      = model_fit,
                        model_forecast = model_forecast,
                        model          = model,
                        dataset        = dataset,
                        species        = species) 

   cast_table     <- read_forecast_table(main = main1)
   cast_table2    <- add_observations_to_forecast_table(main = main1,
                                                        forecast_table = cast_table)
   cast_tables    <- read_forecast_tables(main = main1)
   cast_metadata  <- read_forecast_metadata(main = main1)
   cast_forecast  <- read_model_forecast(main = main1)

   casts_metadata <- read_forecasts_metadata(main = main1)
   select_forecasts(main = main1)

   unlink(main1, recursive = TRUE)
} # }
```
