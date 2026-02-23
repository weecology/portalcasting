# Combine (Ensemble) Casts

Combine multiple forecasts' output into a single ensemble. Presently,
only a general average ensemble is available.

## Usage

``` r
ensemble_forecasts(
  main = ".",
  method = "unwtavg",
  forecasts_groups = NULL,
  forecasts_ids = NULL,
  forecast_table = NULL,
  historic_end_newmoonnumber = NULL,
  models = NULL,
  dataset = NULL,
  species = NULL
)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- method:

  `character` value of the name of the ensemble method to use.
  Presently, only `"unwtavg"` (unweighted average) is allowed.

- forecasts_groups:

  `integer` (or integer `numeric`) value of the forecasts groups to
  combine with an ensemble. If `NULL` (default), the most recent
  forecast group is ensembled.

- forecasts_ids:

  `integer` (or integer `numeric`) values representing the forecasts of
  interest for restricting ensembling, as indexed within the directory
  in the `casts` sub folder. See the forecasts metadata file
  (`forecasts_metadata.csv`) for summary information.

- forecast_table:

  Optional `data.frame` of forecast table outputs. If not input, will be
  loaded.

- historic_end_newmoonnumber:

  `integer` (or integer `numeric`) newmoon number of the forecast
  origin. Default value is `NULL`, which equates to no selection.

- models:

  `character` value(s) of the name of the model to include. Default
  value is `NULL`, which equates to no selection with respect to
  `model`. `NULL` translates to all `models` in the table.

- dataset:

  `character` value of the rodent data set to include Default value is
  `NULL`, which equates to the first data set encountered.

- species:

  `character` vector of the species code(s) or `"total"` for the total
  across species) to be plotted `NULL` translates to the species defined
  by
  [`forecasting_species`](https://weecology.github.io/portalr/reference/rodent_species.html)
  called by
  [`prefab_species`](https://weecology.github.io/portalcasting/reference/prefabricated-rodents-datasets.md).

## Value

`data.frame` of ensembled forecasts.

## Details

A pre-loaded table of forecasts can be input, but if not (default), the
table will be efficiently (as defined by the inputs) loaded and
trimmed.  
The forecasts can be trimmed specifically using the `forecasts_ids`
input, otherwise, all relevant forecasts from the stated
`forecast_groups` will be included.

## See also

Core forecasting functions:
[`evaluate forecasts`](https://weecology.github.io/portalcasting/reference/evaluate-forecasts.md),
[`portalcast()`](https://weecology.github.io/portalcasting/reference/portalcast.md),
[`process forecast output`](https://weecology.github.io/portalcasting/reference/process-forecast-output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "ensemble")
   setup_production(main = main1)

   forecasts_ids <- select_forecasts(main     = main1, 
                                     datasets = "controls", 
                                     species  = "DM")$forecast_id

   ensemble_forecasts(main          = main1, 
                      forecasts_ids = forecasts_ids)

   unlink(main1, recursive = TRUE)
} # }
```
