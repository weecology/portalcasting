# Evaluate Forecasts

Evaluate forecasts in the directory, based on id(s).  
  
Current metrics include raw error (which can be used to calculate root
mean squared error; RMSE), coverage, log score, and continuous rank
probability score (CRPS).  
`read_forecasts_evaluations` read in the forecasts evaluations file.

## Usage

``` r
evaluate_forecasts(main = ".", forecasts_ids = NULL)

evaluate_forecast(main = ".", forecast_id = NULL)

read_forecasts_evaluations(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- forecast_id, forecasts_ids:

  `integer` (or integer `numeric`) value(s) representing the forecasts
  of interest for evaluating, as indexed within the `forecasts`
  subdirectory. See the forecasts metadata file
  (`forecasts_metadata.csv`) for summary information.  
  `forecast_id` can only be a single value, whereas `forecasts_ids` can
  be multiple.

## Value

A `data.frame` of all forecast evaluations at the observation (newmoon)
level, as requested,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## See also

Core forecasting functions:
[`ensemble`](https://weecology.github.io/portalcasting/reference/ensemble.md),
[`portalcast()`](https://weecology.github.io/portalcasting/reference/portalcast.md),
[`process forecast output`](https://weecology.github.io/portalcasting/reference/process-forecast-output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "evaluations")
   setup_dir(main = main1)

   plot_covariates(main = main1)

   make_model_combinations(main = main1)

   portalcast(main   = main1, 
              models = "AutoArima")

   cast(main    = main1,
        model   = "AutoArima", 
        dataset = "controls", 
        species = "DM")

   ## evaluate_forecasts(main = main1) ## extensive runtime for full evaluation from scratch

   ids <- select_forecasts(main = main1)$forecast_id
        
   evaluate_forecast(main        = main1, 
                     forecast_id = ids[1])

   read_forecasts_evaluations(main = main1)

   unlink(main1, recursive = TRUE)
} # }
```
