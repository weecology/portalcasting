# Forecast Portal Rodents Models

Forecast Portal rodent populations using the data and models in a
portalcasting directory.  
  
`portalcast` wraps around `cast` to allow multiple runs of model -
dataset - species combinations. It returns and saves out the model
combinations table with fit success added as a column.  
`cast` runs a single forecast of a single model on one species of one
dataset.  
`make_model_combinations` translates model controls into a `data.frame`
of model, dataset, and species columns, with a row for each combination.

## Usage

``` r
portalcast(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  species = prefab_species(main = main)
)

cast(main = ".", dataset, species, model)

make_model_combinations(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  species = prefab_species(main = main)
)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- models, model:

  `character` vector of name(s) of model(s) to include in the forecast.
  In `cast`, `model` can only be length-one.

- datasets, dataset:

  `character` vector of datasets to be forecast. In `cast`, `dataset`
  can only be length-one.

- species:

  `character` vector of species to be forecast. In `cast`, `species` can
  only be length-one. See
  [`rodent_species`](https://weecology.github.io/portalr/reference/rodent_species.html).

## Value

`portalcast`: `data.frame` of model combinations with a `logical` column
added for fit success,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.  
`cast`: `list` of model outputs from
[`process_model_output`](https://weecology.github.io/portalcasting/reference/process-forecast-output.md).  
`make_model_combinations`: `data.frame` of the model combinations.

## See also

Core forecasting functions:
[`ensemble`](https://weecology.github.io/portalcasting/reference/ensemble.md),
[`evaluate forecasts`](https://weecology.github.io/portalcasting/reference/evaluate-forecasts.md),
[`process forecast output`](https://weecology.github.io/portalcasting/reference/process-forecast-output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "portalcast")
   setup_dir(main = main1)

   make_model_combinations(main = main1)
   portalcast(main = main1, models = "AutoArima")
   cast(main = main1, model = "AutoArima", dataset = "controls", species = "DM")

   unlink(main1, recursive = TRUE)
} # }
```
