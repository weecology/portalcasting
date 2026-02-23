# Prepare a Model-Running Metadata List

Sets up the metadata list used for forecasting, in particular the
matching of time period across the datasets, according to the
[`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).

## Usage

``` r
prepare_metadata(
  main = ".",
  datasets = prefab_datasets(),
  new_datasets_controls = NULL
)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- datasets:

  `character` vector of name(s) of dataset(s) to include.

- new_datasets_controls:

  `list` of controls for any new datasets (not in the prefab datasets)
  listed in `datasets` that are to be added to the control list and
  file.

## Value

`list` of forecasting metadata, which is also saved out as a YAML file
(`.yaml`) if desired.

## See also

Content preparation functions:
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`prepare covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md),
[`prepare models`](https://weecology.github.io/portalcasting/reference/prepare-models.md),
[`prepare newmoons`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md),
[`prepare rodents`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "metadata")

   create_dir(main = main1)
   fill_resources(main = main1)
   fill_forecasts(main = main1)
   fill_fits(main = main1)
   fill_models(main = main1)

   prepare_newmoons(main   = main1)
   prepare_rodents(main    = main1) 
   prepare_covariates(main = main1)
   prepare_metadata(main   = main1)

   unlink(main1, recursive = TRUE)
} # }
```
