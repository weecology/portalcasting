# Fill a Portalcasting Directory with Basic Components

Fill the directory with components including: \* Resources
(`fill_resources`) \* raw data
([`download_observations`](https://weecology.github.io/portalr/reference/download_observations.html))
\* directory archive
([`download_archive`](https://weecology.github.io/portalcasting/reference/download-archive.md))
\* climate forecasts
([`download_climate_forecasts`](https://weecology.github.io/portalcasting/reference/download-climate-forecasts.md))
\* Output \* forecasts (`fill_forecasts`) \* model fits (`fill_fits`) \*
Data (`fill_data`) \* rodent datasets
([`prepare_rodents`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md))
\* temporal (lunar) data
([`prepare_newmoons`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md))
\* covariates
([`prepare_covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md))
\* metadata
([`prepare_metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md))
\* Models (`fill_models`) \* models controls
([`write_models_controls`](https://weecology.github.io/portalcasting/reference/prepare-models.md))
\* models scripts (if needed)
([`write_models_scripts`](https://weecology.github.io/portalcasting/reference/prepare-models.md))
\* Web Application (`fill_app`) \* transfers app files from package to
main \* renders
([`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html))
and sources ([`source`](https://rdrr.io/r/base/source.html)) files into
HTML.  
  
Additionally, new models and datasets can be added to the directory at
filling using the optional arguments `new_models_controls` and
`new_datasets_controls`, but the model or dataset must still be listed
in its respective main argument, as well.

## Usage

``` r
fill_dir(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  new_datasets_controls = NULL,
  new_models_controls = NULL
)

fill_data(
  main = ".",
  datasets = prefab_datasets(),
  new_datasets_controls = NULL
)

fill_app(main = ".")

fill_resources(main = ".")

fill_forecasts(main = ".")

fill_fits(main = ".")

fill_models(main = ".", models = prefab_models(), new_models_controls = NULL)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- models:

  `character` vector of name(s) of model(s) to include. Defaults to
  [`prefab_models`](https://weecology.github.io/portalcasting/reference/prefabricated-models.md).
  If controls are provided in `new_models_controls`, the model still
  needs to be named here to be included.

- datasets:

  `character` vector of name(s) of rodent dataset(s) to be created.
  Defaults to
  [`prefab_datasets`](https://weecology.github.io/portalcasting/reference/prefabricated-rodents-datasets.md).
  If controls are provided in `new_datasets_controls`, the dataset still
  needs to be named here to be included.

- new_datasets_controls:

  Optional named `list` of controls for new datasets. See
  [`datasets_controls`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md).

- new_models_controls:

  Optional named `list` of controls for new models. See
  [`models_controls`](https://weecology.github.io/portalcasting/reference/prepare-models.md).

## Value

`NULL`, [`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## See also

Directory orchestration functions:
[`directory configuration file`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md),
[`directory creation`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
[`directory paths`](https://weecology.github.io/portalcasting/reference/directory-paths.md),
[`directory settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md)

Content preparation functions:
[`prepare covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md),
[`prepare metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md),
[`prepare models`](https://weecology.github.io/portalcasting/reference/prepare-models.md),
[`prepare newmoons`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md),
[`prepare rodents`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "fill_standard")
   main2 <- file.path(tempdir(), "fill_production")

   create_dir(main = main1)
   fill_dir(main = main1)

   create_dir(main = main2, settings = production_settings())
   fill_resources(main = main2)
   fill_forecasts(main = main2)
   fill_fits(main = main2)
   fill_models(main = main2)
   fill_data(main = main2)
   fill_app(main = main2)

   unlink(main1, recursive = TRUE)
   unlink(main2, recursive = TRUE)
} # }
```
