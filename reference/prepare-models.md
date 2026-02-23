# Prepare Portalcasting Models

Add model controls and scripts to the portalcasting directory and read
in controls and model names.

## Usage

``` r
write_models_controls(
  main = ".",
  new_models_controls = NULL,
  models = prefab_models()
)

write_models_scripts(main = ".", controls = prefab_models_controls())

read_models_controls(main = ".")

models_controls(main = ".", models = NULL)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- new_models_controls:

  `list` of controls for any new models (not in the prefab models)
  listed in `models` that are to be added to the control list and file.

- models:

  `character` vector of name(s) of model(s) to include.

- controls:

  `list` of controls for the models.

## Value

`model_controls`: `list` of `models`' control `list`s,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.  
`read_models_controls`: `list` of all `models`' control `list`s, from
the file defined in
[`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md),
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.  
`write_models_controls`: `list` of `models`' control `list`s,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.  
`write_models_scripts`: `NULL`,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## See also

Content preparation functions:
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`prepare covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md),
[`prepare metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md),
[`prepare newmoons`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md),
[`prepare rodents`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "models")

   create_dir(main = main1)
   fill_resources(main = main1)
   fill_forecasts(main = main1)
   fill_fits(main = main1)

   controls <- write_models_controls(main = main1)
   write_models_scripts(main     = main1, 
                      controls = controls)

   unlink(main1, recursive = TRUE)
} # }
```
