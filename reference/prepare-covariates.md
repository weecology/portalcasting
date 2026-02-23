# Prepare Covariate Data for Casting

Prepare and combine the historical and forecast covariate data for a
model run, according to the
[`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).

## Usage

``` r
prepare_covariates(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

## Value

`data.frame` of historical and forecasted covariates that is also saved
out to `settings$files$covariates` if indicated by `settings$save`.

## See also

Content preparation functions:
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`prepare metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md),
[`prepare models`](https://weecology.github.io/portalcasting/reference/prepare-models.md),
[`prepare newmoons`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md),
[`prepare rodents`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "covariates")

   create_dir(main = main1)
   fill_resources(main = main1)
   fill_forecasts(main = main1)
   fill_fits(main = main1)
   fill_models(main = main1)

   prepare_newmoons(main   = main1)
   prepare_rodents(main    = main1) 
   prepare_covariates(main = main1)

   unlink(main1, recursive = TRUE)
} # }
```
