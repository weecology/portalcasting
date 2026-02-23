# Create a List of Full Directory Paths

Upon creation (or updating) of the directory, all the standard file and
subdirectory paths are set based on
[`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).  
`paths` produces the full path `list`, whose contents can then also be
accessed with specialized functions, see `Details`.  
There is also a special function for the shiny application location,
which could either be referencing the file or folder.

## Usage

``` r
paths(main = ".")

models_rmd_path(main = ".")

models_html_path(main = ".")

about_md_path(main = ".")

rodents_profiles_html_path(main = ".")

rodents_profiles_csv_path(main = ".")

rodents_datasets_controls_path(main = ".")

models_controls_path(main = ".")

rodents_dataset_path(main = ".", dataset = "all")

species_names_path(main = ".")

climate_forecasts_paths(main = ".")

forecasts_metadata_path(main = ".")

forecasts_evaluations_path(main = ".")

forecasts_results_path(main = ".")

newmoons_path(main = ".")

covariates_path(main = ".")

metadata_path(main = ".")

www_path(main = ".")

app_paths(main = ".")

data_path(main = ".")

forecasts_path(main = ".")

fits_path(main = ".")

models_path(main = ".")

resources_path(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- dataset:

  `character` value of name of rodent dataset.

## Value

`list` of directory paths or specific `character` paths.

## Details

Wrapper functions for specific subdirectories and files include:

- Files

  - `rodents_dataset_path`

  - `species_names_path`

  - `climate_forecasts_paths`

  - `forecasts_metadata_path`

  - `forecasts_evaluations_path`

  - `forecasts_results_path`

  - `newmoons_path`

  - `covariates_path`

  - `metadata_path`

  - `models_controls_path`

  - `models_rmd_path`

  - `rodents_profiles_html_path`

  - `rodents_profiles_csv_path`

- Subdirectories

  - `www_path`

  - `data_path`

  - `forecasts_path`

  - `fits_path`

  - `models_path`

- Shiny Application

  - `app_paths`

## See also

Directory orchestration functions:
[`directory configuration file`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md),
[`directory creation`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`directory settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "standard")
   create_dir(main = main1)

   paths(main = main1)

   newmoons_path(main = main1)

   species_names_path(main = main1)
   rodents_dataset_path(main = main1)
   rodents_datasets_paths(main = main1)

   covariates_path(main = main1)
   climate_forecasts_paths(main = main1)

   metadata_path(main = main1)

   forecasts_metadata_path(main = main1)
   forecasts_evaluations_path(main = main1)
   forecasts_results_path(main = main1)

   models_controls_path(main = main1)

   models_rmd_path(main = main1)
   models_html_path(main = main1)
   about_md_path(main = main1)
   rodents_profiles_html_path(main = main1)
   rodents_profiles_csv_path(main = main1)

   app_paths(main = main1)

   www_path(main = main1)
   data_path(main = main1)
   forecasts_path(main = main1)
   fits_path(main = main1)
   models_path(main = main1)
   resources_path(main = main1)
} # }
```
