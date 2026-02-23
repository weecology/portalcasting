# Create or Update the Structure of a Directory and Fill It with Content or Update an Existing Directory

Instantiates the necessary folder structure for a directory, writes the
setup configuration file, and fills the directory with content.  
Options for pre-defined setups include `setup_sandbox` for quick and
flexible builds and `setup_production` for robust, rigid builds, as
defined in
[`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).  
`update_dir` updates an existing directory and `update_sandbox` and
`update_production` are companions akin to their `setup_<>` functions.

## Usage

``` r
create_dir(
  main = ".",
  settings = directory_settings(),
  quiet = FALSE,
  verbose = FALSE
)

update_dir(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  new_datasets_controls = NULL,
  new_models_controls = NULL,
  settings = directory_settings(),
  quiet = FALSE,
  verbose = FALSE
)

update_production(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  settings = production_settings(),
  quiet = FALSE,
  verbose = TRUE
)

update_sandbox(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  new_datasets_controls = NULL,
  new_models_controls = NULL,
  settings = sandbox_settings(),
  quiet = FALSE,
  verbose = FALSE
)

setup_dir(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  new_datasets_controls = NULL,
  new_models_controls = NULL,
  settings = directory_settings(),
  quiet = FALSE,
  verbose = FALSE
)

setup_production(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  settings = production_settings(),
  quiet = FALSE,
  verbose = TRUE
)

setup_sandbox(
  main = ".",
  models = prefab_models(),
  datasets = prefab_datasets(),
  new_datasets_controls = NULL,
  new_models_controls = NULL,
  settings = sandbox_settings(),
  quiet = FALSE,
  verbose = FALSE
)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree. Default value (`"."`) roots the directory in the present
  location.

- settings:

  `list` of controls for the directory, with defaults set in
  [`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).

- quiet:

  `logical` indicator if progress messages should be quieted.

- verbose:

  `logical` indicator of whether or not to produce all of the messages.

- models:

  `character` vector of name(s) of model(s) to include.

- datasets:

  `character` vector of name(s) of rodent dataset(s) to be created.

- new_datasets_controls:

  Optional `list` of controls for new datasets. See
  [`datasets_controls`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md).
  This argument is not available in `setup_production`.

- new_models_controls:

  Optional `list` of controls for new models. See
  [`models_controls`](https://weecology.github.io/portalcasting/reference/prepare-models.md).
  This argument is not available in `setup_production`.

## Value

The `list` of directory settings
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## See also

Directory orchestration functions:
[`directory configuration file`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md),
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`directory paths`](https://weecology.github.io/portalcasting/reference/directory-paths.md),
[`directory settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "standard")
   main2 <- file.path(tempdir(), "sandbox")
   main3 <- file.path(tempdir(), "production")

   setup_dir(main = main1)
   setup_sandbox(main = main2)
   setup_production(main = main3)

   update_dir(main = main1)

   unlink(main1, recursive = TRUE)
   unlink(main2, recursive = TRUE)
   unlink(main3, recursive = TRUE)
} # }
```
