# Read from and Write to a Data File

Generalized data input-output functionality with specifics for common
files.  
  
`write_data` saves inputted data out to a data file if requested and
returns it to the console,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly. Currently
available for `yaml`, `csv`, and `json` file extensions.  
`read_data` reads in a specified data file. Specific functions available
include `read_rodents`, `read_rodents_dataset`, `read_covariates`,
`read_climate_forecasts`, `read_newmoons`, and `read_metadata`.

## Usage

``` r
write_data(
  x = NULL,
  main = ".",
  subdirectory = "data",
  save = TRUE,
  overwrite = TRUE,
  filename = NULL,
  quiet = FALSE
)

read_data(
  main = ".",
  data_name = NULL,
  dataset = "all",
  datasets = prefab_datasets()
)

read_rodents_dataset(main = ".", dataset = "all")

read_rodents(main = ".", datasets = prefab_datasets())

read_newmoons(main = ".")

read_covariates(main = ".")

read_metadata(main = ".")

read_climate_forecasts(main = ".")
```

## Arguments

- x:

  Data, such as a `data.frame` or `list`, to be written out.

- main:

  `character` value of the name of the main component of the directory
  tree.

- subdirectory:

  `character` value defining the data subdirectory of the portalcasting
  directory tree.

- save:

  `logical` indicator controlling if `x` should be saved out.

- overwrite:

  `logical` indicator of whether or not file writing should occur even
  if a local copy already exists.

- filename:

  `character` name of the file for saving `x`.

- quiet:

  `logical` indicator if messages should be quieted.

- data_name:

  `character` representation of the data needed. Current options include
  `"rodents"`, `"rodents_table"`, `"covariates"`, `"climate_forecasts"`,
  `"newmoons"`, and `"metadata"`.

- dataset, datasets:

  `character` of the grouping name(s) used to define the rodents.
  Standard options are `"all"`, `"controls"`, and `"exclosures"`.
  `dataset` can only be length 1, `datasets` is not restricted in
  length.

## Value

`write_data`: `x` as input,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.  
`read_data`: data requested, typically as a `data.frame` or `list`.

## See also

File read-write functions:
[`directory configuration file`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md)

## Examples

``` r
 
 if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "io")
   setup_dir(main = main1)

   write_data(main     = main1, 
              x        = data.frame(rnorm(10)), 
              filename = "xx.csv")

   read_data(main = main1)
   read_rodents(main = main1)
   read_rodents_dataset(main = main1)
   read_covariates(main = main1)
   read_climate_forecasts(main = main1)
   read_newmoons(main = main1)
   read_metadata(main = main1)

   unlink(main1, force = TRUE, recursive = TRUE)
 } # }
```
