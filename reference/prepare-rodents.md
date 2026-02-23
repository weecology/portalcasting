# Prepare Rodents Data for Forecasting

`prepare_dataset` is the workhorse function for creating portalcasting
rodent datasets using existing functions.  
Wraps around
[`portalr::summarize_rodent_data`](https://weecology.github.io/portalr/reference/summarize_rodent_data.html)
to produce a `data.frame` associated with a set of data specifications.
Inputs are ready for implementation via `prepare_rodents`.  
`prepare_rodents` creates specified `datasets` using their associated
function (typically `prepare_dataset`) and arguments, according to the
[`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).  
`prepare_abundance` creates a model-ready vector of abundances for
fitting and casting, according to the model's requirements and time
settings.

## Usage

``` r
prepare_abundance(main = ".", dataset = NULL, species = NULL, model = NULL)

prepare_rodents(
  main = ".",
  datasets = prefab_datasets(),
  new_datasets_controls = NULL
)

prepare_dataset(
  name = "all",
  main = ".",
  filename = "rodents_all.csv",
  clean = FALSE,
  level = "Site",
  type = "Rodents",
  plots = "all",
  unknowns = FALSE,
  shape = "crosstab",
  time = "newmoon",
  output = "abundance",
  fillweight = FALSE,
  treatment = NULL,
  na_drop = FALSE,
  zero_drop = FALSE,
  min_traps = 1,
  min_plots = 24,
  effort = TRUE,
  species = prefab_species(main = main),
  total = TRUE,
  save = TRUE
)

read_datasets_controls(main = ".")

datasets_controls(main = ".", datasets = NULL)

write_datasets_controls(
  main = ".",
  new_datasets_controls = NULL,
  datasets = prefab_datasets()
)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- dataset, datasets:

  `character` value(s) of name(s) of rodent dataset(s) to include.

- species:

  `character`-valued vector of species names to include.

- model:

  `character` value of the model name.

- new_datasets_controls:

  Optional `list` of controls for new datasets. See `datasets_controls`.

- name:

  `character` name to be given to the dataset.

- filename:

  `character` value of the file for saving the output.

- clean:

  `logical` indicator of if only the rodent data that passed QA/QC
  (`clean = TRUE`) or if all data (`clean = FALSE`) should be loaded.

- level:

  `character` indicating the type of summary: `"Plot"`, `"Treatment"`,
  or `"Site"`. Pipes directly to
  [`portalr::summarize_rodent_data`](https://weecology.github.io/portalr/reference/summarize_rodent_data.html).

- type:

  `character` value of the rodent data set type, according to
  pre-existing definitions. An alternative toggle to `species`.  
    
  Either all species (`type = "Rodents"`) or only granivoes
  (`type = "Granivores"`).

- plots:

  Specification of subset of plots. Can be a vector of `numeric` plots
  indicators or specific sets indicated by `character` values: `"all"`
  plots or `"Longterm"` plots (plots that have had the same treatment
  for the entire time series).

- unknowns:

  `logical` indicator to either remove all individuals not identified to
  species (`unknowns = FALSE`) or sum them in an additional column
  (`unknowns = TRUE`.

- shape:

  `character` value indicating a "crosstab" or "flat" output.

- time:

  `character` value specifying the format of the time index in the
  output. Options are `"period"` (sequential Portal surveys),
  `"newmoon"` (lunar cycle numbering), and `"date"` (calendar date).  
  The default `time = "newmoon"` produces an equispaced observation
  timestep, a common format format for discrete-time modeling.

- output:

  `character` indicating the type of data: `"abundance"`, `"biomass"`,
  or `"energy"`.

- fillweight:

  `logical` specifier of whether to fill in unknown weights with other
  records from that individual or species, where possible.

- treatment:

  `character` indicating the specific treatment(s) to trim to if
  `level = "Treatment"`: `"control"`, `"exclosure"`, `"removal"`, or
  `"spectabs"`

- na_drop:

  `logical` indicator of if `NA` values (representing insufficient
  sampling) should be dropped.

- zero_drop:

  `logical` indicator of if `0` values (representing sufficient sampling
  but no detection) should be dropped.

- min_traps:

  `integer` (or integer `numeric`) of the minimum number of traps
  collected for a plot to be used.

- min_plots:

  `integer` (or integer `numeric`) of the minimum number of plots
  surveyed for a survey to be used.

- effort:

  `logical` indicator of if the effort columns should be included in the
  output.

- total:

  `logical` value indicating if a total (sum across species should be
  added or not. Only available if more than one species is included.

- save:

  `logical` indicator controlling if the output should be saved out.

## Value

`prepare_dataset`: `data.frame` for the specified dataset.  
`prepare_rodents`: `list` of `data.frame`s for the specified datasets.  
`prepare_abundance`: `numeric` vector of abundance data corresponding to
the time articulated in the metadata file. Missing values are
interpolated if requested via the model controls.  
`read_datasets_controls`, `write_datasets_controls`,
`datasets_controls`: `list` of `datasets`' control `list`s,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly for
`write_datasets_controls`.

## See also

Content preparation functions:
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`prepare covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md),
[`prepare metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md),
[`prepare models`](https://weecology.github.io/portalcasting/reference/prepare-models.md),
[`prepare newmoons`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "rodents")

   create_dir(main = main1)
   fill_resources(main = main1)
   fill_forecasts(main = main1)
   fill_fits(main = main1)
   fill_models(main = main1)

   prepare_newmoons(main   = main1)
   prepare_rodents(main    = main1) 

   write_datasets_controls(main = main1)
   read_datasets_controls(main = main1)
   datasets_controls(main = main1)

   unlink(main1, recursive = TRUE)
} # }
```
