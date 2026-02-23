# portalcasting Codebase

This vignette outlines the codebase and functionality of the
**portalcasting** package (v0.60.6), which underlies the automated
iterative forecasting within the [Portal Predictions or forecasts
production pipeline](https://github.com/weecology/portal-forecasts).
**portalcasting** has utilities for setting up local versions of the
pipeline for developing and testing new models, which are covered in
detail in other vignettes.

## Installation

To install the most recent version of **portalcasting** from GitHub:

``` r
install.packages("remotes")
remotes::install_github("weecology/portalcasting")
```

## Directory Structure

The package uses a directory tree with two levels to organize the
project:

- `main`: project folder encompassing all content
- `subdirectories`: specific subfolders that organize the project files

structured as

    main
    │
    └──resources
    │   <stable version of resources used to populate other folders>
    └──models
    │   <model controls list>
    │   <model scripts>
    └──data
    │   <dataset control list>
    │   <rodent datasets>
    │   <covariates, newmoons, and metadata data files>
    └──forecasts
    │   <previous and current model forecasts>
    │   <casts metadata file>
    └──fits
    │   <previous and current model fits>
    └──www
    │   <ui, server, and application files>
    └──directory_configuration.yaml
    └──app.R

The `main` argument controls the location of the directory and defaults
to `"."`, the present working location. To group the project subfolders
into a multi-leveled folder, simply add structure to the `main` input,
such as `main = "~/project_folder"`.

## Instantiating a Directory

Setting up a fully functional directory for a production or sandbox
pipeline consists of two steps: creating (instantiating folders that are
missing) and filling (adding files to the folders). These steps can be
executed separately or in combination via a general
[`setup_dir()`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
function or via specialized versions of
[`setup_dir()`](https://weecology.github.io/portalcasting/reference/directory-creation.md):
`setup_sandbox`() (for creating a pipeline with defaults to facilitate
sandboxing) and
[`setup_production()`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
(for creating a production pipeline).

These functions are general and flexible, but are designed to work well
under default settings. To alter the directory configurations in
`setup_<>` and
[`create_dir()`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
use the `settings` argument, which takes a list of inputs, condensed and
detailed in
[`directory_settings()`](https://weecology.github.io/portalcasting/reference/directory-settings.md).

### Creating

The directory is established using
[`create_dir()`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
which takes `main` as an argument and in sequence creates each of the
levels’ folders if they do not already exist. A typical user is likely
to want to change the `main` input (to locate the forecasting directory
where they would like it), but general users should not alter the
`subdirectories` structure, and so that option is not directly
available. If needed, the `subdirectories` can be altered via the
[`directory_settings()`](https://weecology.github.io/portalcasting/reference/directory-settings.md)
controls.

[`create_dir()`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
also initializes the `directory_configuration.yaml` file, which is held
within `main` and contains metadata about the directory setting up
process.

### Filling

The directory is filled (loaded with files for forecasting) using a
series of subdirectory-specific functions that are combined in the
overall
[`fill_dir()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
function:

- [`fill_resources()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  downloads each of the resources for the directory, which presently
  include the source data (rodents), covariate data (weather, NDVI), and
  previous forecasts’ archive. Upon completion of the downloads,
  [`fill_resources()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  updates `directory_configuration.yaml` with downloaded versions.
- [`fill_forecasts()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  moves the existing model forecast output files from the `resources`
  subdirectory to the `forecasts` subdirectory.
- [`fill_fits()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  moves the existing model fit files from the `resource` subdirectory to
  the `fits` subdirectory.
- [`fill_models()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  writes the model controls list and scripts into the `models`
  subdirectory.
- [`fill_data()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  prepares the forecasting data files from the `resources` downloaded
  data files and moves them into the `data` subdirectory.
  - [`prepare_newmoons()`](https://weecology.github.io/portalcasting/reference/prepare-newmoons.md)
    prepares and formats the temporal (lunar) data from the raw data.
  - [`prepare_rodents()`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md)
    prepares multiple structures of the rodents data for analyses from
    the raw data.
  - [`prepare_covariates()`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md)
    downloads and forecasts covariates data.
  - [`prepare_metadata()`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md)
    creates and saves out a YAML metadata list for the forecasting
    configurations.
- [`fill_app()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
  moves the app-building files into the directory and renders components
  based on local content.

Each of these components can be run individually, as well. For example,
[`fill_data()`](https://weecology.github.io/portalcasting/reference/directory-filling.md)
can be used to set up the complete set of data for a given model run.

### Updating

The directory is updated (loaded with any out of date resources and
re-filling data) using the `update_dir` function, which provides an
update-flavored implementation of the core functions.

## Running models

Models are run using a function pipeline similar to the creation and
filling function pipelines, with flexible controls through a variety of
arguments, but robust operation under default settings.

- [`portalcast()`](https://weecology.github.io/portalcasting/reference/portalcast.md)
  is the overarching function that controls forecasting of the Portal
  data
  - [`make_model_combinations()`](https://weecology.github.io/portalcasting/reference/portalcast.md)
    takes the input arguments and available components and produces a
    data frame of model run combinations (model - dataset - species).
  - [`cast()`](https://weecology.github.io/portalcasting/reference/portalcast.md)
    runs (“casts”) each of the model combinations using the `fit` and
    `cast` functions described in the model controls list.

## Data IO

**portalcasting** has a generalized
[`read_data()`](https://weecology.github.io/portalcasting/reference/read-write-data.md)
function that allows for toggling among
[`read_rodents()`](https://weecology.github.io/portalcasting/reference/read-write-data.md),
[`read_rodents_dataset()`](https://weecology.github.io/portalcasting/reference/read-write-data.md),
[`read_covariates()`](https://weecology.github.io/portalcasting/reference/read-write-data.md),
[`read_newmoons()`](https://weecology.github.io/portalcasting/reference/read-write-data.md),
and
[`read_metadata()`](https://weecology.github.io/portalcasting/reference/read-write-data.md),
which each have specific loading procedures in place. Similar to the
[`read_data()`](https://weecology.github.io/portalcasting/reference/read-write-data.md)
functions, `read_forecasts()` provides a simple user interface for
reading the forecast files into the R session.

For saving out,
[`write_data()`](https://weecology.github.io/portalcasting/reference/read-write-data.md)
provides a simple means for interfacing with potentially pre-existing
data files, with logical inputs for saving generally and overwriting a
pre-existing file specifically, and flexible file naming. The type of
data saved out is currently restricted to `.csv` `.json`, and `.yaml`,
which is extracted from the filename given.

The directory configuration file is a special file, and has its own IO
functions separate from the rest:
[`write_directory_configuration()`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md)
creates the file (from within
[`create_dir()`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
[`update_directory_configuration()`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md)
adds downloads information from inside
[`fill_resources()`](https://weecology.github.io/portalcasting/reference/directory-filling.md))
and
[`read_directory_configuration()`](https://weecology.github.io/portalcasting/reference/directory-configuration-file.md)
brings the information from the file into the R session. Reading the
configuration file into R is also the means by which directory settings
are passed among functions (to limit clashing arguments and reduce
verbosity).

## Utilities

To facilitate tidy and easy-to-follow code, we introduce a few important
utility functions, which are put to use throughout the codebase.

### (Rodent) data interpolating

[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md)
combines the `round`, `na.interp`, and `pmax` functions to provide a
single-function for interpolating to biologically reasonable values.

### File paths

[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md)
determines the file extension, based on the separating character
(`sep_char`), which facilitates use with generalized URL APIs.

### Messaging

[`messageq()`](https://weecology.github.io/portalcasting/reference/messages.md)
provides a simple wrapper on `message` that also has a logical input for
quieting. This helps switch messaging off as desired while localizing
the actual boolean operator code to one spot.
[`break_line()`](https://weecology.github.io/portalcasting/reference/messages.md)
makes a single horizontal breaking line, `break_lines` makes multiple
`break_line`s, and `castle` makes a castle character element, all for
use in `messageq`.

### Time

[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md)
calculates the fraction of year of a date.
