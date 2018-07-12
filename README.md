# portalcasting: the support package for [Portal Predictions](https://github.com/weecology/portalPredictions)
[![Build Status](https://travis-ci.org/weecology/portalcasting.svg?branch=master)](https://travis-ci.org/weecology/portalcasting)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)

## Overview

The `portalcasting` package contains the functions used for Continuous
Forecasting of Rodent Populations (Portal Predictions: 
[code repo](https://github.com/weecology/portalPredictions);
[website](http://portal.naturecast.org/)).

The repo is currently ***in development*** as the functions are migrated over.

## Installation

You can install the package from github with:

```
install.packages("devtools")
devtools::install_github("weecology/portalcasting")
```

## Usage

The `setup_dir()` function will create and populate a standard portalcasting
directory that includes `data`, `models`, `PortalData`, `predictions` and
`tmp` subdirectories. By default, `setup_dir()` downloads the most up-to-date
version of the [Portal Data Repository](https://github.com/weecology/PortalData)
into the `PortalData` subdirectory, prepares the moons, rodents, covariates,
and metadata data files (and places them in the `data` subdirectory), copies the
historic covariate forecast data file available in the package 
(`/inst/extdata/covariate_forecasts.csv`) into the `data` subdirectory, 
downloads the most recent set of model forecasts into the `predictions` 
subdirectory and populates the `models` subdirectory with scripts for the four
existing models ("AutoArima", "ESSS", "nbGARCH", and "pevGARCH").

The `portalcast()` function controls the running of potentially multiple 
models for either a forecast or a hindcast. It will prepare the data 
(rodents, covariates, model metadata) as needed, verify that the requested
models are available to run, run the models, compile the output, and
generate an ensemble (if desired).

All required inputs to both `setup_dir()` and `portalcast()` are encompassed 
within the singular argument `options_all`, which is a hierarchical list of
options settings, defined through the options-settting function `all_options`
and the default values are appropriate for basic forecast usage:

```
setup_dir()
portalcast()
```

There are many options available for the user to control, and a full list
can be found by running `?all_options`, to return the help file for the 
function that generates the hierarchical options list. 

## Options List

The options list is a hierarchical list containing a suite of inter-linked
options controlling the structure, content, and usage of the directory.

- `options_all`: created by `all_options` 
  - `options_dir`: created by `dir_options`
  - `options_data`: created by `data_options`
    - `moons`: created by `moons_options`
    - `rodents`: created by `rodents_options`
    - `covariates`: created by `covariates_options`
    - `metadata`: created by `metadata_options`
    - additionally includes `cast_type`, `quiet`, and `tree` options
  - `options_predictions`: created by `predictions_options`
  - `options_models`: created by `models_options`
  - `options_casts`: created by `casts_options` 

## Directory Tree

The package uses a simple directory tree to organize the project. There are 
three levels to the hierarchy:

- `base`: existing folder where the project folder will be housed
  - `main`: project folder encompassing all subfolders
    - `subs`: specific sub-folders for the project
 
The tree is housed in a simple list, created by the `dirtree` function, which 
allows for the renaming of all levels, although it is only advisable to alter 
the `base` or `main` levels, as many functions require specifically-named
subdirectories.

## Function Tree

The package organizes the main actions into hierarchical functions

- `setup_dir`
  - `create_dir`
    - `create_main_dir`
    - `create_sub_dirs`
      - `create_sub_dir`
  - `fill_dir`
    - `fill_PortalData`
	  - `download_observations`
    - `fill_data`
      - `transfer_hist_covariate_forecasts`
      - `transfer_trapping_table`
	  - `prep_moons`
	  - `prep_rodents`
	  - `prep_covariates`
	  - `prep_metadata`
	- `fill predictions`
	- `fill_models`
	  - `write_model`

- `portalcast`
  - `clear_tmp`
  - `verify_models`
  - `prep_data`
  - `casts`
    - `cast`
      - `check_to_skip`
      - `cast_models`
      - `combine_forecasts`	  
      - `add_ensemble`
      - `clear_tmp`
	  - `step_casts`
        - `step_hind_forward`
        - `update_data`
          - `prep_moons`
          - `update_rodents`
          - `update_covariates`
          - `prep_metadata`
        - `cast`	
          - `check_to_skip`
          - `cast_models`
          - `combine_forecasts`	  
          - `add_ensemble`
          - `clear_tmp`
