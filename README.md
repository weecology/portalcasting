# portalcasting: the support package for Portal Predictions
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

All required arguments to both `setup_dir()` and `portalcast()` are defined 
and the default values are appropriate for basic forecast usage:

```
setup_dir()
portalcast()
```
