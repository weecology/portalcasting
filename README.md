# portalcasting: the support package for Portal Predictions
[![Build Status](https://travis-ci.org/weecology/portalcasting.svg?branch=master)](https://travis-ci.org/weecology/portalcasting)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)

## Overview

The `portalcasting` package contains the functions used for Continuous
Forecasting of Rodent Populations (Portal Predictions: 
[code repo](https://github.com/weecology/portalPredictions);
[website](http://portal.naturecast.org/)).

The repo currently ***in development*** as the functions are migrated over.

## Installation

You can install the package from github with:

```
install.packages("devtools")
devtools::install_github("weecology/portalcasting")
```

## Usage

The `setup_portalcast_dir()` function will download the most up-to-date version
of the [Portal Data Repository](https://github.com/weecology/PortalData)
and create and populate a standard portalcasting directory that includes 
`models`, `data`, and `predictions` subdirectories. By default, the
`models` subdirectory will be populated with four **R** scripts corresponding
to the existing base models (`autoarima.R`, `esss.R`, `nbgarch.R` and 
`pevgarch.R`). 

The `portalcast()` function controls the running of potentially multiple 
models for either a forecast or a hindcast. It will prepare the data 
(rodents, covariates, model metadata) as needed, verify that the requested
models are available to run, run the models, compile the output, and
generate an ensemble (if desired).

All required arguments to both `setup_portalcast_dir()`and `portalcast()` are
defined and the default values are appropriate for basic usage:

```
setup_portalcast_dir()
portalcast()
```
