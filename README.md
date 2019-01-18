# Supporting iterative forecasting of Portal rodent populations
[![Build Status](https://travis-ci.org/weecology/portalcasting.svg?branch=master)](https://travis-ci.org/weecology/portalcasting)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Overview

The **portalcasting** package contains the functions used for Continuous
Forecasting of Rodent Populations (Portal Predictions: 
[code repo](https://github.com/weecology/portalPredictions);
[website](http://portal.naturecast.org/)).

## Status: Deployed, In Development

This package is currently ***in development*** by the 
[Weecology Team](https://www.weecology.org). Most of the code underlying the 
forecasting functionality has been migrated over from the 
[Portal Predictions repo](https://github.com/weecology/portalPredictions),
although output (website) generation functionality is still housed there.
Coincidingly, the package is deployed for use within the 
[Portal Predictions repo](https://github.com/weecology/portalPredictions).

The API is moderately well defined at this point, but is still evolving 
and may change substantially. 


## Installation

You can install the package from github with:

```
install.packages("devtools")
devtools::install_github("weecology/portalcasting")
```

## Usage

The `setup_dir()` function creates and populates a standard portalcasting
directory within the present location (by default) that includes `data`,
`models`, `PortalData`, `predictions` and `tmp` subdirectories. By default,
`setup_dir()` downloads the most up-to-date version of the 
[Portal Data from Zenodo](https://zenodo.org/record/2541170)
into the `PortalData` subdirectory, prepares the moons, rodents, covariates,
and metadata data files (and places them in the `data` subdirectory), copies 
the historic covariate forecast data file available in the package 
(`/inst/extdata/covariate_forecasts.csv`) into the `data` subdirectory, 
downloads the most recent set of model forecasts into the `predictions` 
subdirectory and populates the `models` subdirectory with scripts for the four
existing models ("AutoArima", "ESSS", "nbGARCH", and "pevGARCH").

The `portalcast()` function controls the running of potentially multiple 
models for either a forecast or a hindcast. It will prepare the data 
(rodents, covariates, model metadata) as needed, verify that the requested
models are available to run, run the models, compile the output, and
generate an ensemble (if desired). Presently, the preloaded model set includes
[four models](https://weecology.github.io/portalcasting/articles/models.html):
ESSS, AutoArima, nbGARCH, and pevGARCH. 

The `cleanup_dir()` function simply removes the temporary files and folders
from the directory.

All required inputs to `setup_dir()`, `portalcast()`, and `cleanup_dir()` 
are encompassed within the singular argument `options_all`, which is a 
hierarchical list of options settings, defined through the options-settting 
function `all_options()` and the default values are appropriate for basic 
forecast usage:

```
setup_dir()
portalcast()
cleanup_dir()
```

There are many options available for the user to control, and a full list
can be found by running `?all_options`, to return the help file for the 
function that generates the hierarchical options list. 

For further information about the **portalcasting** codebase see the 
[vignette](https://weecology.github.io/portalcasting/articles/codebase.html).


## Acknowledgements 

The motivating study—the Portal Project—has been funded nearly continuously 
since 1977 by the [National Science Foundation](http://nsf.gov/), most recently
by [DEB-1622425](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622425) 
to S. K. M. Ernest. Much of the computational work was supported by the 
[Gordon and Betty Moore Foundation’s Data-Driven Discovery 
Initiative](http://www.moore.org/programs/science/data-driven-discovery) 
through [Grant GBMF4563](http://www.moore.org/grants/list/GBMF4563) to E. P. 
White. We thank Henry Senyondo for help with continuous integration, Hao
Ye for feedback on documents and code, Heather Bradley for logistical 
support, John Abatzoglou for assistance with climate forecasts, and James
Brown for establishing the Portal Project.

## Author Contributions

All authors conceived the ideas, designed methodology, and developed the 
automated forecasting system. JLS is leading the transition of code from
the [Portal Predictions repo](https://github.com/weecology/portalPredictions)
to **portalcasting**. 
