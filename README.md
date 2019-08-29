# Supporting [automated forecasting](https://github.com/weecology/portalPredictions) of [rodent populations](https://portal.weecology.org/)
[![Build Status](https://travis-ci.org/weecology/portalcasting.svg?branch=master)](https://travis-ci.org/weecology/portalcasting)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalPredictions/master/LICENSE)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/weecology/portalcasting/master.svg)](https://codecov.io/github/weecology/portalcasting/branch/master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3332973.svg)](https://doi.org/10.5281/zenodo.3332973)

## Overview

The `portalcasting` package contains the functions used for continuous
analysis and forecasting of [Portal rodent populations](https://portal.weecology.org/) 
([code repository](https://github.com/weecology/portalPredictions),
[output website](http://portal.naturecast.org/),
[Zenodo archive](https://zenodo.org/record/2543733)).

`portalcasting`'s functions are also portable, allowing
users to set up a fully-functional replica repository on a local or 
remote machine. This facilitates development and testing of new models
via a [sandbox](https://en.wikipedia.org/wiki/Sandbox_(software_development))
approach. 

## Status: Deployed, Active Development

The `portalcasting` package is deployed for use within the [Portal Predictions
repository](https://github.com/weecology/portalPredictions), providing
the underlying R code to populate the directory with up-to-date data,
analyze the data, produce new forecasts, generate new output figures,
and render a new version of the [website](http://portal.naturecast.org/).
All of the code underlying the forecasting functionality has been migrated 
over from the [predictions repository](https://github.com/weecology/portalPredictions),
which contains the code executed by the continuous integration.
Having relocated the code here, the `portalcasting` package is the location 
for active development of the model set and additional functionality. 

The current master branch code is not necessarily always being executed within 
the [predictions repository](https://github.com/weecology/portalPredictions). 
This is a desired result of our use of a [software
container](https://en.wikipedia.org/wiki/Operating-system-level_virtualization),
in the [repository](https://github.com/weecology/portalPredictions),
which enables reproducibility. Presently, we use a 
[Docker](https://hub.docker.com/r/weecology/portal_predictions) image of 
the software environment to create a container for the code. The 
image update (*i.e.* the integration of the current master branch of 
`portalcasting` into the [predictions 
repository](https://github.com/weecology/portalPredictions)) necessarily
lags behind updates to the master branch of `portalcasting`, although
ideally not long behind. The `latest` image is built using `portalcasting` 
[v0.8.0](https://github.com/weecology/portalcasting/releases/tag/v0.8.0).

The API is moderately well defined at this point, but is still evolving.

## Installation

You can install the R package from github:

```r
install.packages("devtools")
devtools::install_github("weecology/portalcasting")
```

## Production environment

If you wish to spin up a local container from the Portal Predictions 
image (to ensure that you are using a copy of the production environment 
for implementation of the `portalcasting` pipeline), you can run

```
sudo docker pull weecology/portal_predictions
```
from a shell on a computer with [Docker](https://www.docker.com/) installed
(Windows users need not include `sudo`). A tutorial on using the image 
to spin up a container is forthcoming. 

## Usage

Get started with the ["how to set up a Portal Predictions directory"
vignette](https://weecology.github.io/portalcasting/articles/howto.html)

If you are interested in adding a model to the preloaded [set of 
models](https://weecology.github.io/portalcasting/articles/models.html),
see the ["adding a model"
vignette](https://weecology.github.io/portalcasting/articles/adding_a_model.html). 

## Acknowledgements 

The motivating study—the Portal Project—has been funded nearly continuously 
since 1977 by the [National Science Foundation](http://nsf.gov/), most recently
by [DEB-1622425](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622425) 
to S. K. M. Ernest. Much of the computational work was supported by the 
[Gordon and Betty Moore Foundation’s Data-Driven Discovery 
Initiative](http://www.moore.org/programs/science/data-driven-discovery) 
through [Grant GBMF4563](http://www.moore.org/grants/list/GBMF4563) to E. P. 
White. 

We thank Henry Senyondo for help with continuous integration, Heather Bradley 
for logistical support, John Abatzoglou for assistance with climate 
forecasts, and James Brown for establishing the Portal Project.

## Author Contributions

All authors conceived the ideas, designed methodology, and developed the 
automated forecasting system. J. L. Simonis led the transition of code from
the [Portal Predictions repo](https://github.com/weecology/portalPredictions)
to `portalcasting`. 
