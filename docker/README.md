# `portalcasting`

[![Docker](https://github.com/weecology/portalcasting/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/weecology/portalcasting/actions/workflows/docker-publish.yml)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/weecology/portalcasting/main.svg)](https://codecov.io/github/weecology/portalcasting/branch/main)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portal-forecasts/master/LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3332973.svg)](https://doi.org/10.5281/zenodo.3332973)
[![NSF-1929730](https://img.shields.io/badge/NSF-1929730-blue.svg)](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1929730)
[![JOSS](https://joss.theoj.org/papers/10.21105/joss.03220/status.svg)](https://doi.org/10.21105/joss.03220)

Supporting [automated forecasting](https://portal.naturecast.org/) of [rodent populations](https://portal.weecology.org/) through a set of containerized images.

## Image set structure

The set of images is defined using a single [Dockerfile](https://github.com/weecology/portalcasting/blob/main/docker/Dockerfile) with [multi-stage build contexts](https://medium.com/@tonistiigi/advanced-multi-stage-build-patterns-6f741b852fae) to generate different types of the image.

There are currently three image types within the set:

  - "base" (standard image used in production, no directory set up)
  - "production" (comes with a production directory from the date of image creation)
  - "sandbox" (comes with a sandbox directory from the date of image creation)

The production and sandbox images have "`production_`" or "`sandbox_`" appended to the tag as a prefix. 
The base image does not have a prefix.

None of the images are executable.

The image set is built using the [`Docker` GitHub Action](https://github.com/weecology/portalcasting/actions/workflows/docker-publish.yml) for any push or pull request to main in the `portalcasting` repository but is only pushed to [Docker Hub](https://hub.docker.com/repository/docker/weecology/portalcasting) upon creation of a published release.

Two copies of each the image type are pushed to Docker Hub: 

  - tagged with the release version 
  - tagged as "latest"

The [production pipeline](https://github.com/weecology/portal-forecasts/blob/main/portal_weekly_forecast.sh) uses the "latest" tag from the base image allowing for an update of the container used without requiring a code update.


## Acknowledgements

This project is developed in active collaboration with [DAPPER Stats](https://www.dapperstats.com/).

The motivating study—the Portal Project—has been funded nearly continuously since 1977 by the [National Science Foundation](https://www.nsf.gov/), most recently by [DEB-1622425](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622425) to S. K. M. Ernest. 
Much of the computational work was supported by the [Gordon and Betty Moore Foundation’s Data-Driven Discovery Initiative](https://www.moore.org/initiative-strategy-detail?initiativeId=data-driven-discovery) through [Grant GBMF4563](https://www.moore.org/grant-detail?grantId=GBMF4563) to E. P. White. 

We thank Heather Bradley for logistical support, John Abatzoglou for assistance with climate forecasts, and James Brown for establishing the Portal Project. 

