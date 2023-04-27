# `portalcasting`

[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalcasting/main/LICENSE)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

Supporting [automated forecasting](https://portal.naturecast.org/) of [rodent populations](https://portal.weecology.org/)

We have dockerfiles for two images: the non-executable environment setup image (which has always been provided) and an executable image to spin-up the shiny app (provided as of v0.54.0).

The images are built using the [`Docker` GitHub Action](https://github.com/weecology/portalcasting/actions/workflows/docker-publish.yml) for any pull request in the `portalcasting` repository but is only pushed to [Docker Hub](https://hub.docker.com/repository/docker/weecology/portalcasting) upon creation of a tagged release.

Two copies of each image are pushed to Docker Hub: one tagged with the release version and one tagged as "latest", the later of which are pointed to in the [production pipeline](https://github.com/weecology/portalPredictions/blob/main/portal_weekly_forecast.sh) and app (allowing for an update of the image without requiring an associated code update).