# `portalcasting`

[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalcasting/main/LICENSE)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

Supporting [automated forecasting](https://portal.naturecast.org/) of [rodent populations](https://portal.weecology.org/)

The image is built using the [`Docker` GitHub Action](https://github.com/weecology/portalcasting/actions/workflows/docker-publish.yml) for any pull request in the `portalcasting` repository but is only pushed to [Docker Hub](https://hub.docker.com/repository/docker/weecology/portalcasting) upon creation of a tagged release.

Two copies of the image are pushed to Docker Hub: one tagged with the release version and one tagged as "latest", the second of which is pointed to in the [production pipeline](https://github.com/weecology/portalPredictions/blob/main/portal_weekly_forecast.sh) (allowing for an update of the image without requiring an associated code update).