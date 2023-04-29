# We use multi-stage build contexts to generate different versions of the image
# See https://medium.com/@tonistiigi/advanced-multi-stage-build-patterns-6f741b852fae

# image_name argument defines state of finished image

ARG image_name "base"

# built upon the rocker/tidyverse image, which includes remotes and shiny

FROM rocker/tidyverse:latest AS tidyverse

RUN echo "Building the $image_name image"

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

# Install portalcasting from github

ARG event_sha "HEAD"

RUN echo "Installing from github at ref $event_sha"

RUN R -e "remotes::install_github('weecology/portalcasting', ref = '$event_sha')"


# dev

FROM tidyverse AS base-branch

FROM tidyverse AS production-branch
RUN R -e "library(portalcasting); prefab_datasets(); #setup_production( )"

FROM tidyverse AS app-branch
RUN R -e "library(portalcasting); prefab_models(); #setup_production( )"
# EXPOSE ??
# CMD run app

FROM ${image_name}-branch AS final
