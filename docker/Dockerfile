# We use multi-stage build contexts to generate different versions of the image
# See https://medium.com/@tonistiigi/advanced-multi-stage-build-patterns-6f741b852fae

# image_name argument defines state of finished image

ARG image_name="base"


# Our image is built upon rocker/verse, tagged as verse for downstream reference

FROM rocker/verse:latest AS verse

RUN apt-get update \
    && apt-get install -y \
       curl \
       jags


# Install portalcasting from github event that triggered the specific workflow

ARG event_sha="HEAD"
RUN echo "Installing from github at ref $event_sha"
RUN R -e "remotes::install_github('weecology/portalcasting', ref = '$event_sha')"

# For branches with directories set up, set main as ~/portalPredictions, as . is in general use

ARG main="~/portalPredictions"

FROM verse AS base-branch

FROM verse AS sandbox-branch
RUN R -e "portalcasting::setup_sandbox(main = '$main')"

FROM verse AS production-branch
RUN R -e "portalcasting::setup_production(main = '$main')"

FROM ${image_name}-branch AS final
