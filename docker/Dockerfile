# image_name argument defines state of finished image

ARG image_name "base"

# base image is rocker/tidyverse, which includes remotes and shiny

FROM rocker/tidyverse:latest

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

# Install portalcasting from github

ARG event_sha "HEAD"

RUN echo "Installing from github at ref $event_sha"

RUN R -e "remotes::install_github('weecology/portalcasting', ref = '$event_sha')"