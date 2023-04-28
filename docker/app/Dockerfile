# base image is rocker/tidyverse, which includes remotes and shiny

FROM rocker/tidyverse:latest

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

# Install portalcasting from github

ARG SHA "HEAD"

RUN echo "Installing from github at ref $SHA"

RUN R -e "remotes::install_github('weecology/portalcasting', ref = '$SHA')"

# Copy in build script

COPY NEWS.md NEWS.md

# Set up the directory
RUN R -e "library(portalcasting)"
RUN R -e "portalcasting::setup_sandbox()"