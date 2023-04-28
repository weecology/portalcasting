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

# Set up the directory
RUN R -e ".libPaths()"
RUN R -e "list.files(.libPaths()[1])"
RUN R -e "list.files(.libPaths()[2])"