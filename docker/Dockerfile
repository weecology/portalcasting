# base image is rocker/tidyverse, which includes remotes and shiny

FROM rocker/tidyverse:latest

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

# Install portalcasting from github

# TODO: reference the specific location on github of the current build

RUN R -e "remotes::install_github('weecology/portalcasting')"
