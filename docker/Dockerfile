# image_name argument defines state of finished image

#ARG image_name "base"

#RUN echo "Building the $image_name image"

# built upon the rocker/tidyverse image, which includes remotes and shiny

FROM rocker/tidyverse:latest tidyverse

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

# Install portalcasting from github

ARG event_sha "HEAD"

RUN echo "Installing from github at ref $event_sha"

RUN R -e "remotes::install_github('weecology/portalcasting', ref = '$event_sha')"


