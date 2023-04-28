# base image is rocker/tidyverse, which includes remotes and shiny

FROM rocker/tidyverse:latest

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

# Install git2r, portalcasting from github
RUN R -e "remotes::install_github('weecology/portalcasting')"
