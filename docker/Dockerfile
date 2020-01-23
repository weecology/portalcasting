# base image is rocker/tidyverse, which includes devtools

FROM rocker/tidyverse:latest

# Install `curl` and `jags` c libraries

RUN apt-get update \
    && apt-get install -y \
       curl \
       jags

# Install portalcasting from github

RUN R -e "devtools::install_github('weecology/portalcasting')"
