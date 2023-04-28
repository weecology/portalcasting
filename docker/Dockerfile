# base image is rocker/tidyverse, which includes remotes and shiny

FROM rocker/tidyverse:latest

# Install `jags` c library

RUN apt-get update \
    && apt-get install -y \
       jags

COPY /home/runner/work/portalcasting/portalcasting_0.54.0.tar.gz .



