# We use multi-stage build contexts to generate different versions of the image
# See https://medium.com/@tonistiigi/advanced-multi-stage-build-patterns-6f741b852fae

# image_name argument defines state of finished image

ARG image_name="base"


# Our image is built upon rocker/tidyverse, tagged as tidyverse for downstream reference

FROM rocker/tidyverse:latest AS tidyverse


# Install `jags` and `build-essentials` c libraries

RUN apt-get update \
    && apt-get install -y \
       jags 

# Install miniconda

ENV CONDA_DIR /opt/conda
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
     /bin/bash ~/miniconda.sh -b -p /opt/conda

# Put conda in path so we can use conda activate

ENV PATH=$CONDA_DIR/bin:$PATH

# Install cmdstan using miniconda 

RUN conda create -n stan -c conda-forge cmdstan
RUN conda init bash
RUN conda activate stan

# Install portalcasting from github event that triggered the specific workflow

ARG event_sha="HEAD"
RUN echo "Installing from github at ref $event_sha"
RUN R -e "remotes::install_github('weecology/portalcasting', ref = '$event_sha')"

# For branches with directories set up, set main as ~/portalPredictions, as . is in general use

ARG main="~/portalPredictions"

FROM tidyverse AS base-branch

FROM tidyverse AS sandbox-branch
RUN R -e "portalcasting::setup_sandbox(main = '$main')"

FROM tidyverse AS production-branch
RUN R -e "portalcasting::setup_production(main = '$main')"

FROM ${image_name}-branch AS final
