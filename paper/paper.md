---
title: 'portalcasting: Supporting automated forecasting of rodent populations'

tags:
  - Portal Project
  - forecasting
authors:
  - name: Juniper L. Simonis
    orcid: 0000-0001-9798-0460
    affiliation: "1, 2"
  - name: Glenda M. Yenni
    orcid: 0000-0001-6969-1848
    affiliation: 1
  - name: Ellen K. Bledsoe
    orcid: 0000-0002-3629-7235
    affiliation: "1, 3"
  - name: Erica M. Christensen
    orcid: 0000-0002-5635-2502
    affiliation: 4
  - name: Henry Senyondo
    orcid: 0000-0001-7105-5808
    affiliation: 1
  - name: Shawn D. Taylor
    orcid: 0000-0002-6178-6903
    affiliation: 4
  - name: Hao Ye
    orcid: 0000-0002-8630-1458
    affiliation: 1
  - name: Ethan P. White
    orcid: 0000-0001-6728-7745
    affiliation: 1
  - name: S. K. Morgan Ernest
    orcid: 0000-0002-6026-8530
    affiliation: 1
affiliations:
  - name: University of Florida
    index: 1
  - name: DAPPER Stats
    index: 2
  - name: University of Regina, Saskatchewan
    index: 3
  - name: Jornada Experimental Range
    index: 4
date: 30 September 2020
bibliography: paper.bib
---
  
# Summary

The portalcasting package provides a model development, deployment, and evaluation system for forecasting how ecological systems change through time, with a focus on a widely used long-term study of mammal population and community dynamics, the Portal Project [@brown1977competition; @brown1998desert; @Ernest:2018b]. The software is designed to encourage the active engagement of the broad ecological modeling community in understanding the dynamics of this well-sampled empirical system and forecasting future changes. The initial infrastructure for continuous analysis and forecasting at the site provided a functional system for running existing models repeatedly [@White:2018]. However, it did not facilitate the development and evaluation of new models. The portalcasting package combines existing R packages for modeling (e.g., JAGS), forecasting (e.g., forecast) and data management (e.g., portalr) into a continuous pipeline that produces new forecasts and allows for new model development.

The process of code development and exploration outside of a production pipeline is known as sandboxing and is key to scientific progress in a forecasting setting, as it substantially lowers the effort required for new users to contribute models. The portalcasting R package [@RCoreTeam2020] simultaneously provides the engine for production forecasting for this system and a portable sandbox that allows users to set up a fully-functioning replica of the production system on their own computer for testing new models. The package also leverages a container based approach to enable fully reproducible models to be developed and tested for the production system. The associated Docker image is automatically rebuilt with each portalcasting release.

The portalcasting package automates the acquisition and updating of near real-time data; analysis of the data using models; and making, reporting, and evaluating ecological forecasts. A combination of a simple yet powerful API with advanced configuration options allow users to both quickly add new models that run using default configurations and also produce highly specific modifications to the underlying settings.


# Statement of need

Accurate and up-to-date forecasts are needed in ecology for tasks ranging from endangered species management, disease epidemiology, and invasive species control. However, the field generally lacks many of the tools necessary for operationalizing ecological forecasts. As a result research on automated ecological forecasting requires years of development for each system of interest.  Further, engaging the ecological modeling community in these efforts is challenging, especially as a novice. The portalcasting package helps address this gap by allowing other researchers to quickly engage in the development of forecasting models for a widely known and used long-term ecological study with a long history of open data [@ernest2009data; @ernest2016data; @Ernest:2018a; @Ernest:2018b] and associated open source software [@christensen2019portalr]. The software will be used as a key component of a series of ecological forecasting competitions to engage the field broadly in how to make and improve ecological forecasts and the resulting models will be integrated into the production forecasting system. While portalcasting is designed for the Portal Project forecasting system, it also lays the groundwork for a generalized approach to ecological forecasting pipelines that work seamlessly in production and sandbox environments, thus facilitating broad scientific development in an ecological forecasting setting.

# Acknowledgements

The motivating study—the Portal Project—has been funded nearly continuously since 1977 by the National Science Foundation, most recently by DEB-1929730 to S. K. M. Ernest and E.P White. The development of the portalcasting software package was supported by this NSF grant, NSF grant DEB-1622425 to S. K. M. Ernest, and the Gordon and Betty Moore Foundation’s Data-Driven Discovery Initiative through Grant GBMF4563 to E. P. White.

We thank Donna Dyer, Brandie Seay, and Dan Deep for logistical support, John Abatzoglou for assistance with climate forecasts, and James Brown, Diane Davidson, and O.J. Reichman
for establishing the Portal Project.

# References
