# LDATS (development version)

# [portalcasting 0.1.1](https://github.com/weecology/portalcasting/commit/05ed76f76a82f32a5a3120eb7c9ef0dc95bd8ae4)
*2019-01-13*

## Addressing Portal Data download
* Setting default back to the Zenodo link via updated portalr function.
* Updated `fill_PortalData()` and new `PortalData_options()` allow 
for control over download source.

# [portalcasting 0.1.0](https://github.com/weecology/portalcasting/tree/77fb0c8a32de5ff39715e652ce8e5b813ad02ff3) 
*true code edits 2018-12-14, version number updated 2019-01-02*

## Migration from Portal Predictions repository
* Code was brought over from the forecasting repository to be housed in its 
own pacakge.
* Multiple updates to the codebase were included, but intentionally just
"under the hood", meaning little or no change to the output and simplification
of the input.
* A major motivation here was also to facilitate model development, which 
requires being able to set up a local version of the repository to play with
in what we might consider a ["sandbox"](https://en.wikipedia.org/wiki/Sandbox_(software_development)).
This will allow someone to develop and test new forecasting models in a space
that isn't the forecasting repo itself (or a clone or a fork), but a truly novel
location. At this point, the sandbox setup isn't fully robust from within this 
package, but rather requires some additional steps (to be documented).

## Development of code pipeline
* The previous implementation of the R codebase driving the forecasting 
(housed within the [portalPredictions](https://github.com/weecology/portalPredictions/tree/ac032f3938a6695a8e5d27ee380032195b2af396)
repo) was a mix of functions and loose code and hard-coded to the repo.
* The package implementation generalizes the functionality and organizes
the code into a set of hierarchical functions that drive creation and use
of the code within the repo or elsewhere. 
* See the [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) for further details.

## Explicit directory tree
* To facilitate portability of the package (a necessity for smooth sandboxing
and the development of new models), we now include explicit, controllable
definition of the forecasting directory tree.
* See the [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) for further details.

## Introduction of options lists
* To facilitate simple control via defaults argument inputs and flexibility to 
changes to inputs throughout the code hierarchy, we include a set of functions
that default options for all aspects of the codebase.
* See the [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) for further details.

# [portalcasting 0.0.1](https://github.com/weecology/portalPredictions/tree/ac032f3938a6695a8e5d27ee380032195b2af396) 
*2018-11-06*

* This is the last iteration of the code that now exists in portalcasting in
its previous home within the portalPredictions repo. 
* It was not referred to by the name portalcasting at the time.