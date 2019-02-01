# portalcasting (development version)

## [portalcasting 0.2.0](https://github.com/weecology/portalcasting)
*2019-02-01*

### Code testing
* All of the code is now tested via [`testthat`](https://github.com/weecology/portalcasting/tree/master/tests).
* Test coverage is tracked via [Codecov](https://codecov.io/gh/weecology/portalcasting).
* The only functionality not covered in testing on Codecov is associated with
`download_predictions()`, which intermittently hangs on Travis. Testing is
available, but requires manual toggling of the `test_location` value to
`"local"` in the relevant test scripts (02-directory and 12-prepare_predictions).

### Enforcement of inputs
* Most of the functions previously did not have any checks on input argument 
classes, sizes, etc.
* Now all functions specifically check each argument's value for validity
and throw specific errors.

### Documentation
* All of the functions have fleshed out documentation that specify argument
requirements, link to each other and externally, and include more information.

### Data classes
* To smooth checking of different data structures, we now define data
objects with classes in addition to their existing (`data.frame` or
`list`) definitions.
* `rodents`, `rodents_list`, `covariates`, `metadata`, `moons`
* These classes do not presently have any specified methods or functions.

## Options list classes 
* To smooth checking of different list structures, we now define the options
list objects with classes in addition to their existing `list` definitions.
* `all_options`, `dir_options`, `PortalData_options`, `data_options`, 
`moons_options`, `rodents_options`, `covariates_options`, `metadata_options`,
`predictions_options`, `models_options`, and `cast_options`
* Each of these classes is created by a function of that name.
* These classes do not presently have any specified methods or functions 
that operate on them.

### Added functions
* `classy()` allows for easy application of classes to objects in a `%>%` pipeline
* `read_data()` provides simple interface for loading and applying classes to
model-ready data files.
* `remove_incompletes()` removes any incomplete entries in a table, as defined
by an `NA` in a specific column.
* `check_options_args()` provides a tidy way to check the input arguments (wrt
class, length, numeric limitations, etc.) to the options functions.

### Vignettes
* Three vignettes were added:
  * [current models vignette](https://weecology.github.io/portalcasting/articles/models.html) was brought from the [forecasting website](https://portal.naturecast.org/models.html).
  * [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) was created from the earlier `README.md` file.
  * [adding a model vignette](https://weecology.github.io/portalcasting/articles/adding_a_model.html) was constructed based on two pages from the 
    [Portal Predictions repository](https://github.com/weecology/portalPredictions) wiki
    ([1](https://github.com/weecology/portalPredictions/wiki/Adding-a-new-model) and 
    [2](https://github.com/weecology/portalPredictions/wiki/Forecast-file-format)) and with substantial additional text added.

### Retention of all forecasts of covariates
* Previous versions retained only one covariate forecast per newmoon.
* We now enable retension of multiple covariate forecasts per newmoon and tag 
the forecast date with a time stamp as well.

### Website
* Added a website driven by [pkgdown](https://pkgdown.r-lib.org/).

### Changelog
* Developed this changelog as part of the package.

### Support documents
* Added [code of conduct](https://github.com/weecology/portalcasting/blob/master/CODE_OF_CONDUCT.md) 
and [contribution guidelines](https://github.com/weecology/portalcasting/blob/master/CONTRIBUTING.md)
to the repository.

## [portalcasting 0.1.1](https://github.com/weecology/portalcasting/commit/05ed76f76a82f32a5a3120eb7c9ef0dc95bd8ae4)
*2019-01-13*

### Addressing Portal Data download
* Setting default back to the Zenodo link via updated portalr function.
* Updated `fill_PortalData()` and new `PortalData_options()` allow 
for control over download source.

## [portalcasting 0.1.0](https://github.com/weecology/portalcasting/tree/77fb0c8a32de5ff39715e652ce8e5b813ad02ff3) 
*true code edits 2018-12-14, version number updated 2019-01-02*

### Migration from Portal Predictions repository
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

### Development of code pipeline
* The previous implementation of the R codebase driving the forecasting 
(housed within the [portalPredictions](https://github.com/weecology/portalPredictions/tree/ac032f3938a6695a8e5d27ee380032195b2af396)
repo) was a mix of functions and loose code and hard-coded to the repo.
* The package implementation generalizes the functionality and organizes
the code into a set of hierarchical functions that drive creation and use
of the code within the repo or elsewhere. 
* See the [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) for further details.

### Explicit directory tree
* To facilitate portability of the package (a necessity for smooth sandboxing
and the development of new models), we now include explicit, controllable
definition of the forecasting directory tree.
* See the [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) for further details.

### Introduction of options lists
* To facilitate simple control via defaults argument inputs and flexibility to 
changes to inputs throughout the code hierarchy, we include a set of functions
that default options for all aspects of the codebase.
* See the [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) for further details.

## [portalcasting 0.0.1](https://github.com/weecology/portalPredictions/tree/ac032f3938a6695a8e5d27ee380032195b2af396) 
*2018-11-06*

* This is the last iteration of the code that now exists in portalcasting in
its previous home within the portalPredictions repo. 
* It was not referred to by the name portalcasting at the time.