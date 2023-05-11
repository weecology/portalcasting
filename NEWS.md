# portalcasting (development version)

Version numbers follow [Semantic Versioning](https://semver.org/).

# [portalcasting 0.57.0](https://github.com/weecology/portalcasting/releases/tag/v0.57.0)
*5-11-2023*

## Editing documentation
* Tightening and improving language in Title and Description fields in DESCRIPTION file

## Preventing duplicate keys in control lists at the top level

# [portalcasting 0.56.0](https://github.com/weecology/portalcasting/releases/tag/v0.56.0)
*5-5-2023*

## Tidying the app.R script
* Facilitating the working across different calling styles.

# [portalcasting 0.55.0](https://github.com/weecology/portalcasting/releases/tag/v0.55.0)
*5-4-2023*

## Reorganizing the app pointing
* Now having a single app.R file in the top level of the directory simplifies the issue around where the files are for the app
* This replaces the global.R, ui.R, and server.R files 

# [portalcasting 0.54.0](https://github.com/weecology/portalcasting/releases/tag/v0.54.0)
*5-3-2023*

## Adding `sandbox` and `production` version of `update_` functions. 

## Adding Pat as an author

## Pointing the dockerfile to the current build SHA
* Previously, we were using default settings for `remotes::install_github`, which actually points to HEAD, when testing the build and push for the docker action. that was fine (although not great) when we were only running that action on tagged version releases, as the main branch was typically up-to-date with the tag and such, but this isn't exact and it also means any testing of the build on a PR was actually still grabbing from main, which is not what we want.
* We now use the SHA for the specific event that triggers the build by passing it into the docker file as an ARG

# [portalcasting 0.53.0](https://github.com/weecology/portalcasting/releases/tag/v0.53.0)
*4-26-2023*

## Removing temporary patch for backwards compatibility  

# [portalcasting 0.52.0](https://github.com/weecology/portalcasting/releases/tag/v0.52.0)
*4-25-2023*

## Patching evaluations issues
* The table was too big as was for github standard, so removing columns of repeated content that can be grabbed from metadata

# [portalcasting 0.51.1](https://github.com/weecology/portalcasting/releases/tag/v0.51.1)
*4-24-2023*

## Patches to pkgdown site


# [portalcasting 0.51.0](https://github.com/weecology/portalcasting/releases/tag/v0.51.0)
*4-24-2023*

## Major updates to pkgdown site
* Shift to bootstrap 5

## Include CI tests of \dontrun examples and eval=FALSE vignettes
* Because of the long run time of some of the code, we wrap most documentation examples and vignette code in a way to prevent evaluation in real-time. As a result, much of the documentation code isn't run and therefore would not break builds if it would error. 
* To address this, we add two scripts in the new inst/extra_testing folder and a github action runner for each.

## Added CITATION file
* Cites the JOSS paper.

## Bringing the app code into the package
* Improves robustness of building the app (includes code and dependencies in the docker image, allows for unit testing app components, etc.)
* Also allows users to spin-up a local version with `run_web_app()` pointed to `main`

## Using `arrow` to speed up reading and writing files

## Evaluation figures now read from evaluations file
* Avoids computing evaluations while generating plots

## Elimination of model-named functions
* The models are now implemented using `cast` on their fit and cast elements in their control lists
* Only models the need new functions have them (`meta_tsglm` and `fit_runjags`) for fitting
* `forecast` method used generally now for `cast` function
  * introduction of `forecast` methods for `tsglm` and `runjags` objects

## Shifting covariates to a daily-level build initial step
* need to shift this to level daily so we can manage when a newmoon is split between historic and forecast days

## Putting species under dataset in the models' controls lists
* This is more articulated and allows for finer control to help avoid fitting issues, etc.

## Moving arguments into functionalities
* `cast_date` is not an argument anymore, just filled automatically 
* `dataset` arguments are also being removed as possible to streamline (just pull from model controls)

## Model functions are now species-level
* To facilitate a lot of downstream functionality, we're breaking up the model functions to operate on the species-level rather than the dataset-level, according to the new control lists
* Species that were failing the nb GARCH models (s or not) have been removed, since that throws a warning and then fits a Poisson version, and we are now fitting Poisson versions of everyone.

## process_model_output replaces save_cast_output and various model processing bits
* provides a much more general way to produce a forecast that can be integrated in the system, leveraging the metadata files

## casts metadata table includes new column
* species
* to facilitate backwards compatibility, filled with NA for previous tables if missing when loaded

## updates to prefab models to 13 time steps forward (addressing issue 297)
* pevGARCH, nbGARCH, nbsGARCH all get past_mean set to 13 
* all models set with lead_time of 13

## new models
* sAutoArima
* sNaiveArima
* pGARCH
* psGARCH

## new functions
* `update_dir` is an "update-flavored" setup function 

## scoring
* log and crps!


# [portalcasting 0.50.0](https://github.com/weecology/portalcasting/releases/tag/v0.50.0)
*2023-03-03*

## Update prefab_model_controls.yaml
* including a nicer print_name for rendering the model texts


# [portalcasting 0.49.0](https://github.com/weecology/portalcasting/releases/tag/v0.49.0)
*2023-02-24*

## Added a clearing out step to fill_data

## Updated download climate forecasts to check for version pre-updates

# [portalcasting 0.48.0](https://github.com/weecology/portalcasting/releases/tag/v0.48.0)
*2023-02-23*

## Settings updates to avoid redownloading archive
* settings `overwrite` is now `FALSE` by default to manage the version match decision making
* the directory resources portalpredictions version is updated to be the correct value (`"latest"`; was `NULL`)
* the `overwrite` setting is temporarily removed from the file saving functions to prevent argument name confusion
* we need to have an external location of file version to compare

## Fixing jags model inits values
* Model values were not facilitating good chain mixing, updated now

## More transitions from `moons` to `newmoons` to align naming

# [portalcasting 0.47.0](https://github.com/weecology/portalcasting/releases/tag/v0.47.0)
*2023-02-21*

## Patching error in cast metadata output for jags models
* Multiple models had the jags_logistic_covariates name being used in the metadata output when that was not correct.


# [portalcasting 0.46.0](https://github.com/weecology/portalcasting/releases/tag/v0.46.0)
*2023-01-24*

## Articulating dataset controls for species
* Changing dataset controls list input from having `all` for species to listing the species.


# [portalcasting 0.45.0](https://github.com/weecology/portalcasting/releases/tag/v0.45.0)
*2022-11-18*

## Removing `dm_controls` as a separate dataset
* Following the removal of the `interp` datasets
* Now model controls indicate species to which their applied

## New models
* `jags_logistic_competition`
* `jags_logistic_covariates`
* `jags_logistic_competition_covariates`


# [portalcasting 0.44.0](https://github.com/weecology/portalcasting/releases/tag/v0.44.0)
*2022-10-27*

## Major updates to JAGS models
* Modeling and tracking sigma not tau (sd, not precision)
* No max caps for density or counts (aka removing guardrails)
* No use of ` + 0.1` `- 0.1` for logging, managing it differently
* Chain increase from 2 to 4, silent jags is now FALSE
* Removal of `jags_SS` wrapper, which limited adaptation of the model

## Removing JAGS model vignette
* Removal of the `jags_SS` wrapper eliminates the core of that vignette

## Adding integrated warm precip and ndvi covariates 
* Allows covariates logistic model

## Shift internal naming of subs to subdirectories

## Shift folder naming of raw, casts to resources, forecasts

# [portalcasting 0.43.0](https://github.com/weecology/portalcasting/releases/tag/v0.43.0)
*2022-05-31*


## Trim text in model vignette
* Vignette now pulls text from the the model list, and loops the model list, rather than have the raw text in the markdown doc.
* This locates the description in a place that is accessible to other content generation (e.g., the portal forecast website)

# [portalcasting 0.42.0](https://github.com/weecology/portalcasting/releases/tag/v0.42.0)
*2022-05-27*


## Data interpolation moved from "dataset" to "within model"
* Patching, not all hit with previous release

# [portalcasting 0.41.0](https://github.com/weecology/portalcasting/releases/tag/v0.41.0)
*2022-05-26*


## Data interpolation moved from "dataset" to "within model"
* Previously, datasets included, for example `all` and `all_interp`. Now, only `all` exists and models interpolate as needed.


# [portalcasting 0.40.0](https://github.com/weecology/portalcasting/releases/tag/v0.40.0)
*2022-05-12*


## Argument / nomenclature updates
* `dfl` arg to `x`

## `read_rodents` default settings update
* Now pulls all datasets using `prefab_datasets`

## Subdirectory internal naming changed to remove spaces
* `model fits` now back to `fits` and `model scripts` back to `models`

## `forecast_evaluations` file now saved
* Flattened version of the generated list of evaluations
* Rather crude options of saving or not and overwriting the whole file or not based on settings 
* No file saving occurs when a single cast is evaluated

# [portalcasting 0.39.0](https://github.com/weecology/portalcasting/releases/tag/v0.39.0)
*2022-05-11*

## `download_timeout` now set to default of 600 for `setup_production`
* Allows download of larger directory archive without timeout 


# portalcasting 0.38.0
*2022-04-21*

## If there's only one model, don't ensemble
* Prevents warnings / errors 


# [portalcasting 0.37.0](https://github.com/weecology/portalcasting/releases/tag/v0.37.0)
*2022-04-21*

## Building out evaluation pipeline
* starting with what is already occurring, but formalizing as such as part of an `evaluate_casts` and `evaluate_cast` pair of functions
* `evaluate_casts` function now works automatically to evaluate all the casts using `evaluate_cast`, generating the error table as it does when being used, but nothing is saved out or updated.
* there is also no filter on evaluated casts by default, so the output from the forecasts without observations to evaluate is a table with a single row with NaN, and then they get wrapped up into the list. 
  * no errors, just noteworthy

# [portalcasting 0.36.0](https://github.com/weecology/portalcasting/releases/tag/v0.36.0)
*2022-04-08*

## `rodent_species`
* Now includes `type` argument with `abbreviation`, `Latin`, and `table` options
* No more `most_abundant_species` function, as we're not using it on the website.


# [portalcasting 0.35.0](https://github.com/weecology/portalcasting/releases/tag/v0.35.0)
*2022-04-07*

## Updating model controls
* `time`
* `response` with `type` and `link` 

## Developing evaluate functions
* `evaluate_casts` and `evaluate_cast` currently just placeholders

# [portalcasting 0.34.0](https://github.com/weecology/portalcasting/releases/tag/v0.34.0)
*2022-04-07*

## Removing `tmp` sub
* No longer used, internal R code (e.g., `tempdir`) provides needed functionality
* Also removing `clear_tmp`

# [portalcasting 0.33.0](https://github.com/weecology/portalcasting/releases/tag/v0.33.0)
*2022-04-07*

## `setup_production` defaults to `verbose = TRUE`
* Facilitates fuller log


# [portalcasting 0.32.0](https://github.com/weecology/portalcasting/releases/tag/v0.32.0)
*2022-04-05*


## Relocation of prefab controls 
* Moved from source code scripts to `.yaml` files in `inst/extdata`
* `read_` and `write_` functions for both rodent and model controls lists


## Updating / rectifying terminology
* "data_set" -> "dataset"

# [portalcasting 0.31.0](https://github.com/weecology/portalcasting/releases/tag/v0.31.0)
*2022-04-01*

## Tidying up documentation


# [portalcasting 0.30.0](https://github.com/weecology/portalcasting/releases/tag/v0.30.0)
*2022-04-01*

## Settings list
* `setup_dir` now takes a `settings` argument that is a `list` of the arguments 
* `directory_settings` function now quickly and cleanly collapses the settings that go into `setup_dir`

## Generalized functionality for models and rodent data sets
* Control lists are now structured for use with `do.call`

## Codebase formatting [work in progress]
* No longer concerned about the 80 char line limit
* Long argument lists, etc. are now formatted for quick top-to-bottom reading, via alignment on the `(` and `=` 

## Removal of superfluous `_path` functions
* Use of base R functions is sufficient

## `arg_checks` removed
* Internalized auto-checking relieves user of need to dictate checking

## Temporary removal of "adding a model and data" vignette
* Need to update with new API
* Also need to add alt-text to all images

# [portalcasting 0.29.0](https://github.com/weecology/portalcasting/releases/tag/v0.29.0)
*2022-03-30*

## Patch `bline` bug
* Argument needed to be removed still

# [portalcasting 0.28.0](https://github.com/weecology/portalcasting/releases/tag/v0.28.0)
*2022-03-28*

## `messageq` 
* Function redesigned to align with `message` directly argument for argument with the addition of the `quiet` argument.
* Now allows for multiple message arguments via `...` that become pasted together

## Removal of specialized message functions
* Minimize unnecessary functions

# [portalcasting 0.27.0](https://github.com/weecology/portalcasting/releases/tag/v0.27.0)
*2022-03-28*

## Simplified directory creation function pipeline
* Now just `create_dir`

# [portalcasting 0.26.0](https://github.com/weecology/portalcasting/releases/tag/v0.26.0)
*2022-03-02*

## jags_logistic model added
* invoked as `jags_logistic` like `jags_RW`, applied to `DM` controls dataset.
* Building upon the jags_RW model, jags_logistic expands upon the "process model" underlying the Poisson observations. 
* There are four process parameters: mu (the density of the species at the beginning of the time series) and tau (the precision (inverse variance) of the random walk, which is Gaussian on the log scale) for the starting value and r (growth rate) and K (carrying capacity) of the dynamic population. The observation model has no additional parameters. 

# [portalcasting 0.25.4](https://github.com/weecology/portalcasting/releases/tag/v0.25.4)
*2022-01-26*

## Docker build check issues

# [portalcasting 0.25.3](https://github.com/weecology/portalcasting/releases/tag/v0.25.3)
*2022-01-26*

## Further removal of vestigial rEDM code
* Commenting out as needed to prevent build breaks


# [portalcasting 0.25.2](https://github.com/weecology/portalcasting/releases/tag/v0.25.2)
*2022-01-19*

## Updating title to match JOSS 


# [portalcasting 0.25.1](https://github.com/weecology/portalcasting/releases/tag/v0.25.1)
*2021-12-08*

## Getting latest version of portalr
* tagging to update Docker image with portalr 0.3.9

# [portalcasting 0.25.0](https://github.com/weecology/portalcasting/releases/tag/v0.25.0)
*2021-11-30*

## Tidying for JOSS ms
* adding source and version args to check args
* software context added to ms
* overview added to readme
* updating the getting started vignette to use production to allow for historical uses

# [portalcasting 0.24.0](https://github.com/weecology/portalcasting/releases/tag/v0.24.0)
*2021-11-11*

## Speeding downloading
* Shift to default downloading archive from GitHub
* setup_dir does not download archive by default, but setup_production does
* download function being broken out into components -- work is still ongoing, but now have a separate function for each of the portaldata portalpredictions and climate forecasts
* addresses #132  #199 

# [portalcasting 0.23.0](https://github.com/weecology/portalcasting/releases/tag/v0.23.0)
*2021-11-10*

## patching issue with ndvi preparation
* the ndvi data stream is not filling in with new content, resulting in NAs for the latter half of 2021
* using a forecast call to fill in the missing values as a temporary patch 


# [portalcasting 0.22.0](https://github.com/weecology/portalcasting/releases/tag/v0.22.0)
*2021-11-09*

## Removal of `simplexEDM` and `GPEDM`
* Previous versions used Hao's fork of the `rEDM` packages, which has been deprecated and now breaks because of the updates in Rcpp
* Switching to CRAN version
* Updating `rEDM` to CRAN does not fix the issue, so these models cannot be used in prefab set
* Removed from the prefab control list and removed the documentation
* No longer exported from the NAMESPACE

## Edit tests for ensembling and figure making
* A few edge case issues arose in test because of fixed moons
* Should be resolved through edits to test scripts without altering functions

# [portalcasting 0.21.4](https://github.com/weecology/portalcasting/releases/tag/v0.21.4)
*2021-11-09*

## Add Henry to the DESCRIPTION file



# [portalcasting 0.21.3](https://github.com/weecology/portalcasting/releases/tag/v0.21.3)
*2021-09-17*

## Add git2r to the docker container

# portalcasting 0.21.2
*Skipped*

## Improving GitHub Actions Running 
* Including no running of examples (needed to be explicitly stated); [addressing 206](https://github.com/weecology/portalcasting/issues/206)
* Use RStudio package manager to speed up running; [addressing 206](https://github.com/weecology/portalcasting/issues/206)

# [portalcasting 0.21.1](https://github.com/weecology/portalcasting/releases/tag/v0.21.1)
*2021-02-11*

## Patch "NA" plotting issue

# [portalcasting 0.21.0](https://github.com/weecology/portalcasting/releases/tag/v0.21.0)
*2021-02-10*

## Highlighting of species in plotting

## Shift to GitHub actions

# [portalcasting 0.20.0](https://github.com/weecology/portalcasting/releases/tag/v0.20.0)
*2021-02-04*

## Stops saving model fits in the portalPredictions repository

## Addresses issues with covariate data
* Missing data from weather stations caused issues
* Now if there is a missing set of data for a month of covariates, the saved covariate forecasts are used

## Added `dm_controls` and `dm_controls_interp` to prefab data sets
* For use in the basic single-species process models
* Note lowercase name! Using capitals in the actual name of the data set creation will cause problems because `tolower` gets used elsewhere!

## Patches issue with `check_args` when using `portalcasting::function_name()`

## Tightens testing time

# portalcasting 0.19.0
*2020-10-28*

## Patch moons
* Addressing issues with updated dplyr

# [portalcasting 0.18.3](https://github.com/weecology/portalcasting/releases/tag/v0.18.3)
*2020-01-23*

## Testing the Dockerfile

# [portalcasting 0.18.2](https://github.com/weecology/portalcasting/releases/tag/v0.18.2)
*2020-01-23*

## Setting the Docker build up with its own folder

## Tidying README etc

# [portalcasting 0.18.1](https://github.com/weecology/portalcasting/releases/tag/v0.18.1)
*2020-01-23*

## Testing the Dockerfile 

# [portalcasting 0.18.0](https://github.com/weecology/portalcasting/releases/tag/v0.18.0)
*2020-01-23*

## Bringing the Dockerfile over from `portalPredictions`
* For building here


# [portalcasting 0.17.1](https://github.com/weecology/portalcasting/releases/tag/v0.17.1)
*2020-01-16*

## Addition of GPEDM (model and function)
* Gaussian processes using Empirical Dynamic Modeling
* Actually does this (0.17.0 had a snag)

# [portalcasting 0.17.0](https://github.com/weecology/portalcasting/releases/tag/v0.17.0)
*2020-01-15*

## Addition of GPEDM (model and function)
* Gaussian processes using Empirical Dynamic Modeling

# [portalcasting 0.16.0](https://github.com/weecology/portalcasting/releases/tag/v0.16.0)
*2020-01-10*

## Change in format for saving out `model_fits` and `model_casts`
* Saving `model_fits` and `model_casts` out as serialized `.json` files now instead of `.rData` files
* More reliable and generalized.
* Also added functions for reading them in (`read_model_casts` and `read_model_fits`).
* [addresses](https://github.com/weecology/portalcasting/issues/164)

# [portalcasting 0.15.2](https://github.com/weecology/portalcasting/releases/tag/v0.15.2)
*2019-12-18*

## Using github version of portalr
* Due to backwards incompatible changes in portalr and it not being on CRAN yet


# [portalcasting 0.15.1](https://github.com/weecology/portalcasting/releases/tag/v0.15.1)
*2019-12-18*

## Trivial updating
* To address a Zenodo hiccup

# [portalcasting 0.15.0](https://github.com/weecology/portalcasting/releases/tag/v0.15.0)
*2019-10-31*

## JAGS vignette
* Added a vignette that describes how to use the JAGS/runjags API within portalcasting.
* [addresses](https://github.com/weecology/portalcasting/issues/152)

## Pulls code for `match.call.defaults` into the package
* Use of it from `DesignLibrary` causes a problematic dependency chain with the docker image building

## Patch bug in `most_abundant_species`
* Wasn't using the species name function, and so was pulling in the traps column, which was causing a break in plotting.

# [portalcasting 0.14.0](https://github.com/weecology/portalcasting/releases/tag/v0.14.0)
*2019-10-24*

## Adds exclosure data to the prefab models

# [portalcasting 0.13.0](https://github.com/weecology/portalcasting/releases/tag/v0.13.0)
*2019-10-12*

## Full writing of `control_files` in model scripts
* Previously, the controls list for the files in the model scripts was taken from the environment in which the script was run, which opens the script to everything, which is undesirable.
* After the need to include a control list for runjags models forced an explicit writing of the list inputs, the code was available to transfer to the files control list.
* This does mean that the function calls in the scripts are now super long and explicit, but that's ok.
* To avoid super long model script lines (where event default inputs are repeated in the list functions), a function `control_list_arg` was made to generalize what was coded up from the runjags list for use also with the files control list. This function writes a script component that only includes arguments to the list function that are different from the formal definition. 

# portalcasting 0.12.1
*2019-10-12*

## Fixes to the pkgdown site
* rmarkdown v1.16.0 has some issues with rendering images, so forcing use of v1.16.1 for now.
* Inclusion of new functions in reference list.

# [portalcasting 0.12.0](https://github.com/weecology/portalcasting/releases/tag/v0.12.0)
*2019-10-11*

## `portalcast` updates model scripts according to `controls_model`
* Previously, if you changed any controls of a prefab model, you had to manually re-write the models using `fill_models` before running `portalcast`.
* Using `fill_models` would result in hand-made scripts being overwritten, so a specific function (`update_models`) for updating the models was created. 
* `update_models` by default only updates the models listed in the `controls_model` input, to avoid overwriting model scripts. To change this behavior and also update all of the prefab models' scripts, set `update_prefab_models = TRUE`. This is particularly handy when changing a global (with respect to model scripts) argument: `main`, `quiet`, `verbose`, or `arg_checks`.
* [addresses](https://github.com/weecology/portalcasting/issues/147)

## Messaging around trying to use not-complete directory improved
* Indication now made that a component of the directory is missing and suggestion is made to run `create_dir`.
* [addresses](https://github.com/weecology/portalcasting/issues/137)

## Patching data set bug in plotting
* There was a bug with matching the interpolated to the non interpolated data sets within the ensembling, which has been fixed. 
* [addresses](https://github.com/weecology/portalcasting/issues/140)

## Updated messaging 
* Moved most of the messaging into tidied functions.

## Changed behavior of `prep_rodents_table` and `prep_rodents` 
* Now there is no `start_moon` argument, and all of the data prior to `end_moon` are returned.
* This aligns the rodents prep functions with the other (moons, covariates) prep functions.
* Facilitates use of data prior to `start_moon` in forecasting models (e.g., for distributions of starting state variables).
* Requires that model functions now explicitly trim the rodents table being used. This has been added to all prefab models. 

## Fixed codecov targets
* Previous targets were restrictively high due to earlier near-perfect coverage.
* A codecov.yml file is now included in the repo (and ignored for the R build) which sets the target arbitrarily at the still-quite-high-but-not-restrictively-so 95%. 
* It can be changed if needed in the future.

## Simple EDM model added
* [addresses](https://github.com/weecology/portalcasting/issues/115)

## JAGS infrastructure added
* Using the runjags package, with extensive access to the API of `run.jags` via a `control_runjags` `list` (see `runjags_control`).
* Currently in place with a very simple random walk model. 
* [addresses](https://github.com/weecology/portalcasting/issues/142)

## Prepared rodents table includes more content
* Expanded back in time to the start.
* Added effort columns (all default options in `prefab_rodents_controls` have `effort = TRUE`).

## Updated adding a model and data vignette
* Added section at the end about just extending existing models to new data sets.
* [addresses](https://github.com/weecology/portalcasting/issues/145)

# [portalcasting 0.11.0](https://github.com/weecology/portalcasting/releases/tag/v0.11.0)
*2019-09-14*

## Ensembling reintroduced
* Associated with the reconfiguration of portalcasting from v0.8.1 to 0.9.0, ensembling was removed temporarily.
* A basic ensemble is reintroduced, now as an unweighted average across all selected models, allowing us to have an ensemble but not have it be tied to AIC weighting (because AIC weighting is no longer possible with the split between interpolated and non-interpolated data for model fitting).
* In a major departure from v0.8.1 and earlier, the ensemble's output is not saved like the actual models'. Rather, it is only calculated when needed on the fly.
* In plotting, it is now the default to use the ensemble for `plot_cast_ts` and `plot_cast_point` and for the ensemble to be included in `plot_casts_err_lead` and `plot_casts_cov_RMSE`.

## Return of `most_abundant_species`
* Function used to select the most common species.
* Now uses the actual data and not the casts to determine the species.

# [portalcasting 0.10.0](https://github.com/weecology/portalcasting/releases/tag/v0.10.0)
*2019-09-13*

## Model evaluation and ensembling added back in
* Were removed with the updated version from 0.8.1 to 0.9.0 to allow time to develop the code with the new infrastructure. 
* Model evaluation happens within the cast tab output as before.

## Temporarily removed figures returned
* Associated with the evaluation.
* Plotting of error as a function of lead time for multiple species and multiple models. Now has a fall-back arrangement that works for a single species-model combination.
* Plotting RMSE and coverage within species-model combinations.

## Flexing model controls to allow user-defined lists for prefab models
* For sandboxing with existing models, it is useful to be able to change a parameter in the model's controls, such as the data sets. Previously, that would require a lot of hacking around. Now, it's as simple as inputting the desired controls and flipping `arg_checks = FALSE`. 

# [portalcasting 0.9.0](https://github.com/weecology/portalcasting/releases/tag/v0.9.0)
*2019-09-06*

## Major API update: increase in explicit top-level arguments
* Moved key arguments to focal top-level inputs, rather than nested within control options list. Allows full control, but with default settings working cleanly. [addresses](https://github.com/weecology/portalcasting/issues/123)
* Restructuring of the controls lists, retained usage in situations where necessary: model construction, data set construction, file naming, climate data downloading.
* Openness for new `setup` functions, in particular `setup_sandbox`. [addresses](https://github.com/weecology/portalcasting/issues/125)
* Simplification of model naming inputs. Just put the names in you need, only use the `model_names` functions when you need to (usually in coding inside of functions or for setting default argument levels). [addresses](https://github.com/weecology/portalcasting/issues/119)

##  Directory tree structure simplified
* `dirtree` was removed
* `base` (both as a function and a concept) was removed. To make that structure use `main = "./name"`.
* "PortalData" has been removed as a sub and replaced with "raw", which includes all raw versions of files (post unzipping) downloaded: Portal Data and Portal Predictions and covariate forecasts (whose saving is also new here).

## Tightened messaging
* Expanded use of `quiet` and `verbose` connected throughout the pipeline.
* Additional messaging functions to reduce code clutter.
* Formatting of messages to reduce clutter and highlight the outline structure.

## Download capacity generalized
* Flexible interface to downloading capacity through a url, with generalized and flexible functions for generating Zenodo API urls (for retrieving the raw data and historical predictions) and NMME API urls (for retrieving weather forecasts) to port into the `download` function. [addresses](https://github.com/weecology/portalcasting/issues/121) and [addresses](https://github.com/weecology/portalcasting/issues/107) and [addresses](https://github.com/weecology/portalcasting/issues/53)

## Changes for users adding their own models to the prefab set
* Substantial reduction in effort for users who wish to add models (i.e. anyone who is sandboxing). You can even just plunk your own R script (which could be a single line calling out to an external program if desired) without having to add any model script writing controls, and just add the name of the model to the models argument in `portalcast` and it will run it with everything else.
* Outlined in the updated [Getting Started](https://weecology.github.io/portalcasting/articles/getting_started.html) and [Adding a Model/Data](https://weecology.github.io/portalcasting/articles/adding_model_and_data.html) vignettes.
* Users adding models to the prefab suite should now permanently add their model's control options to the source code in `model_script_controls` rather than write their own control functions.
* Users adding models to the prefab suite should permanently add their model's function code to the `prefab_models` script (reusing and adding to the documentation in `prefab_model_functions`), rather than to its own script. 
* Users should still add their model's name to the source code in `model_names`.

## Relaxed model requirements
* Models are no longer forced to use interpolated data.
* Models are no longer required to output a rigidly formatted data-table. Presently, the requirement is just a list, but soon some specifications will be added to improve reliability.
* Outlined in the updated [Adding a Model/Data](https://weecology.github.io/portalcasting/articles/adding_model_and_data.html) vignette. 

## More organization via metadata
* Generalized cast output is now tracked using a unique id in the file name associated with the cast, which is related to a row in a metadata table, newly included here. [addresses](https://github.com/weecology/portalcasting/issues/105) and [addresses](https://github.com/weecology/portalcasting/issues/106) and [addresses](https://github.com/weecology/portalPredictions/issues/316)
* Additional control information (like data set setup) is sent to the model metadata and saved out.
* Directory setting up configuration information is now tracked in a `dir_config.yaml` file, which is pulled from to save information about what was used to create, setup, and run the particular casts.

## Changes for users interested in analyzing their own data sets not in the standard data set configuration
* Users are now able to define rodent observation data sets that are not part of the standard data set ("all" and "controls", each also with interpolation of missing data) by giving the name in the `data_sets` argument and the controls defining the data set (used by portalr's `summarize_rodent_data` function) in the `controls_rodents` argument. 
* In order to actualize this, a user will need to flip off the argument checking (the default in a sandbox setting, if using a standard or production setting, set `arg_checks = FALSE` in the relevant function).
* Users interested in permanently adding the treatment level to the available data sets should add the source code to the `rodents_controls` function, just like with the models.
* [addresses](https://github.com/weecology/portalcasting/issues/133)
* Internal code points the pipeline to the files named via the data set inputs. The other data files are pointed to using the `control_files` (see `file_controls`) input list, which allows for some general flexibility with respect to what files the pipeline is reading in from the `data` subdirectory.

## Split of standard data sets 
* The prefab `all` and `controls` were both default being interpolated for all models because of the use of AIC for model comparison and ensemble building. That forced all models to use interpolated data.
* Starting in this version, the models are not required to have been fit in the same fashion (due to generalization of comparison and post-processing code), and so interpolation is not required if not needed, and we have split out the data to standard and interpolated versions.

## Application of specific models to specific data sets now facilitated
*  `write_model` and `model_template` have a `data_sets` argument that is used to write the code out, replacing the hard code requirement of analyzing "all" and "controls" for every model. Now, users who wish to analyze a particular data component can easily add it to the analysis pipeline. 

## Generalization of code terms
* Throughout the codebase, terminology has been generalized from "fcast"/"forecast"/"hindcast" to "cast" except where a clear distinction is needed (here primarily due to where the covariate values used come from).
* Nice benefits: highlights commonality between the two (see next section) and reduces code volume.
* `start_newmoon` is now `start_moon` like `end_moon`
* [addresses](https://github.com/weecology/portalcasting/issues/134)

## "Hindcasting" becomes more similar to "forecasting"
* In the codebase now, "hindcasting" is functionally "forecasting" with a forecast origin (`end_moon`) that is not the most recently occurring moon.
* Indeed, "hindcast" is nearly entirely removed from the codebase and "forecast" is nearly exclusively retained in documentation (and barely in the code itself), with both functionally being replaced with the generalized (and shorter) "cast". 
* `cast_type` is retained in the metadata file for posterity, but functionality is more generally checked by considering `end_moon` and `last_moon` in combination, where `end_moon` is the forecast origin and `last_moon` is the most recent 
* Rather than the complex machinery used to iterate through multiple forecasts ("hindcasting") that involved working backwards and skipping certain moons (which didn't need to be skipped anymore due to updated code from a while back that allows us to forecast fine even without the most recent samples yet), a simple for loop is able to manage iterating. This is also facilitated by the downloading of the raw portalPredictions repository from Zenodo and critically its retention in the "raw" subdirectory, which allows quick re-calculation of historic predictions of covariates. [addresses](https://github.com/weecology/portalcasting/issues/11)
* `cast_type` has been removed as an input, it's auto determined now based on `end_moon` and the last moon available (if they're equal it's a "forecast", if not it's a "hindcast").

## Softer handling of model failure
* Within `cast`, the model scripts are now sourced within a for-loop (rather than sapply) to allow for simple error catching of each script. [addresses](https://github.com/weecology/portalcasting/issues/22)

## Improved argument checking flow
* Arg checking is now considerably tighter, code-wise. 
* Each argument is either recognized and given a set of attributes (from an internally defined list) or unrecognized and stated to the user that it's not being checked (to help notify anyone building in the code that there's a new argument).
* The argument's attributes define the logical checking flow through a series of pretty simple options. 
* There is also now a `arg_checks` logical argument that goes into `check_args` to turn off all of the underlying code, enabling the user to go off the production restrictions that would otherwise through errors, even though they might technically work under the hood.

## Substantial re-writes of the vignettes
* Done in general to update with the present version of the codebase.
* Broke the `adding a model or data` vignette into "working locally" and "adding to the pipeline", also added checklists and screen shots. [addresses](https://github.com/weecology/portalcasting/issues/113)
* Reorganized the `getting started` vignette to an order that makes sense. [addresses](https://github.com/weecology/portalcasting/issues/120)

## Additional things
* `drop_spp` is now changed to `species` (so focus on inclusion, not exclusion). [addresses](https://github.com/weecology/portalcasting/issues/128)
* Improved examples, also now as `\donttest{}`. [addresses](https://github.com/weecology/portalcasting/issues/127)
* Tightened testing with `skip_on_cran` used judiciously. [addresses](https://github.com/weecology/portalcasting/issues/124)
* No longer building the AIC-based ensemble. [addresses](https://github.com/weecology/portalcasting/issues/102)
* Default confidence limit is now the more standard 0.95.

# [portalcasting 0.8.1](https://github.com/weecology/portalcasting/releases/tag/v0.8.1)
*2019-07-11*

## Hookup with Zenodo
Inclusion of json file and some minor editing of documentation, but no functional coding changes

# [portalcasting 0.8.0](https://github.com/weecology/portalcasting/releases/tag/v0.8.0)
*2019-03-21*

## `plot_cov_RMSE_mod_spp` now only plots the most recent -cast by default
* If `cast_dates = NULL` (the default), the plot only uses the most recent -cast
to avoid swamping more current -casts with historic -casts.

## Added specific checks for no casts returned in plot functions
* There's a bit of leeway with respect to argument validity, in particular around
model names (to facilitate users making new models with new names, we don't want
to hardwire a naming scheme in `check_arg`), so now there are checks to see if 
the tables returned from `select_casts` have any rows or not.

## Handling the edge cases in model function testing
* The trimming of the data sets for model function testing (happens in the AutoArima
test script) now includes addition of some dummy values for edge cases (all 0 
observations and nearly-all-0 observations), which allows better coverage of testing
for the -GARCH model functions in particular.

## Fixing a typo bug within `pevGARCH`
* There was a mismatch between `fcast` and `forecast` for one of the edge cases.

# [portalcasting 0.7.0](https://github.com/weecology/portalcasting/releases/tag/v0.7.0)
*2019-03-21*

## Addressing `nbGARCH` and `nbsGARCH` when even the Poisson fallback fails
* In `nbGARCH` and then extended into `nbsGARCH`, the models fall back
to a Poisson distribution if the negative binomial fit fails. Previously
(with only `nbGARCH`) the Poisson fit always succeeded in those back-ups,
but now (with `nbsGARCH`) that sometimes isn't the case (because the predictor
model is more complex) and even the Poisson fit can fail. So now for both 
models, if that fit fails, we follow what occurs in `pevGARCH` which is to
use the `fcast0` forecast of 0s and an arbitrarily high AIC (`1e6`).

# [portalcasting 0.6.0](https://github.com/weecology/portalcasting/releases/tag/v0.6.0)
*2019-03-20*

## Addressing covariate forecasts in `pevGARCH` under hindcasting
* `pevGARCH()` was not set up to leverage the `covariate_forecasts` file.
* It's now set up with a toggle based on the `cast_type` in the metadata list 
(which has replaced the formerly named `filename_suffix` element) to load
the `covariate_forecasts` file (using a new `read_covariate_forecasts` function)
and then select the specific hindcast based on the `source` and `date_made` columns
as selected by new elements in the metadata list (`covariate_source` and
`covariate_date_made`).

# [portalcasting 0.5.0](https://github.com/weecology/portalcasting/releases/tag/v0.5.0)
*2019-03-19* 

## Adding nbsGARCH
* Model `nbsGARCH` has been added to the base set of models.

## `foy` function
* `foy()` calculates the fraction of the year for a given date or set
of dates.

# [portalcasting 0.4.1](https://github.com/weecology/portalcasting/releases/tag/v0.4.1)
*2019-03-19*

## Move to usage of CRAN *portalr*
* To aid with stability, we're now using the [CRAN release of portalr](https://cran.r-project.org/package=portalr)

## `model_scripts` function
* Provides a simple way to list the scripts in the `models` subdirectory.

## Including the package version message in `setup_dir` and `portalcast`
* Including a simple message to report the version of portalcasting 
loaded in top level functions.

## Vignette updates
* Adding plot (from pre-constructed images) to the how-to vignette.

## Patching a bug in `model_template`
* There was a lingering old name from the argument switch over that was
causing model templates to be written with a `""` argument for the `model`
model name input into `save_forecast_output`.

# [portalcasting 0.4.0](https://github.com/weecology/portalcasting/releases/tag/v0.4.0)
*2019-03-16* 

## Tidied functionality for checking function arguments
* Introduction of `check_args` and `check_arg` which collaborate to
check the validity of function arguments using a standardized set
of requirements based on the argument names, thereby helping to unify
and standardize the use of the codebase's arguments.

## Updated function names
* `prep_rodents` is now  `prep_rodents_list`
* `rodents_data` is now `prep_rodents`
* `update_rodents` is now `update_rodents_list`
* `read_data` has been split out into `read_all`, `read_controls`,
`read_covariates`, `read_moons`, and `read_metadata`
* `model_path` is now `model_paths`
* `sub_path` and `sub_paths` have been merged into `sub_paths`, which 
returns all if `specific_subs` is NULL
* `lag_data` is now `lag_covariates`

## Updated argument (names to leverage `check_args`, etc.)
* In multiple functions `data` has been replaced with `rodents` to be
specific.
* `CI_level` is now subsumed by `confidence_level`
* `name` is now subsumed by `model`
* `set` is not split into `species_set` and `model_set`
* The order of arguments in `model_names` is now back to `model_set`, 
`add`.
* The default `subs_type` for `subdirs` is now `"portalcasting"`.
* The four model functions have a reduced set of inputs to leverage the 
directory tree, and the script generation is updated to match.
* Updating the `cast` argument to `cast_to_check` in `cast_is_valid` and 
removing the `verbose` argument from `verify_cast` to allow `check_arg` to 
leverage `check_arg` for `verify_cast`.

## Removal of classes
* The `models` class has been removed.
* The `subdirs` class has been removed.

## `messageq` function
* `messageq` function is added to tidy code around messages being printed
based on the `quiet` arguments.

## Inclusion of `"wEnsemble"` as an option in `model_names`
* Produces the `prefab` list with an `"Ensemble"` entry added, to allow for
that simply without using the `NULL` options, which collects all model names.
* This facilitated addition of `models` as an argument in the evaluations 
plots.

# [portalcasting 0.3.1](https://github.com/weecology/portalcasting/pull/93)
*2019-03-12* 

## Bug fix in `plot_cast_ts()`
* `plot_cast_ts` did not cleanly plot time series where observations had 
been made after the start of the prediction window.
* The function has been set up to now split observations that occurred
during the prediction window out, execute the plot as if they didn't 
exist, then add them on top. 
* Functionality has now been added to allow the toggling on and off of
those points via the `add_obs` input (defaults to `TRUE`).

# [portalcasting 0.3.0](https://github.com/weecology/portalcasting/releases/tag/v0.3.0)
*2019-03-04*

## Completed migration of plotting code
* `plot_cast` is now `plot_cast_ts` and is now fully vetted and tested
* `plotcastts_ylab` and `plotcastts_xaxis` provide tidied functions for
producing the y label and x axis (respectively) for `plot_cast_ts`.
* `plot_cast_point` is now added to replace `plot_species_forecast`.
* `plotcastpoint_yaxis` provides tidied functionality for the y axis of 
`plot_cast_point`.
* `select_most_ab_spp` allows for a simple selection of the most abundant
species from a -cast.
* `plot_err_lead_spp_mods` and `plot_cov_RMSE_mod_spp` now added to 
replace the raw code in the evaluation page.

## Processing of forecasts
* `read_casts` (old) is now `read_cast` and specifically works for only one -cast.
* `read_casts` (new) reads in multiple -casts.
* `select_cast` is now `select_casts` and allows a more flexible selection
by default.
* `make_ensemble` now returns a set of predictions with non-`NA` bounds when 
only one model is included (it returns that model as the ensemble).
* `most_recent_cast` returns the date of the most recent -cast. Can be dependent
on the presence of a census.
* `verify_cast` and `cast_is_valid` replace `forecast_is_valid` from the 
repo codebase. `verify_cast` is a logical wrapper on `cast_is_valid` that 
facilitates a pipeline integration. `cast_is_valid` does the major set of
checks of the cast data frame.  
* `append_observed_to_cast` is provided to add the observed data to the forecasts
and add columns for the raw error, in-forecast-window, and lead time as well.
* `measure_cast_error` allows for summarization of errors at the -cast level.

## Processing of data
* `most_recent_census` returns the date of the most recent census.

## Minor changes
* Argument order in `models` is reversed (`add` then `set`) and defaults in general
are now `NULL` and `NULL`, but `set = "prefab"` within the options functions, to
make it easy to run a novel model set.
* Argument order in `subdirs` is reversed (`subs` then `type`) and defaults in 
general are now `NULL` and `NULL`, but `type = "portalcasting"` within options
functions and `dirtree` to make it easier to manage a single subdirectory.
* `fdate` argument has been replaced throughout with `cast_date` for generality.

## Utilities
* `na_conformer` provides tidy functionality for converting non-character `NA` 
entries (can get read in from the data due to the `"NA"` species) to `"NA"`. 
Works for both vectors and data frames.

# [portalcasting 0.2.2](https://github.com/weecology/portalcasting/pull/82)
*2019-02-12* 

## Beginning to migrate plotting code
* `plot_cast` is developed but not yet fully vetted and tested, nor integrated
in the main repository. It will replace `forecast_viz` as a main plotting
function.

## Added `"moons"` to `read_data`
* `read_data`'s options have been expanded to include `"moons"`.
* Not fully implemented everywhere, but now available.

## Bug fix in `interpolate_data`
* `interpolate_data` was using `rodent_spp` in a way that assumed the `"NA"` 
species was coded as `"NA."`, which it wasn't. 
* Expansion of `rodent_spp` to include an `nadot` logical argument, with default
value of `FALSE`.

# [portalcasting 0.2.1](https://github.com/weecology/portalcasting/pull/81)
*2019-02-12* 

## Bug fix in `read_data`
* `read_data` was reading the All rodents file for Controls as well, which caused
the forecasts for the Controls to be duplicated of the All forecasts.
* Simple correction here.

# [portalcasting 0.2.0](https://github.com/weecology/portalcasting/pull/79)
*2019-02-04* 

## Code testing
* All of the code is now tested via [`testthat`](https://github.com/weecology/portalcasting/tree/master/tests).
* Test coverage is tracked via [Codecov](https://app.codecov.io/gh/weecology/portalcasting).
* The only functionality not covered in testing on Codecov is associated with
`download_predictions()`, which intermittently hangs on Travis. Testing is
available, but requires manual toggling of the `test_location` value to
`"local"` in the relevant test scripts (02-directory and 12-prepare_predictions).

## Enforcement of inputs
* Most of the functions previously did not have any checks on input argument 
classes, sizes, etc.
* Now all functions specifically check each argument's value for validity
and throw specific errors.

## Documentation
* All of the functions have fleshed out documentation that specify argument
requirements, link to each other and externally, and include more information.

## Data classes
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

## Added functions
* `classy()` allows for easy application of classes to objects in a `%>%` pipeline
* `read_data()` provides simple interface for loading and applying classes to
model-ready data files.
* `remove_incompletes()` removes any incomplete entries in a table, as defined
by an `NA` in a specific column.
* `check_options_args()` provides a tidy way to check the input arguments (wrt
class, length, numeric limitations, etc.) to the options functions.

## Vignettes
* Three vignettes were added:
  * [current models vignette](https://weecology.github.io/portalcasting/articles/current_models.html) was brought from the [forecasting website](https://portal.naturecast.org).
  * [codebase vignette](https://weecology.github.io/portalcasting/articles/codebase.html) was created from the earlier `README.md` file.
  * [adding a model vignette](https://weecology.github.io/portalcasting/articles/adding_model_and_data.html) was constructed based on two pages from the 
    [Portal Predictions repository](https://github.com/weecology/portalPredictions) wiki
    ([1](https://github.com/weecology/portalPredictions/wiki/Adding-a-new-model) and 
    [2](https://github.com/weecology/portalPredictions/wiki/Forecast-file-format)) and with substantial additional text added.

## Retention of all forecasts of covariates
* Previous versions retained only one covariate forecast per newmoon.
* We now enable retention of multiple covariate forecasts per newmoon and tag 
the forecast date with a time stamp as well.

## Website
* Added a website driven by [pkgdown](https://pkgdown.r-lib.org/).

## Changelog
* Developed this changelog as part of the package.

## Support documents
* Added [code of conduct](https://github.com/weecology/portalcasting/blob/master/CODE_OF_CONDUCT.md) 
and [contribution guidelines](https://github.com/weecology/portalcasting/blob/master/CONTRIBUTING.md)
to the repository.

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
own package.
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

# [portalcasting 0.0.0.1](https://github.com/weecology/portalPredictions/tree/ac032f3938a6695a8e5d27ee380032195b2af396) 
*2018-11-06*

* This is the last iteration of the code that now exists in portalcasting in
its previous home within the portalPredictions repo. 
* It was not referred to by the name portalcasting at the time.
