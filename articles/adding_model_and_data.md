# Adding a Model and Data

## Overview

The **portalcasting** package provides the ability to add functions both
to a local copy of the repository for testing as well as to contribute
to the base set of models provided within the package (and thus executed
in the main repository). Similarly, users may often want or need to
analyze the data in a slightly different configuration than what is
already available. Here, we walk through the steps to add user-defined
models and data sets to the directory for local use or for integration
within the production pipeline.

For the purposes here, consider that you are interested in adding a
model named “newmod” to the forecasting suite. While “newmod” can work
on the existing data, you are are really interested in a slightly
different configuration (perhaps only the long-term kangaroo rat
exclosures) of the data you call “newdata”.

Here, we assume the user has already run through a basic installation,
set up, and evaluation of the package, as covered in the [Getting
Started](https://weecology.github.io/portalcasting/articles/getting_started.html)
vignette.

## Model requirements

Unlike previous versions of **portalcasting**, starting with v0.9.0,
there are very few formal requirements for the output of a model. The
requirements have been further relaxed starting with v0.51.0, where we
now leverage the model controls lists to track and document most
functionality.

The forecasting pipeline will work with basically any univariate model
fitting function that will run in R, and produce an object that is then
capable of being processed by the forecasting function to forecast the
model’s predictions, so long as they can be processed via
[`process_model_output()`](https://weecology.github.io/portalcasting/reference/process-forecast-output.md),
which only requires `mean`, `lower`, and `upper` elements. The vast
majority of the information saved about the forecasts is located in the
metadata files and controls lists, resulting in the model itself not
needing to produce much specific output to be valid.

Models can be based on either pre-existing functions (e.g., the
`AutoArima` model uses `auto.arima` from the **forecasting** package) or
specialized functions (such as those designed for the `runjags` models
in **portalcasting**, which are collated into `fit_runjags`). Each
model-dataset-species combination is run in
[`cast()`](https://weecology.github.io/portalcasting/reference/portalcast.md)
wrapped in a [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
call, which softens any errors in implementation. The
[`cast()`](https://weecology.github.io/portalcasting/reference/portalcast.md)
call runs a `do.call` implementation of the fit and forecast functions
from the model controls list and then passes the output of the two
functions to `process_model_output`.

## Set up the directory

To allow for a more flexible environment, here we use the
[`setup_sandbox()`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
function to make and fill our directory, which we house at
`"~/sandbox"`:

``` r
library(portalcasting)
main <- "~/sandbox"
setup_sandbox(main = main)
```

The controls list for the standard, prefabricated (“prefab”) models that
come with the **portalcasting** package is automatically added to the
directory’s `models` sub folder, as are the model script files for the
`runjags` models.

Similarly, the prefabricated data sets that come with the
**portalcasting** package are automatically added to the `data`
subdirectory.

## Adding a model

A new model is added to the pipeline via (at least) additional model
controls (with additional files if needed). A starting template for
model controls is provided in the **portalcasting** core files and
accessible via
[`model_controls_template()`](https://weecology.github.io/portalcasting/reference/new-models.md):

``` r
model_controls_template()
#> $metadata
#> $metadata$name
#> [1] "model_name"
#> 
#> $metadata$print_name
#> [1] "model name"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fit
#> $fit$fun
#> NULL
#> 
#> $fit$args
#> NULL
#> 
#> 
#> $forecast
#> $forecast$fun
#> [1] "forecast"
#> 
#> $forecast$args
#> $forecast$args$object
#> [1] "model_fit"
#> 
#> $forecast$args$h
#> [1] "metadata$time$lead_time_newmoons"
#> 
#> $forecast$args$level
#> [1] "metadata$confidence_level"
#> 
#> 
#> 
#> $interpolate
#> $interpolate$needed
#> [1] FALSE
#> 
#> 
#> $datasets
#> $datasets$all
#> $datasets$all$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PL"    "PM"    "PP"    "RM"    "RO"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$controls
#> $datasets$controls$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PM"    "PP"    "RM"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$exclosures
#> $datasets$exclosures$species
#>  [1] "BA"    "NA"    "OL"    "OT"    "PB"    "PE"    "PF"    "PM"    "PP"   
#> [10] "RM"    "SF"    "SH"    "total"
#> 
#> 
#> 
#> $response
#> $response$link
#> NULL
#> 
#> $response$type
#> NULL
#> 
#> $response$scoring_family
#> NULL
#> 
#> 
#> $time
#> [1] "newmoon"
```

### Metadata

One can create custom controls using a suite of `new_model_<>`
functions, each of which wraps a call to a specific component of the
`model_controls_template` inside an
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)
call:

``` r
new_model_controls()
#> $metadata
#> $metadata$name
#> [1] "model_name"
#> 
#> $metadata$print_name
#> [1] "model name"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fit
#> $fit$fun
#> NULL
#> 
#> $fit$args
#> NULL
#> 
#> 
#> $forecast
#> $forecast$fun
#> [1] "forecast"
#> 
#> $forecast$args
#> $forecast$args$object
#> [1] "model_fit"
#> 
#> $forecast$args$h
#> [1] "metadata$time$lead_time_newmoons"
#> 
#> $forecast$args$level
#> [1] "metadata$confidence_level"
#> 
#> 
#> 
#> $interpolate
#> $interpolate$needed
#> [1] FALSE
#> 
#> 
#> $datasets
#> $datasets$all
#> $datasets$all$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PL"    "PM"    "PP"    "RM"    "RO"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$controls
#> $datasets$controls$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PM"    "PP"    "RM"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$exclosures
#> $datasets$exclosures$species
#>  [1] "BA"    "NA"    "OL"    "OT"    "PB"    "PE"    "PF"    "PM"    "PP"   
#> [10] "RM"    "SF"    "SH"    "total"
#> 
#> 
#> 
#> $response
#> $response$link
#> NULL
#> 
#> $response$type
#> NULL
#> 
#> $response$scoring_family
#> NULL
#> 
#> 
#> $time
#> [1] "newmoon"

new_model_metadata()
#> $name
#> [1] "model_name"
#> 
#> $print_name
#> [1] "model name"
#> 
#> $tags
#> list()
#> 
#> $text
#> NULL

new_model_metadata(name = "newmod")
#> $name
#> [1] "newmod"
#> 
#> $print_name
#> [1] "model name"
#> 
#> $tags
#> list()
#> 
#> $text
#> NULL

new_model_controls(metadata = new_model_metadata(name = "newmod", print_name = "New Model"))
#> $metadata
#> $metadata$name
#> [1] "newmod"
#> 
#> $metadata$print_name
#> [1] "New Model"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fit
#> $fit$fun
#> NULL
#> 
#> $fit$args
#> NULL
#> 
#> 
#> $forecast
#> $forecast$fun
#> [1] "forecast"
#> 
#> $forecast$args
#> $forecast$args$object
#> [1] "model_fit"
#> 
#> $forecast$args$h
#> [1] "metadata$time$lead_time_newmoons"
#> 
#> $forecast$args$level
#> [1] "metadata$confidence_level"
#> 
#> 
#> 
#> $interpolate
#> $interpolate$needed
#> [1] FALSE
#> 
#> 
#> $datasets
#> $datasets$all
#> $datasets$all$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PL"    "PM"    "PP"    "RM"    "RO"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$controls
#> $datasets$controls$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PM"    "PP"    "RM"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$exclosures
#> $datasets$exclosures$species
#>  [1] "BA"    "NA"    "OL"    "OT"    "PB"    "PE"    "PF"    "PM"    "PP"   
#> [10] "RM"    "SF"    "SH"    "total"
#> 
#> 
#> 
#> $response
#> $response$link
#> NULL
#> 
#> $response$type
#> NULL
#> 
#> $response$scoring_family
#> NULL
#> 
#> 
#> $time
#> [1] "newmoon"
```

### Fit and Cast Functions

At the very least, each new model will need a fitting function, which
can be provided using the
[`new_model_fit()`](https://weecology.github.io/portalcasting/reference/new-models.md),
with elements for the function and arguments:

``` r
new_model_fit(fun = "arima", args = list(x = "abundance"))
#> $fun
#> [1] "arima"
#> 
#> $args
#> $args$x
#> [1] "abundance"

new_model_controls(metadata = new_model_metadata(name = "newmod", print_name = "New Model"),
                   fit      = new_model_fit(fun = "arima", args = list(x = "abundance")))
#> $metadata
#> $metadata$name
#> [1] "newmod"
#> 
#> $metadata$print_name
#> [1] "New Model"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fit
#> $fit$fun
#> [1] "arima"
#> 
#> $fit$args
#> $fit$args$x
#> [1] "abundance"
#> 
#> 
#> 
#> $forecast
#> $forecast$fun
#> [1] "forecast"
#> 
#> $forecast$args
#> $forecast$args$object
#> [1] "model_fit"
#> 
#> $forecast$args$h
#> [1] "metadata$time$lead_time_newmoons"
#> 
#> $forecast$args$level
#> [1] "metadata$confidence_level"
#> 
#> 
#> 
#> $interpolate
#> $interpolate$needed
#> [1] FALSE
#> 
#> 
#> $datasets
#> $datasets$all
#> $datasets$all$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PL"    "PM"    "PP"    "RM"    "RO"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$controls
#> $datasets$controls$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PM"    "PP"    "RM"    "SF"    "SH"    "total"
#> 
#> 
#> $datasets$exclosures
#> $datasets$exclosures$species
#>  [1] "BA"    "NA"    "OL"    "OT"    "PB"    "PE"    "PF"    "PM"    "PP"   
#> [10] "RM"    "SF"    "SH"    "total"
#> 
#> 
#> 
#> $response
#> $response$link
#> NULL
#> 
#> $response$type
#> NULL
#> 
#> $response$scoring_family
#> NULL
#> 
#> 
#> $time
#> [1] "newmoon"
```

Because the `arima` function already has a defined `forecast` method, we
do not need to update the `cast` element of the model controls, as it is
already pre-loaded:

``` r
new_model_forecast()
#> $fun
#> [1] "forecast"
#> 
#> $args
#> $args$object
#> [1] "model_fit"
#> 
#> $args$h
#> [1] "metadata$time$lead_time_newmoons"
#> 
#> $args$level
#> [1] "metadata$confidence_level"
```

### Datasets

By default, any new model is set to run for the three prefab datasets
(all, exclosures, controls), for each relevant species (e.g, not
including the kangaroo rats in the exclosures):

``` r
new_model_datasets()
#> $all
#> $all$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PL"    "PM"    "PP"    "RM"    "RO"    "SF"    "SH"    "total"
#> 
#> 
#> $controls
#> $controls$species
#>  [1] "BA"    "DM"    "DO"    "DS"    "NA"    "OL"    "OT"    "PB"    "PE"   
#> [10] "PF"    "PM"    "PP"    "RM"    "SF"    "SH"    "total"
#> 
#> 
#> $exclosures
#> $exclosures$species
#>  [1] "BA"    "NA"    "OL"    "OT"    "PB"    "PE"    "PF"    "PM"    "PP"   
#> [10] "RM"    "SF"    "SH"    "total"
```

If the specific model needs to use interpolated data, the
[`new_model_interpolate()`](https://weecology.github.io/portalcasting/reference/new-models.md)
function should be given an argument `needed = TRUE`, and then a
function input, which allows for invoking specialized functions. A
simple example that is used for the prefab models is
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
which would be implemented as:

``` r
new_model_interpolate(needed = TRUE, fun = "round_na.interp")
#> $needed
#> [1] TRUE
#> 
#> $fun
#> [1] "round_na.interp"
```

which would just be added as the `interpolate` element in
`new_model_controls`.

The default, however, is `needed = FALSE`:

``` r
new_model_interpolate()
#> $needed
#> [1] FALSE
```

### Model Response

The response distribution for a model is a key aspect to scoring it
properly. We store information about the response in the list for
evaluation purposes:

``` r
new_model_controls()$response
#> $link
#> NULL
#> 
#> $type
#> NULL
#> 
#> $scoring_family
#> NULL
```

The options for each component are as follows:

`link`: normal, negative_binomial, poisson `type`: distribution,
empirical `scoring_family`: normal, nbinom, poisson, sample

``` r
new_model_response(link = "normal", type = "distribution", scoring_family = "normal")
#> $link
#> [1] "normal"
#> 
#> $type
#> [1] "distribution"
#> 
#> $scoring_family
#> [1] "normal"
```

### Adding the Model

The specific updates can be called together to generate the model
controls list for newmod:

``` r
new_controls <- new_model_controls(metadata = new_model_metadata(name = "newmod", print_name = "New Model"),
                                   fit      = new_model_fit(fun = "arima", args = list(x = "abundance")),
                                   response = new_model_response(link = "normal", type = "distribution", scoring_family = "normal"))
```

Given that the directory is already established, one can use the
[`add_new_model()`](https://weecology.github.io/portalcasting/reference/new-models.md)
function to add the model to the directory at main, via the controls
list:

``` r
new_controls <- new_model_controls(metadata = new_model_metadata(name = "newmod", print_name = "New Model"),
                                   fit      = new_model_fit(fun = "arima", args = list(x = "abundance")),
                                   response = new_model_response(link = "normal", type = "distribution", scoring_family = "normal"))


added <- add_new_model(main = main, new_model_controls = new_controls)


names(read_models_controls(main = main))

#>  [1] "AutoArima"                            "sAutoArima"                           "ESSS"                                
#>  [4] "NaiveArima"                           "sNaiveArima"                          "nbGARCH"                             
#>  [7] "nbsGARCH"                             "pGARCH"                               "psGARCH"                             
#> [10] "pevGARCH"                             "jags_RW"                              "jags_logistic"                       
#> [13] "jags_logistic_covariates"             "jags_logistic_competition"            "jags_logistic_competition_covariates"
#> [16] "newmod"
```

And the model is directly ready to be `portalcast`ed:

``` r
portalcast(main     = main, 
           models   = "newmod", 
           datasets = "all", 
           species  = c("DM", "PP", "total"))

#> ------------------------------------------------------------
#> Forecasting models...
#> ------------------------------------------------------------
#> This is portalcasting v0.51.0
#> ------------------------------------------------------------
#> 
#>   - newmod for all DM
#>     |++++| successful |++++|
#>   - newmod for all PP
#>     |++++| successful |++++|
#>   - newmod for all total
#>     |++++| successful |++++|
#> ------------------------------------------------------------
#> ...forecasting complete.
#> ------------------------------------------------------------
```

### From the `setup` Stage

Incorporating the model in the establishment of the directory requires
adding the controls list to the initial `setup_<>` call:

``` r
main2 <- "~/sandbox2"
setup_sandbox(main                = main2, 
              new_models_controls = list(newmod = new_controls), 
              models              = c(prefab_models(), "newmod"))
```

## Adding a Dataset

A new dataset is added to the pipeline much in the same way as a new
model, but with only a single generating function and fewer elements in
the controls list:

``` r
dataset_controls_template()
#> $metadata
#> $metadata$name
#> [1] "dataset_name"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fun
#> [1] "prepare_dataset"
#> 
#> $args
#> $args$name
#> [1] "dataset_name"
#> 
#> $args$species
#>  [1] "BA" "DM" "DO" "DS" "NA" "OL" "OT" "PB" "PE" "PF" "PH" "PL" "PM" "PP" "RF"
#> [16] "RM" "RO" "SF" "SH" "SO"
#> 
#> $args$total
#> [1] TRUE
#> 
#> $args$clean
#> [1] FALSE
#> 
#> $args$type
#> [1] "Rodents"
#> 
#> $args$level
#> [1] "Site"
#> 
#> $args$plots
#> [1] "all"
#> 
#> $args$treatment
#> NULL
#> 
#> $args$min_plots
#> [1] 24
#> 
#> $args$min_traps
#> [1] 1
#> 
#> $args$output
#> [1] "abundance"
#> 
#> $args$fillweight
#> [1] FALSE
#> 
#> $args$unknowns
#> [1] FALSE
#> 
#> $args$time
#> [1] "newmoon"
#> 
#> $args$na_drop
#> [1] FALSE
#> 
#> $args$zero_drop
#> [1] FALSE
#> 
#> $args$effort
#> [1] TRUE
#> 
#> $args$filename
#> [1] "rodents_dataset_name.csv"
```

### Metadata

One can create custom controls using a suite of `new_data_<>` functions,
each of which wraps a call to a specific component of the
`data_controls_template` inside an
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)
call:

``` r
new_dataset_controls()
#> $metadata
#> $metadata$name
#> [1] "dataset_name"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fun
#> [1] "prepare_dataset"
#> 
#> $args
#> $args$name
#> [1] "dataset_name"
#> 
#> $args$species
#>  [1] "BA" "DM" "DO" "DS" "NA" "OL" "OT" "PB" "PE" "PF" "PH" "PL" "PM" "PP" "RF"
#> [16] "RM" "RO" "SF" "SH" "SO"
#> 
#> $args$total
#> [1] TRUE
#> 
#> $args$clean
#> [1] FALSE
#> 
#> $args$type
#> [1] "Rodents"
#> 
#> $args$level
#> [1] "Site"
#> 
#> $args$plots
#> [1] "all"
#> 
#> $args$treatment
#> NULL
#> 
#> $args$min_plots
#> [1] 24
#> 
#> $args$min_traps
#> [1] 1
#> 
#> $args$output
#> [1] "abundance"
#> 
#> $args$fillweight
#> [1] FALSE
#> 
#> $args$unknowns
#> [1] FALSE
#> 
#> $args$time
#> [1] "newmoon"
#> 
#> $args$na_drop
#> [1] FALSE
#> 
#> $args$zero_drop
#> [1] FALSE
#> 
#> $args$effort
#> [1] TRUE
#> 
#> $args$filename
#> [1] "rodents_dataset_name.csv"

new_dataset_metadata()
#> $name
#> [1] "dataset_name"
#> 
#> $tags
#> list()
#> 
#> $text
#> NULL

new_dataset_metadata(name = "newdata")
#> $name
#> [1] "newdata"
#> 
#> $tags
#> list()
#> 
#> $text
#> NULL

new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"))
#> $metadata
#> $metadata$name
#> [1] "newdata"
#> 
#> $metadata$tags
#> list()
#> 
#> $metadata$text
#> NULL
#> 
#> 
#> $fun
#> [1] "prepare_dataset"
#> 
#> $args
#> $args$name
#> [1] "dataset_name"
#> 
#> $args$species
#>  [1] "BA" "DM" "DO" "DS" "NA" "OL" "OT" "PB" "PE" "PF" "PH" "PL" "PM" "PP" "RF"
#> [16] "RM" "RO" "SF" "SH" "SO"
#> 
#> $args$total
#> [1] TRUE
#> 
#> $args$clean
#> [1] FALSE
#> 
#> $args$type
#> [1] "Rodents"
#> 
#> $args$level
#> [1] "Site"
#> 
#> $args$plots
#> [1] "all"
#> 
#> $args$treatment
#> NULL
#> 
#> $args$min_plots
#> [1] 24
#> 
#> $args$min_traps
#> [1] 1
#> 
#> $args$output
#> [1] "abundance"
#> 
#> $args$fillweight
#> [1] FALSE
#> 
#> $args$unknowns
#> [1] FALSE
#> 
#> $args$time
#> [1] "newmoon"
#> 
#> $args$na_drop
#> [1] FALSE
#> 
#> $args$zero_drop
#> [1] FALSE
#> 
#> $args$effort
#> [1] TRUE
#> 
#> $args$filename
#> [1] "rodents_dataset_name.csv"
```

### Generating Function and Its Arguments

All of the existing datasets use the same generating function
[`prepare_dataset()`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md),
which is quite flexible and ports arguments directly to the generalized
`summarize_rodent_data()` from the **portalr** package. We therefore
include this function as the default in new dataset controls:

``` r
new_dataset_fun()
#> [1] "prepare_dataset"
```

although that can be changed to whatever generating function a user may
want to implement.

The arguments to the function can be updated via the
[`new_dataset_args()`](https://weecology.github.io/portalcasting/reference/new-datasets.md)
function:

``` r
new_dataset_args(name = "newdata")
#> $name
#> [1] "newdata"
#> 
#> $species
#>  [1] "BA" "DM" "DO" "DS" "NA" "OL" "OT" "PB" "PE" "PF" "PH" "PL" "PM" "PP" "RF"
#> [16] "RM" "RO" "SF" "SH" "SO"
#> 
#> $total
#> [1] TRUE
#> 
#> $clean
#> [1] FALSE
#> 
#> $type
#> [1] "Rodents"
#> 
#> $level
#> [1] "Site"
#> 
#> $plots
#> [1] "all"
#> 
#> $treatment
#> NULL
#> 
#> $min_plots
#> [1] 24
#> 
#> $min_traps
#> [1] 1
#> 
#> $output
#> [1] "abundance"
#> 
#> $fillweight
#> [1] FALSE
#> 
#> $unknowns
#> [1] FALSE
#> 
#> $time
#> [1] "newmoon"
#> 
#> $na_drop
#> [1] FALSE
#> 
#> $zero_drop
#> [1] FALSE
#> 
#> $effort
#> [1] TRUE
#> 
#> $filename
#> [1] "rodents_dataset_name.csv"
```

### Adding the Dataset

These can all be wrapped up together in a call to
[`new_dataset_controls()`](https://weecology.github.io/portalcasting/reference/new-datasets.md)
to create the controls list that is then passed into
[`add_new_dataset()`](https://weecology.github.io/portalcasting/reference/new-datasets.md):

``` r
new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
                                     args     = new_dataset_args(name     = "newdata", 
                                                                 filename = "rodents_newdata.csv"))
```

Given that the directory is already established, one can use the
[`add_new_dataset()`](https://weecology.github.io/portalcasting/reference/new-datasets.md)
function to add the dataset controls to the directory at main, via the
controls list, noting which existing models should have the new dataset
added to their controls list:

``` r
new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
                                     args     = new_dataset_args(name     = "newdata", 
                                                                 filename = "rodents_newdata.csv"))

added <- add_new_dataset(main                 = main, 
                         new_dataset_controls = new_controls, 
                         models               = "AutoArima"))


names(read_datasets_controls(main = main))

#>  [1] "all"        "controls"   "exclosures" "newdata"
```

And the dataset can then be forecast with:

``` r
portalcast(main     = main, 
           models   = "AutoArima", 
           datasets = "newdata", 
           species  = c("DM", "PP", "total"))

#> ------------------------------------------------------------
#> Forecasting models...
#> ------------------------------------------------------------
#> This is portalcasting v0.51.0
#> ------------------------------------------------------------
#> 
#>   - AutoArima for newdata DM
#>     |++++| successful |++++|
#>   - AutoArima for newdata PP
#>     |++++| successful |++++|
#>   - AutoArima for newdata total
#>     |++++| successful |++++|
#> ------------------------------------------------------------
#> ...forecasting complete.
#> ------------------------------------------------------------
```

### From the `setup` Stage

Incorporating the dataset in the establishment of the directory requires
adding the controls list to the initial `setup_<>` call:

``` r
main3 <- "~/sandbox3"
setup_sandbox(main                  = main3, 
              new_datasets_controls = list(newdata = new_controls), 
              datasets              = c(prefab_datasets(), "newdata"))
```
