# Create and Run a runjags Portalcasting Model

Using the runjags (Denwood 2016) package to produce JAGS-based
forecasts.  
`fit_runjags`: Wraps up the runjags model object preparation functions
with the model running
([`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html) function in
the runjags (Denwood 2016) package) we use to run JAGS (Plummer 2003)
models in portalcasting.  
`runjags_data`, `runjags_monitor`, `runjags_model`, `runjags_inits`:
Produce the model-specific components as named.  
`forecast.runjags`: A convenience function for extracting existing
forecasts from runjags objects and summarizing them into a
`"forecast"`-class object.  
`runjags_controls`: Combines the
[`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html) control
parameters that users may be interested in changing with a few
portalcasting-specific parameters into a control list for input into
specific model functions.

## Usage

``` r
runjags_inits(inits)

runjags_model(model_file)

runjags_monitors(monitors, metadata)

runjags_data(data_names, abundance, metadata, covariates)

fit_runjags(
  abundance,
  metadata,
  covariates,
  monitors,
  inits,
  model_file,
  data_names,
  control_runjags = runjags_controls()
)

# S3 method for class 'runjags'
forecast(object, h, level, nsamples, seed = NULL, ...)

runjags_controls(
  nchains = 4,
  adapt = 10000,
  burnin = 10000,
  sample = 10000,
  thin = 10,
  modules = "glm",
  method = "interruptible",
  factories = "",
  mutate = NA,
  silent_jags = FALSE
)
```

## Arguments

- inits:

  `list` of model parameter initializer functions. See
  [`prefab_models_controls`](https://weecology.github.io/portalcasting/reference/prefabricated-models.md).

- model_file:

  `character` value of the model file name. See
  [`prefab_models_controls`](https://weecology.github.io/portalcasting/reference/prefabricated-models.md).

- monitors:

  `character` vector of parameters to track. Forecasted observations and
  state variables are tracked automatically.

- metadata:

  `list` of model control elements. See
  [`prepare_metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md).

- data_names:

  `character` vector of data values to include in the data `list`. See
  [`prefab_models_controls`](https://weecology.github.io/portalcasting/reference/prefabricated-models.md).

- abundance:

  Non-negative `integer`-conformable vector of rodent abundances to use
  in forecasting. See
  [`prepare_abundance`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md).

- covariates:

  `data.frame` of covariates used in modeling. See
  [`prepare_covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md).

- control_runjags:

  `list` of controls for running runjags models. See `runjags_controls`.
  Optional. If not provided here, will be taken from the model controls
  list.

- object:

  A `runjags`-class object with columns of `"X"` values (state
  variables) in the the `mcmc` element.

- h:

  `integer`-conformable number of steps forward to include in the
  forecast.

- level:

  `numeric` of the confidence level to use in summarizing the
  predictions.

- nsamples:

  `integer` (or integer `numeric`) number of samples used to summarizing
  model output of sample-based estimates.

- seed:

  A single `integer`-conformable value or `NULL` set in
  [`set.seed`](https://rdrr.io/r/base/Random.html).

- ...:

  Additional parameters

- nchains:

  Non-negative `integer`-conformable value of the number of parallel
  chains to use. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- adapt:

  Non-negative `integer`-conformable value of the number of adaptation
  steps to use. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- burnin:

  Non-negative `integer`-conformable value of the number of burnin steps
  to use. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- sample:

  Non-negative `integer`-conformable value of the number of sampling
  steps to use. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- thin:

  Positive `integer`-conformable value of the thinning interval to use.
  See [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- modules:

  `character` vector of external modules to add to JAGS. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- method:

  `character` value of the
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html) method to
  use. Options include `"rjags"`, `"simple"`, `"interruptible"`,
  `"parallel"`, `"rjparallel"`, `"background"`, `"bgparallel"`, and
  `"snow"`. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- factories:

  `character` vector of factory modules to add to JAGS. See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- mutate:

  A `function` or `list` (with the first element being a `function`)
  used to add variables to the posterior chain (rather than throughout
  sampling). See
  [`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).

- silent_jags:

  `logical` value for quieting the output from the runjags function,
  including the underlying JAGS output.

## Value

`fit_runjags`: An object of class `"runjags"` of model components. See
[`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).  
`runjags_data`: A `list` of model-specific data for use in
[`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).  
`runjags_monitor`: A `vector` of model-specific `character` values of
parameters to track in
[`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).  
`runjags_model`: A single `character` value of the JAGS model block for
[`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).  
`runjags_inits`: A `function` that takes the argument `data` to produce
chain-specific initial values for
[`run.jags`](https://rdrr.io/pkg/runjags/man/run.jags.html).  
`runjags_controls`: A `list` of controls.  
`forecast.runjags`: `list` with `"forecast"`-class with named elements
including `"mean"`, `"lower"`, `"upper"`, and `"level"`.

## References

Denwood, M. J. 2016. runjags: an R package providing interface
utilities, model templates, parallel computing methods and additional
distributions for MCMC models in JAGS. Journal of Statistical Software,
71:9. [URL](https://www.jstatsoft.org/article/view/v071i09).

Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical
models using Gibbs Sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003). ISSN
1609-395X.

## See also

Helper functions for prefab models:
[`prefabricated models`](https://weecology.github.io/portalcasting/reference/prefabricated-models.md),
[`tsglm models`](https://weecology.github.io/portalcasting/reference/tsglm-models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "runjags")

   setup_dir(main = main1)
   dataset    <- "all"
   species    <- "DM"
   model      <- "jags_RW"
   model_file <- gsub("'", "",
                      models_controls(main1)[[model]]$fit$full_model_file)
 
   abundance      <- prepare_abundance(main    = main1,
                                       dataset = dataset,
                                       species = species,
                                       model   = model)
   model_controls <- models_controls(main       = main1,
                                     models     = model)[[model]]
   metadata       <- read_metadata(main        = main1)
   newmoons       <- read_newmoons(main        = main1)                                        
   covariates     <- read_covariates(main      = main1)
   control_runjags <- runjags_controls(nchains = 3, 
                                       adapt   = 1e3, 
                                       burnin  = 1e3, 
                                       sample  = 1e3, 
                                       thin    = 1)
   data_names      <- c("count", "N", "log_mean_count")

   runjags_model(model_file = model_file)

   runjags_monitors(monitors = c("mu", "sigma"),
                    metadata = metadata)

   data <- runjags_data(data_names = data_names,
                        abundance  = abundance,
                        metadata   = metadata,
                        covariates = covariates)

   runjags_inits(inits = list(mu    = rnorm(1, mean = data$log_mean_count, sd = 0.1),
                              sigma = runif(1, min  = 0.01, max = 0.5)))

   fit <- fit_runjags(abundance       = abundance, 
                      metadata        = metadata,
                      covariates      = covariates, 
                      monitors        = c("mu", "sigma"), 
                      inits           = list(mu    = rnorm(1, data$log_mean_count, 0.1),
                                             sigma = runif(1, 0.01, 0.5)), 
                      model_file      = model_file,
                      data_names      = data_names,
                      control_runjags = control_runjags)
 
   forecast(object   = fit,  
            h        = metadata$time$lead_time_newmoons,   
            level    = metadata$confidence_level,   
            nsamples = metadata$nsamples)

   unlink(main1, recursive = TRUE)
} # }
```
