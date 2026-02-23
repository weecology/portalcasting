# Create, Run, and Forecast Multi-model Inference TSGLM Model Runs

Using the tscount (Liboschik et al. 2017) package to forecast time
series of counts.  
`meta_tsglm`: Combines the model running with the covariate preparation
functions for a multi-model
[`tsglm`](https://rdrr.io/pkg/tscount/man/tsglm.html) (from the tscount
(Liboschik et al. 2017) package) model.  
`forecast.tsglm`: A wrapper around the `predict` function for tsglm
objects that produces a `"forecast"`-class object.

## Usage

``` r
meta_tsglm(
  ts,
  model,
  distr,
  link,
  lag,
  submodels,
  covariates,
  metadata,
  quiet = FALSE
)

# S3 method for class 'tsglm'
forecast(object, h, level, ...)
```

## Arguments

- ts:

  Non-negative `integer`-conformable vector of rodent abundances to use
  in forecasting. See
  [`prepare_abundance`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md).

- model:

  A named `list` of model linear predictors. See
  [`tsglm`](https://rdrr.io/pkg/tscount/man/tsglm.html).

- distr:

  `character` of the response distribution. See
  [`tsglm`](https://rdrr.io/pkg/tscount/man/tsglm.html).

- link:

  `character` of the link function. See
  [`tsglm`](https://rdrr.io/pkg/tscount/man/tsglm.html).

- lag:

  `integer`-conformable value of the number of timesteps used in a bulk
  lagging for all covariates in all submodels.

- submodels:

  `list` of `character` vectors defining the covariates to include in
  each of the submodels.

- covariates:

  `data.frame` of covariates used in modeling. See
  [`prepare_covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md).

- metadata:

  `list` of model control elements. See
  [`prepare_metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md).

- quiet:

  `logical` indicator controlling if messages are printed.

- object:

  A `tsglm`-class object.

- h:

  `integer`-conformable number of steps forward to forecast. Passed into
  `predict` as `n.ahead`.

- level:

  `numeric` of the confidence level to use in summarizing the
  predictions.

- ...:

  Additional parameters passed into `predict`.

## Value

`meta_tsglm`: An object of class `"tsglm"` with additional elements
defining the submodel and lag.  
`forecast.tsglm`: `list` with `"forecast"`-class with named elements
including `"mean"`, `"lower"`, `"upper"`, and `"newxreg"` (if provided
for prediction) as well as the other elements returned by `predict`.

## References

Liboschik T., K. Fokianos, and R. Fried. 2017. tscount: An R Package for
Analysis of Count Time Series Following Generalized Linear Models.
Journal of Statistical Software, 82:1-51.
[URL](https://doi.org/10.18637/jss.v082.i05).

## See also

Helper functions for prefab models:
[`prefabricated models`](https://weecology.github.io/portalcasting/reference/prefabricated-models.md),
[`runjags models`](https://weecology.github.io/portalcasting/reference/runjags-models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "metatsglm")

   setup_dir(main = main1)
   dataset <- "all"
   species <- "DM"
   model   <- "pevGARCH"
 
   abundance      <- prepare_abundance(main    = main1,
                                       dataset = dataset,
                                       species = species,
                                       model   = model)
   model_controls <- models_controls(main       = main1,
                                     models     = model)[[model]]
   metadata       <- read_metadata(main        = main1)
   newmoons       <- read_newmoons(main        = main1)                                        
   covariates     <- read_covariates(main      = main1)
   model          <- list(past_obs = 1, past_mean = 13)
   distr          <- "poisson"
   link           <- "log"
   lag            <- 6
   submodels      <- list(c("mintemp", "ndvi"),
                          c("maxtemp"),
                          c("meantemp"),
                          c("precipitation"),
                          c(NULL))

   fit_tsglm      <- meta_tsglm(ts         = abundance, 
                                model      = model, 
                                distr      = distr, 
                                link       = link, 
                                lag        = lag, 
                                submodels  = submodels, 
                                covariates = covariates, 
                                metadata   = metadata, 
                                quiet      = FALSE)
   newmoons_in <- match(metadata$time$forecast_newmoonnumbers - lag, covariates$newmoonnumber)
   newxreg     <- covariates[newmoons_in, unlist(fit_tsglm$submodel)]

   forecast(object  = fit_tsglm,   
            h       = metadata$time$lead_time_newmoons,   
            level   = metadata$confidence_level,   
            newxreg = newxreg)

   unlink(main1, recursive = TRUE)
} # }
```
