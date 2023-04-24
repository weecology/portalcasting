

#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "runjags")
#'
#'    setup_dir(main = main1)
#'    dataset <- "all"
#'    species <- "DM"
#'    model   <- "runjags_RW"
#'  
#'    abundance      <- prepare_abundance(main    = main1,
#'                                        dataset = dataset,
#'                                        species = species,
#'                                        model   = model)
#'    model_controls <- models_controls(main       = main1,
#'                                      models     = model)[[model]]
#'    metadata       <- read_metadata(main        = main1)
#'    newmoons       <- read_newmoons(main        = main1)                                        
#'    covariates     <- read_covariates(main      = main1)
#'    control_runjags <- runjags_controls( )
#'    data_names      <- c("count", "N", "log_mean_count")
#'
#'    runjags_model(model = model)
#'
#'    runjags_monitors(monitors = c("mu", "sigma"),
#'                     metadata = metadata)
#'
#'    data <- runjags_data(data_names = data_names,
#'                         abundance  = abundance,
#'                         metadata   = metadata,
#'                         covariates = covariates)
#'
#'    runjags_inits(inits = list(mu    = rnorm(1, mean = data$log_mean_count, sd = 0.1),
#'                               sigma = runit(1, min  = 0.01, max = 0.5)))
#'
#'    fit_runjags <- function (abundance       = abundance, 
#'                             metadata        = metadata,, 
#'                             covariates      = covariates, 
#'                             monitors        = c("mu", "sigma"), 
#'                             inits           = list(mu    = rnorm(1, mean = data$log_mean_count, sd = 0.1),
#'                                                    sigma = runit(1, min  = 0.01, max = 0.5)), 
#'                             model           = model,
#'                             data_names      = data_names,
#'                             control_runjags = control_runjags)
#'  
#'    forecast(fit_runjags, h = metadata$lead_time_newmoons, level = metadata$confidence_level, nsamples = metadata$nsamples)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL
