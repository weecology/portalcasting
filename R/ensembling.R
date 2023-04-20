#' @title Combine (Ensemble) Casts
#'
#' @description Combine multiple forecasts' output into a single ensemble. Presently, only a general average ensemble is available.
#'
#' @details A pre-loaded table of forecasts can be input, but if not (default), the table will be efficiently (as defined by the inputs) loaded and trimmed. \cr 
#'          The forecasts can be trimmed specifically using the `forecasts_ids` input, otherwise, all relevant forecasts from the stated `forecast_groups` will be included. 
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param method `character` value of the name of the ensemble method to use. Presently, only `"unwtavg"` (unweighted average) is allowed.
#'
#' @param forecasts_groups `integer` (or integer `numeric`) value of the forecasts groups to combine with an ensemble. If `NULL` (default), the most recent forecast group is ensembled. 
#'
#' @param forecasts_ids `integer` (or integer `numeric`) values representing the forecasts of interest for restricting ensembling, as indexed within the directory in the `casts` sub folder. See the forecasts metadata file (`forecasts_metadata.csv`) for summary information.
#'
#' @param historic_end_newmoonnumber `integer` (or integer `numeric`) newmoon number of the forecast origin. Default value is `NULL`, which equates to no selection.
#'
#' @param forecast_table Optional `data.frame` of forecast table outputs. If not input, will be loaded.
#'
#' @param models `character` value(s) of the name of the model to include. Default value is `NULL`, which equates to no selection with respect to `model`. `NULL` translates to all `models` in the table.
#'
#' @param dataset `character` value of the rodent data set to include Default value is `NULL`, which equates to the first data set encountered.
#'
#' @param species `character` vector of the species code(s) or `"total"` for the total across species) to be plotted `NULL` translates to the species defined by [`forecasting_species`][portalr::forecasting_species].
#'
#' @family core
#'
#' @return `data.frame` of ensembled forecasts.
#'
#' @name ensemble
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "ensemble")
#'    setup_production(main = main1)
#'
#'    forecasts_ids <- select_forecasts(main = main1, datasets = "controls", species = "DM")$forecast_id
#'
#'    ensemble_forecasts(main          = main1, 
#'                       forecasts_ids = forecasts_ids)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL


#' @rdname ensemble
#'
#' @export
#'
ensemble_forecasts <- function (main                       = ".", 
                                method                     = "unwtavg", 
                                forecasts_groups           = NULL, 
                                forecasts_ids              = NULL, 
                                forecast_table             = NULL, 
                                historic_end_newmoonnumber = NULL, 
                                models                     = NULL, 
                                dataset                    = NULL, 
                                species                    = NULL) {

  settings <- read_directory_settings(main = main)

  if (is.null(forecast_table)) {

    forecast_choices <- select_forecasts(main                        = main, 
                                         forecasts_ids               = forecasts_ids, 
                                         forecasts_groups            = forecasts_groups, 
                                         models                      = models, 
                                         species                     = species, 
                                         historic_end_newmoonnumbers = historic_end_newmoonnumber, 
                                         datasets                    = dataset)

    if (NROW(forecast_choices) == 0) {

      stop("no forecasts available for request")

    } else {

      forecast_table <- read_forecasts_tables(main          = main, 
                                              forecasts_ids = forecast_choices$forecast_id)
      forecast_table <- add_observations_to_forecast_table(main           = main,  
                                                           forecast_table = forecast_table)
      forecast_table$covered <- forecast_table$observation >= forecast_table$lower_pi & forecast_table$observation <= forecast_table$upper_pi 
      forecast_table$error   <- forecast_table$estimate - forecast_table$observation

    }

  }

  forecasts_ids                 <- ifnull(forecasts_ids, unique(forecast_table$forecast_id))
  models                        <- ifnull(models, unique(forecast_table$model)[1])
  dataset                       <- ifnull(dataset, unique(forecast_table$dataset)[1])
  species                       <- ifnull(species, unique(forecast_table$species)[1]) 
  historic_end_newmoonnumber    <- ifnull(historic_end_newmoonnumber, unique(forecast_table$historic_end_newmoonnumber)[1]) 
  forecast_id_in                <- forecast_table$forecast_id %in% forecasts_ids
  model_in                      <- forecast_table$model %in% models
  dataset_in                    <- forecast_table$dataset == dataset
  species_in                    <- forecast_table$species %in% species
  historic_end_newmoonnumber_in <- forecast_table$historic_end_newmoonnumber %in% historic_end_newmoonnumber
  all_in                        <- forecast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no forecasts available for requested plot")

  }

  forecast_table    <- forecast_table[all_in, ]

  nspecies    <- length(species)

  moons       <- unique(forecast_table$newmoonnumber)
  
  nmoons      <- length(moons)
  nmodels     <- length(models)
  weight      <- 1/nmodels
  estimate    <- rep(NA, nspecies * nmoons)
  mvar        <- rep(NA, nspecies * nmoons)
  l_pi        <- rep(NA, nspecies * nmoons)
  u_pi        <- rep(NA, nspecies * nmoons)
  obs         <- rep(NA, nspecies * nmoons)
  error       <- rep(NA, nspecies * nmoons)
  end_moon_id <- rep(NA, nspecies * nmoons)
  eforecast_id    <- rep(NA, nspecies * nmoons)
  species_id  <- rep(NA, nspecies * nmoons)
  moon_id     <- rep(NA, nspecies * nmoons)
  covered     <- rep(NA, nspecies * nmoons)
  nmodels     <- length(models)
  counter     <- 1

  for (i in 1:nspecies) {

    for (j in 1:nmoons) {

      species_in <- forecast_table$species %in% species[i]
      moon_in    <- forecast_table$newmoonnumber %in% moons[j]

      all_in     <- species_in & moon_in
      pcast_table  <- forecast_table[all_in, ]
      estimates  <- na.omit(pcast_table$estimate)

      if (length(estimates) > 0) {

        mvars             <- ((na.omit(pcast_table$upper_pi) - na.omit(pcast_table$estimate)) / (na.omit(pcast_table$confidence_level))) ^2
        estimate[counter] <- sum(estimates * weight)
        wt_ss             <- sum(weight * (estimates - estimate[counter]) ^ 2)

        if (nmodels > 1) {

          mvar[counter] <- sum((mvars * weight) + (wt_ss / (nmodels - 1)))

        } else {

          mvar[counter] <- mvars

        }

        CL  <- unique(na.omit(pcast_table$confidence_level))

        u_pi[counter]        <- estimate[counter] + sqrt(mvar[counter] * CL)
        l_pi[counter]        <- estimate[counter] - sqrt(mvar[counter] * CL)
        obs[counter]         <- unique(pcast_table$observation)
        error[counter]       <- estimate[counter] - obs[counter]
        
        end_moon_id[counter] <- unique(pcast_table$historic_end_newmoonnumber)

        eforecast_id[counter]    <- as.numeric(paste0(9999, min(as.numeric(pcast_table$forecast_id))))
        moon_id[counter]     <- moons[j]
        species_id[counter]  <- species[i]
        covered[counter]     <- estimate[counter] >= l_pi[counter] &
                                estimate[counter] <= u_pi[counter]
      }
      counter <- counter + 1
      
    }  
  }

  ensemble_name <- paste0("ensemble_", method)

  data.frame(origin                     = Sys.Date(),
             forecast_date                  = Sys.Date(),
             forecast_month                 = as.numeric(format(Sys.Date(), "%m")),
             forecast_year                  = as.numeric(format(Sys.Date(), "%Y")),
             currency                   = "abundance",
             model                      = ensemble_name, 
             newmoonnumber              = moon_id, 
             species                    = species_id,
             estimate                   = estimate, 
             var                        = mvar, 
             lower_pi                   = l_pi, 
             upper_pi                   = u_pi, 
             obs                        = obs, 
             error                      = error, 
             historic_end_newmoonnumber = end_moon_id, 
             lead_time_newmoons         = moon_id - end_moon_id, 
             dataset                    = dataset,
             forecast_id                    = eforecast_id,
             covered                    = covered)

 
}