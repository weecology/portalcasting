#' @title Combine (Ensemble) Casts
#'
#' @description Combine multiple casts' output into a single ensemble. Presently, only a general average ensemble is available.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), the table will be efficiently (as defined by the inputs) loaded and trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input, otherwise, all relevant casts from the stated \code{cast_groups} will be included. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param method \code{character} value of the name of the ensemble method to use. Presently, only \code{"unwtavg"} (unweighted average) is allowed.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast group to combine with an ensemble. If \code{NULL} (default), the most recent cast group is ensembled. 
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting ensembling, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param historic_end_newmoonnumber \code{integer} (or integer \code{numeric}) newmoon number of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param dataset \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to the first data set encountered.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{\link[portar]{forecasting_species}}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{data.frame} of ensembled casts.
#' 
#' @export
#'
ensemble_casts <- function (main                       = ".", 
                            settings                   = directory_settings( ), 
                            method                     = "unwtavg", 
                            cast_groups                = NULL, 
                            cast_ids                   = NULL, 
                            cast_tab                   = NULL, 
                            historic_end_newmoonnumber = NULL, 
                            models                     = NULL, 
                            dataset                    = NULL, 
                            species                    = NULL) {

  if (is.null(cast_tab)) {

    cast_choices <- select_casts(main                        = main, 
                                 settings                    = settings,
                                 cast_ids                    = cast_ids, 
                                 cast_groups                 = cast_groups, 
                                 models                      = models, 
                                 species                     = species, 
                                 historic_end_newmoonnumbers = historic_end_newmoonnumber, 
                                 datasets                    = dataset)

    if (NROW(cast_choices) == 0) {

      stop("no casts available for request")

    } else {

      cast_tab <- read_cast_tabs(main     = main, 
                                 settings = settings,
                                 cast_ids = cast_choices$cast_id)
      cast_tab <- add_obs_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
      cast_tab <- add_err_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
      cast_tab <- add_covered_to_cast_tab(main     = main,  
                                          settings = settings,
                                          cast_tab = cast_tab)

    }

  }

  cast_ids                      <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models                        <- ifnull(models, eval_models())
  dataset                       <- ifnull(dataset, unique(cast_tab$dataset)[1])
  species                       <- ifnull(species, "DM") 
  historic_end_newmoonnumber    <- ifnull(historic_end_newmoonnumber, unique(cast_tab$historic_end_newmoonnumber)) 
  cast_id_in                    <- cast_tab$cast_id %in% cast_ids
  model_in                      <- cast_tab$model %in% models
  dataset_in                    <- cast_tab$dataset == dataset
  species_in                    <- cast_tab$species %in% species
  if ("end_moon" %in% colnames(cast_tab)) {
    historic_end_newmoonnumber_in <- cast_tab$end_moon %in% historic_end_newmoonnumber
  } else if ("historic_end_newmoonnumber" %in% colnames(cast_tab)) {
    historic_end_newmoonnumber_in <- cast_tab$historic_end_newmoonnumber %in% historic_end_newmoonnumber
  }
  all_in                        <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no casts available for requested plot")

  }

  cast_tab    <- cast_tab[all_in, ]
  nspecies    <- length(species)

  if ("moon" %in% colnames(cast_tab)) {
    moons       <- unique(cast_tab$moon)
  } else if ("newmoonnumber" %in% colnames(cast_tab)) {
    moons       <- unique(cast_tab$newmoonnumber)
  }
  
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
  ecast_id    <- rep(NA, nspecies * nmoons)
  species_id  <- rep(NA, nspecies * nmoons)
  moon_id     <- rep(NA, nspecies * nmoons)
  covered     <- rep(NA, nspecies * nmoons)
  nmodels     <- length(models)
  counter     <- 1

  for (i in 1:nspecies) {

    for (j in 1:nmoons) {

      species_in <- cast_tab$species %in% species[i]
      
      if ("moon" %in% colnames(cast_tab)) {
        moon_in    <- cast_tab$moon %in% moons[j]
      } else if ("newmoonnumber" %in% colnames(cast_tab)) {
        moon_in    <- cast_tab$newmoonnumber %in% moons[j]
      }

      all_in     <- species_in & moon_in
      pcast_tab  <- cast_tab[all_in, ]
      estimates  <- na.omit(pcast_tab$estimate)

      if (length(estimates) > 0) {

        mvars             <- ((na.omit(pcast_tab$upper_pi) - na.omit(pcast_tab$estimate)) / (na.omit(pcast_tab$confidence_level))) ^2
        estimate[counter] <- sum(estimates * weight)
        wt_ss             <- sum(weight * (estimates - estimate[counter]) ^ 2)

        if (nmodels > 1) {

          mvar[counter] <- sum((mvars * weight) + (wt_ss / (nmodels - 1)))

        } else {

          mvar[counter] <- mvars

        }

        CL  <- unique(na.omit(pcast_tab$confidence_level))

        u_pi[counter]        <- estimate[counter] + sqrt(mvar[counter] * CL)
        l_pi[counter]        <- estimate[counter] - sqrt(mvar[counter] * CL)
        obs[counter]         <- unique(pcast_tab$obs)
        error[counter]       <- estimate[counter] - obs[counter]
        

        if ("end_moon" %in% colnames(cast_tab)) {
          end_moon_id[counter] <- unique(pcast_tab$end_moon)
        } else if ("historic_end_newmoonnumber" %in% colnames(cast_tab)) {
          end_moon_id[counter] <- unique(pcast_tab$historic_end_newmoonnumber)
        }

        ecast_id[counter]    <- as.numeric(paste0(9999, min(as.numeric(gsub("-", ".", pcast_tab$cast_id)))))
        moon_id[counter]     <- moons[j]
        species_id[counter]  <- species[i]
        covered[counter]     <- estimate[counter] >= l_pi[counter] &
                                estimate[counter] <= u_pi[counter]
      }
      counter <- counter + 1
      
    }  
  }

  ensemble_name <- paste0("ensemble_", method)




  data.frame(origin               = Sys.Date(),
             cast_date            = Sys.Date(),
             cast_month           = as.numeric(format(Sys.Date(), "%m")),
             cast_year            = as.numeric(format(Sys.Date(), "%Y")),
             currency             = "abundance",
             model                = ensemble_name, 
             newmoonnumber        = moon_id, 
             species              = species_id,
             estimate             = estimate, 
             var                  = mvar, 
             lower_pi             = l_pi, 
             upper_pi             = u_pi, 
             obs                  = obs, 
             error                = error, 
             historic_end_newmoon = end_moon_id, 
             lead_time_newmoons   = moon_id - end_moon_id, 
             dataset              = dataset,
             cast_id              = ecast_id,
             covered              = covered)





 
}