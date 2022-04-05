#' @title Provide the Names or Controls for the Prefab Models
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) models or a \code{list} of their controls
#'
#' @return \code{prefab_models}: \code{character} vector of model names. \cr
#'         \code{prefab_model_controls}: \code{list} vector of model controls. \cr
#'
#' @examples
#'  prefab_models()
#'  prefab_model_controls()
#'
#' @name prefabricated_models
#'
NULL

#' @rdname prefabricated_models
#'
#' @export
#'
prefab_model_controls <- function( ) {

  prefab_controls_file <- system.file("extdata", "prefab_model_controls.yaml", package = "portalcasting")

  read_yaml(prefab_controls_file)

}

#' @rdname prefabricated_models
#'
#' @export
#'
prefab_models <- function( ) {

  names(prefab_model_controls())

}


#' @title Functions for the prefab portalcasting models
#'
#' @description Functions used for fitting the prefab models. For detailed descriptions of the models, see the \href{https://bit.ly/2xP9jKI}{model vignette}. \cr \cr
#'  \code{AutoArima} fits a flexible auto-regressive integrated moving average model using \code{\link[forecast]{auto.arima}}. \cr \cr
#'  \code{NaiveArima} fits a baseline random walk with no drift model as an ARIMA(0,1,0) using \code{\link[forecast]{Arima}}. \cr \cr
#'  \code{ESSS} fits a flexible exponential smoothing state space model with the possibility of multiplicative trends using \code{\link[forecast]{ets}}. \cr \cr
#'  \code{nbGARCH} fits a generalized autoregresive conditional heteroscedasticity model with overdispersion (\emph{i.e.}, a negative binomial response variable) using \code{\link[tscount]{tsglm}}. \cr \cr
#'  \code{nbsGARCH} fits a seasonal generalized autoregresive conditional heteroscedasticity model with overdispersion (\emph{i.e.}, a negative binomial response variable) using \code{\link[tscount]{tsglm}}. \cr \cr
#'  \code{pevGARCH} fits a set of generalized autoregressive conditional heteroscedasticity models with Poisson response variables and  environmental covariates (max temp, mean temp, min temp, precipitation, and NDVI) using \code{\link[tscount]{tsglm}}. \cr \cr
#  \code{simplexEDM} fits an EDM model with the simplex projection "kernel" using \code{\link[rEDM]{simplex}}. \cr \cr
#  \code{GPEDM} fits an EDM model using Gaussian Processes for function approximation using \code{\link[rEDM]{tde_gp}}. \cr \cr
#'  \code{jags_RW} fits a log-scale density random walk with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. \cr \cr
#'  \code{jags_logistic} fits a log-scale density logistic-growth (r-K) model with a Poisson observation process using JAGS (Just Another Gibbs Sampler; Plummer 2003) hierarchical Bayesian inference. 
#'
#' @details 
#'  \code{AutoArima} \cr
#'  Because the seasonality and sampling occurring with different frequencies, which \code{\link[forecast]{auto.arima}} cannot accommodate, seasonal AutoArima models are not included.  \cr \cr
#'  \code{pevGARCH} \cr
#'  Environmental predictors have an optional \code{lag} time, which is currently structured to impact all covariates the same and defaults to \code{6}. \cr
#'  All covariate models are fit and the best model (by AIC) is selected for use. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param dataset \code{character} value name of the rodent data set, such as (\code{"all"} or \code{"controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be quiet.
#'
#' @param lag \code{integer} (or integer \code{numeric}) of the lag time to use for the covariates.
#'  
#' @param control_runjags \code{list} of arguments passed to \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{list} of \enumerate{
#'   \item{model metadata \code{list} (\code{"metadata"})}
#'   \item{cast summary \code{data.frame} (\code{"cast_tab"})}
#'   \item{\code{list} of model fit objects (\code{"model_fits"})}
#'   \item{\code{list} of model cast objects (\code{"model_casts"})}}
#'
#' @references 
#'  Hyndman, R., Bergmeir, C., Caceres, G., Chhay, L., O'Hara-Wild, M., Petropoulos, F., Razbash, S., Wang, E., and Yasmeen, F. 2018. forecast: Forecasting functions for time series and linear models. R package version 8.3. \href{http://pkg.robjhyndman.com/forecast}{URL}. 
#'
#'  Liboschik, T., Fokianos, K., and Fried, R. 2017. tscount: An R Package for Analysis of Count Time Series Following Generalized Linear Models. \emph{Journal of Statistical Software} \strong{82}:5, 1-51. \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical models using Gibbs Sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X. \href{https://bit.ly/33aQ37Y}{URL}.
#'
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   AutoArima()
#'   NaiveArima()
#'   ESSS()
#'   nbGARCH()
#'   nbsGARCH()
#'   pevGARCH()
#'   jags_RW()
#'   jags_logistic()
#'  }
#'
#' @name prefab_model_functions
#'
NULL

#' @rdname prefab_model_functions
#'
#' @export
#'
AutoArima <- function (main     = ".", 
                       dataset  = "all",
                       settings = directory_settings(), 
                       quiet    = FALSE, 
                       verbose  = FALSE) {

  dataset <- tolower(dataset)

  messageq("  -AutoArima for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)
  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)

    messageq("   -", ss, quiet = !verbose)

    abund_s <- rodents_table[ , s]

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    mods[[i]]  <- auto.arima(abund_s)
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = confidence_level)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date  = metadata$time$cast_date, 
                             cast_month = metadata$time$rodent_cast_months,
                             cast_year  = metadata$time$rodent_cast_years, 
                             moon       = metadata$time$rodent_cast_moons,
                             currency   = dataset_controls$args$output,
                             model      = "AutoArima", 
                             dataset    = dataset, 
                             species    = ss, 
                             estimate   = casts[[i]][ , "Point.Forecast"], 
                             lower_pi   = casts[[i]][ , paste0("Lo.", confidence_level * 100)], 
                             upper_pi   = casts[[i]][ , paste0("Hi.", confidence_level * 100)], 
                             start_moon = metadata$time$start_moon,
                             end_moon   = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, models           = "AutoArima",
                                    datasets         = dataset,
                                    dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}





#' @rdname prefab_model_functions
#'
#' @export
#'

NaiveArima <- function (main     = ".", 
                        dataset  = "all",
                        settings = directory_settings(), 
                        quiet    = FALSE, 
                        verbose  = FALSE) {

  dataset <- tolower(dataset)

  messageq("  -NaiveArima for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)
  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)

    messageq("   -", ss, quiet = !verbose)

    abund_s <- rodents_table[ , s]

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    mods[[i]]  <- Arima(abund_s, order = c(0, 1, 0))
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = confidence_level)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date  = metadata$time$cast_date, 
                             cast_month = metadata$time$rodent_cast_months,
                             cast_year  = metadata$time$rodent_cast_years, 
                             moon       = metadata$time$rodent_cast_moons,
                             currency   = dataset_controls$args$output,
                             model      = "NaiveArima", 
                             dataset    = dataset, 
                             species    = ss, 
                             estimate   = casts[[i]][ ,"Point.Forecast"], 
                             lower_pi   = casts[[i]][ ,paste0("Lo.", confidence_level * 100)], 
                             upper_pi   = casts[[i]][ ,paste0("Hi.", confidence_level * 100)], 
                             start_moon = metadata$time$start_moon,
                             end_moon   = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, models           = "NaiveArima",
                                    datasets         = dataset,
                                    dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}





#' @rdname prefab_model_functions
#' 
#' @export
#'

ESSS <- function (main     = ".", 
                  dataset  = "all_interp",
                  settings = directory_settings(), 
                  quiet    = FALSE, 
                  verbose  = FALSE) {

  dataset <- tolower(dataset)

  messageq("  -ESSS for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)
  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)

    messageq("   -", ss, quiet = !verbose)

    abund_s <- rodents_table[ , s]

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    mods[[i]]  <- ets(abund_s)
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = confidence_level,
                           allow.multiplicative.trend = TRUE)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date  = metadata$time$cast_date, 
                             cast_month = metadata$time$rodent_cast_months,
                             cast_year  = metadata$time$rodent_cast_years, 
                             moon       = metadata$time$rodent_cast_moons,
                             currency   = dataset_controls$args$output,
                             model      = "ESSS", 
                             dataset    = dataset, 
                             species    = ss, 
                             estimate   = casts[[i]][ , "Point.Forecast"], 
                             lower_pi   = casts[[i]][ , paste0("Lo.", confidence_level * 100)], 
                             upper_pi   = casts[[i]][ , paste0("Hi.", confidence_level * 100)], 
                             start_moon = metadata$time$start_moon,
                             end_moon   = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, models           = "ESSS",
                                    datasets         = dataset,
                                    dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}



#' @rdname prefab_model_functions
#'
#' @export
#'

nbGARCH <- function (main     = ".", 
                     dataset  = "all_interp",
                     settings = directory_settings(), 
                     quiet    = FALSE, 
                     verbose  = FALSE) {

  dataset <- tolower(dataset)

  messageq("  -nbGARCH for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)
  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)

    messageq("   -", ss, quiet = !verbose)

    abund_s <- rodents_table[ , s]

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    past <- list(past_obs = 1, past_mean = 12)
    mods[[i]] <- tryCatch(
                   tsglm(abund_s, model = past, distr = "nbinom", 
                         link = "log"),
                   warning = function(x){NA}, 
                   error = function(x){NA})
    if(all(is.na(mods[[i]])) || AIC(mods[[i]]) == Inf){
      mods[[i]] <- tryCatch(
                     tsglm(abund_s, model = past, distr = "poisson", 
                           link = "log"),
                     warning = function(x){NA}, 
                     error = function(x){NA})
    }
    if(!all(is.na(mods[[i]]))){
      casts[[i]] <- predict(mods[[i]], nmoons, level = confidence_level)
      casts[[i]]$moon <- cast_moons
    }    


    cast_tab_s <- data.frame(cast_date  = metadata$time$cast_date, 
                             cast_month = metadata$time$rodent_cast_months,
                             cast_year  = metadata$time$rodent_cast_years, 
                             moon       = metadata$time$rodent_cast_moons,
                             currency   = dataset_controls$args$output,
                             model      = "nbGARCH", 
                             dataset    = dataset, 
                             species    = ss, 
                             estimate   = as.numeric(casts[[i]]$pred), 
                             lower_pi   = as.numeric(casts[[i]]$interval[ , 1]), 
                             upper_pi   = as.numeric(casts[[i]]$interval[ , 2]), 
                             start_moon = metadata$time$start_moon,
                             end_moon   = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, models           = "nbGARCH",
                                    datasets         = dataset,
                                    dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}


#' @rdname prefab_model_functions
#'
#' @export
#'

nbsGARCH <- function (main     = ".", 
                      dataset  = "all_interp",
                      settings = directory_settings(), 
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  dataset <- tolower(dataset)

  messageq("  -nbsGARCH for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)
  moons <- read_moons(main     = main,
                            settings = settings)

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon
  moon_foys        <- foy(dates = moons$newmoondate)
  sin2pifoy        <- sin(2 * pi * moon_foys)
  cos2pifoy        <- cos(2 * pi * moon_foys)
  fouriers         <- data.frame(sin2pifoy, cos2pifoy)

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  for_hist         <- which(moons$newmoonnumber %in% rodents_table$newmoonnumber & moons$newmoonnumber >= start_moon)
  for_cast         <- which(moons$newmoonnumber %in% cast_moons) 
  predictors       <- fouriers[for_hist, ]
  cast_predictors  <- fouriers[for_cast, ]

  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)
  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()

  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)

    messageq("   -", ss, quiet = !verbose)

    abund_s <- rodents_table[ , s]

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    past <- list(past_obs = 1, past_mean = 12)
    mods[[i]] <- tryCatch(
                   tsglm(ts    = abund_s, 
                         model = past, 
                         distr = "nbinom", 
                         xreg  = predictors, 
                         link  = "log"),
                   warning = function(x){NA}, 
                   error   = function(x){NA})

    if (all(is.na(mods[[i]])) || AIC(mods[[i]]) == Inf) {

      mods[[i]] <- tryCatch(
                     tsglm(ts    = abund_s, 
                           model = past, 
                           distr = "poisson", 
                           xreg  = predictors, 
                           link  = "log"),
                     warning = function(x){NA}, 
                     error   = function(x){NA})
    }
    if (!all(is.na(mods[[i]]))) {

      casts[[i]] <- predict(object = mods[[i]], nmoons, level = confidence_level,
                            newxreg = cast_predictors)
      casts[[i]]$moon <- cast_moons

    }    


    cast_tab_s <- data.frame(cast_date  = metadata$time$cast_date, 
                             cast_month = metadata$time$rodent_cast_months,
                             cast_year  = metadata$time$rodent_cast_years, 
                             moon       = metadata$time$rodent_cast_moons,
                             currency   = dataset_controls$args$output,
                             model      = "nbsGARCH", 
                             dataset    = dataset, 
                             species    = ss, 
                             estimate   = as.numeric(casts[[i]]$pred), 
                             lower_pi   = as.numeric(casts[[i]]$interval[ , 1]), 
                             upper_pi   = as.numeric(casts[[i]]$interval[ , 2]), 
                             start_moon = metadata$time$start_moon,
                             end_moon   = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, models           = "nbsGARCH",
                                    datasets         = dataset,
                                    dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}



#' @rdname prefab_model_functions
#' 
#' @export
#'
pevGARCH <- function (main     = ".", 
                      dataset  = "all_interp",
                      lag      = 6,
                      settings = directory_settings(), 
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  dataset <- tolower(dataset)

  messageq("  -pevGARCH for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)
  covariates <- read_covariates(main     = main,
                            settings = settings)

  covar_lag <- lag_covariates(covariates = covariates, 
                              lag        = lag,
                              tail       = TRUE)

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  for_hist         <- which(covar_lag$newmoonnumber %in% rodents_table$newmoonnumber & covar_lag$newmoonnumber >= start_moon)
  for_cast         <- which(covar_lag$newmoonnumber %in% cast_moons) 
  covar_hist       <- covar_lag[for_hist, ]

  if (metadata$time$end_moon == metadata$time$last_moon) {

    covar_cast <- covar_lag[for_cast, ]

  } else {

    # removed for the time being
    stop(" retrieval of pre-existing covariate data not presently available!")

  }

  mods             <- named_null_list(species)
  casts            <- named_null_list(species)
  cast_tab         <- data.frame()
  dataset_controls <- metadata$dataset_controls[[dataset]]      
  models           <- covariate_models(model = "pevGARCH")
  nmodels          <- length(models)


  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)


  for (i in 1:nspecies) {

    s  <- species[i]
    ss <- gsub("NA.", "NA", s)

    messageq(paste0("   -", ss), quiet = !verbose)

    abund_s <- rodents_table[ , s]

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    past <- list(past_obs = 1, past_mean = 12)

    model_count <- 1


    mods_i  <- named_null_list(models)
    casts_i <- named_null_list(models)
    AICs    <- rep(NA, nmodels)

    for (j in 1:nmodels) {

      m          <- models[j]
      model_name <- paste(m[[1]], collapse = ", ")
      model_name <- ifnull(model_name, "<intercept only>")
      messageq("    -", j, ": ", model_name, quiet = !verbose)

      predictors      <- NULL
      cast_predictors <- NULL

      if (!(is.null(unlist(m)))) {

        cols_in         <- unlist(m) 
        predictors      <- covar_hist[ , cols_in]
        cast_predictors <- covar_cast[ , cols_in]

      }

      mods_i[[j]] <- tryCatch(tsglm(abund_s, model = past, distr = "poisson",
                                    xreg = predictors, link = "log"), 
                             warning = function(x){NA}, 
                              error = function(x) {NA})
      if(!all(is.na(mods_i[[j]]))){
        casts_i[[j]] <-  tryCatch(predict(mods_i[[j]], nmoons, level = confidence_level, 
                                          newxreg = cast_predictors),
                                 warning = function(x){NA}, 
                                  error = function(x) {NA})
      }
      AICs[j] <- tryCatch(AIC(mods_i[[j]]), 
                          error = function(x) {Inf})
    } 
    if(all(AICs == Inf)){
      next()
    }
    best_mod <- which.min(AICs)

    mods[[i]] <- mods_i[[best_mod]] 
    casts[[i]] <- casts_i[[best_mod]] 
    casts[[i]]$moon <- cast_moons


    cast_tab_s <- data.frame(cast_date  = metadata$time$cast_date, 
                             cast_month = metadata$time$rodent_cast_months,
                             cast_year  = metadata$time$rodent_cast_years, 
                             moon       = metadata$time$rodent_cast_moons,
                             currency   = dataset_controls$args$output,
                             model      = "pevGARCH", 
                             dataset    = dataset, 
                             species    = ss, 
                             estimate   = as.numeric(casts[[i]]$pred), 
                             lower_pi   = as.numeric(casts[[i]]$interval[ , 1]), 
                             upper_pi   = as.numeric(casts[[i]]$interval[ , 2]), 
                             start_moon = metadata$time$start_moon,
                             end_moon   = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, models           = "pevGARCH",
                                    datasets         = dataset,
                                    dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}
