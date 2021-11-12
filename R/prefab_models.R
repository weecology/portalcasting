#' @title Functions for the prefab portalcasting models
#'
#' @description Functions used for fitting the prefab models. For detailed
#'  descriptions of the models, see the
#'  \href{https://bit.ly/2xP9jKI}{model vignette}. \cr \cr
#'  \code{AutoArima} fits a flexible auto-regressive integrated 
#'  moving average model using \code{\link[forecast]{auto.arima}}. \cr \cr
#'  \code{NaiveArima} fits a baseline random walk with no drift model as an
#'  ARIMA(0,1,0) using \code{\link[forecast]{Arima}}. \cr \cr
#'  \code{ESSS} fits a flexible exponential smoothing state space 
#'  model with the possibility of multiplicative trends using 
#'  \code{\link[forecast]{ets}}. \cr \cr
#'  \code{nbGARCH} fits a generalized autoregresive conditional 
#'  heteroscedasticity model with overdispersion (\emph{i.e.}, a negative 
#'  binomial response variable) using \code{\link[tscount]{tsglm}}. \cr \cr
#'  \code{nbsGARCH} fits a seasonal generalized autoregresive conditional 
#'  heteroscedasticity model with overdispersion (\emph{i.e.}, a negative 
#'  binomial response variable) using \code{\link[tscount]{tsglm}}. \cr \cr
#'  \code{pevGARCH} fits a set of generalized autoregressive conditional
#'  heteroscedasticity models with Poisson response variables and 
#'  environmental covariates (max temp, mean temp, min temp, precipitation,
#'  and NDVI) using \code{\link[tscount]{tsglm}}. \cr \cr
#  \code{simplexEDM} fits an EDM model with the simplex projection "kernel" 
#  using \code{\link[rEDM]{simplex}}. \cr \cr
#  \code{GPEDM} fits an EDM model using Gaussian Processes for function 
#  approximation using \code{\link[rEDM]{tde_gp}}. \cr \cr
#'  \code{jags_RW} fits a log-scale density random walk with a Poisson 
#'  observation process using JAGS (Just Another Gibbs Sampler; 
#'  Plummer 2003) hierarchical Bayesian inference. 
#'
#' @details 
#'  \code{AutoArima} \cr
#'  Because the seasonality and sampling occurring with different 
#'  frequencies, which \code{\link[forecast]{auto.arima}} cannot accommodate,
#'  seasonal AutoArima models are not included.  \cr \cr
#'  \code{pevGARCH} \cr
#'  Environmental predictors have an optional \code{lag} time, which is 
#'  currently structured to impact all covariates the same and defaults to
#'  \code{6}. \cr
#'  All covariate models are fit and the best model (by AIC) is selected for
#'  use. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param data_set \code{character} value name of the rodent data set, such as 
#'  (\code{"all"} or \code{"controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @param lag \code{integer} (or integer \code{numeric}) of the lag time to
#'  use for the covariates.
#'  
# @param max_E \code{integer} (or integer \code{numeric}) for the maximum 
#  embedding dimension to search amongst for EDM models. See 
#  \code{\link[rEDM]{simplex}} for more information.
#'
#' @param control_runjags \code{list} of arguments passed to 
#'  \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages).
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return \code{list} of [1] model metadata \code{list} (\code{"metadata"}), 
#'  [2] cast summary \code{data.frame} (\code{"cast_tab"}),
#'  [3] \code{list} of model fit objects (\code{"model_fits"}), and 
#'  [4] \code{list} of model cast objects (\code{"model_casts"}).
#'
#' @references 
#'  Hyndman, R., Bergmeir, C., Caceres, G., Chhay, L., O'Hara-Wild, M., 
#'  Petropoulos, F., Razbash, S., Wang, E., and Yasmeen, F. 2018. 
#'  forecast: Forecasting functions for time series and linear models. 
#'  R package version 8.3. \href{http://pkg.robjhyndman.com/forecast}{URL}. 
#'
#'  Liboschik, T., Fokianos, K., and Fried, R. 2017. tscount: An R Package for 
#'  Analysis of Count Time Series Following Generalized Linear Models. 
#'  \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'  \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical 
#'  models using Gibbs Sampling. Proceedings of the 3rd International 
#.  Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X.
#'  \href{https://bit.ly/33aQ37Y}{URL}.
#'
#'  Ye, H., Clark, A., Deyle, E., Munch, S., Cai, J., Cowles, J., Daon, Y., 
#'  Edwards, A., Keyes, O., Stagge, J., Ushio, M., White, E., and Sugihara G. 
#'  2018. rEDM: Applications of Empirical Dynamic Modeling from Time Series. 
#'  Zenodo. R package version0.7.4 
#'  \href{http://doi.org/10.5281/zenodo.1935847}{URL}.
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
#'  }
#'
#' @name prefab_model_functions
#'
NULL

#' @rdname prefab_model_functions
#'
#' @export
#'
AutoArima <- function(main = ".", data_set = "all",  
                      quiet = FALSE, 
                      verbose = FALSE){

  data_set <- tolower(data_set)

  messageq("  -AutoArima for ", data_set, quiet = quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  data_set_controls <- metadata$controls_r[[data_set]]
  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)
    abund_s <- rodents_table[ , s]
    if(sum(abund_s, na.rm = TRUE) == 0){
      next()
    }
    mods[[i]] <- auto.arima(abund_s)
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = CL)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons,
                             currency = data_set_controls$output,
                             model = "AutoArima", data_set = data_set, 
                             species = ss, 
                             estimate = casts[[i]][ ,"Point.Forecast"], 
                             lower_pi = casts[[i]][ ,paste0("Lo.", CL * 100)], 
                             upper_pi = casts[[i]][ ,paste0("Hi.", CL * 100)], 
                             start_moon = metadata$start_moon,
                             end_moon = metadata$end_moon,
                             stringsAsFactors = FALSE)
    cast_tab <- rbind(cast_tab, cast_tab_s)
  }
  metadata <- update_list(metadata, models = "AutoArima",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}





#' @rdname prefab_model_functions
#'
#' @export
#'
NaiveArima <- function(main = ".", data_set = "all",  
                       control_files = files_control(), quiet = FALSE, 
                       verbose = FALSE){
  
  data_set <- tolower(data_set)

  messageq("  -NaiveArima for ", data_set, quiet = quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  data_set_controls <- metadata$controls_rodents[[data_set]]

  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)
    abund_s <- rodents_table[ , s]
    if(sum(abund_s, na.rm = TRUE) == 0){
      next()
    }
    mods[[i]] <- Arima(abund_s, order = c(0, 1, 0))
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = CL)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons,
                             currency = data_set_controls$output,
                             model = "NaiveArima", data_set = data_set, 
                             species = ss, 
                             estimate = casts[[i]][ ,"Point.Forecast"], 
                             lower_pi = casts[[i]][ ,paste0("Lo.", CL * 100)], 
                             upper_pi = casts[[i]][ ,paste0("Hi.", CL * 100)], 
                             start_moon = metadata$start_moon,
                             end_moon = metadata$end_moon,
                             stringsAsFactors = FALSE)
    cast_tab <- rbind(cast_tab, cast_tab_s)
  }
  metadata <- update_list(metadata, models = "NaiveArima",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}




#' @rdname prefab_model_functions
#' 
#' @export
#'
ESSS <- function(main = ".", data_set = "all_interp", 
                 control_files = files_control(), quiet = FALSE,
                 verbose = FALSE){
  
  data_set <- tolower(data_set)
  messageq("  -ESSS for ", data_set, quiet = quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  data_set_controls <- metadata$controls_rodents[[data_set]]

  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)
    abund_s <- rodents_table[ , s]
    if(sum(abund_s, na.rm = TRUE) == 0){
      next()
    }

    mods[[i]] <- ets(abund_s)
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = CL,
                           allow.multiplicative.trend = TRUE)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons,
                             currency = data_set_controls$output,
                             model = "ESSS", data_set = data_set, 
                             species = ss, 
                             estimate = casts[[i]][ ,"Point.Forecast"], 
                             lower_pi = casts[[i]][ ,paste0("Lo.", CL * 100)],
                             upper_pi = casts[[i]][ ,paste0("Hi.", CL * 100)],
                             start_moon = metadata$start_moon,
                             end_moon = metadata$end_moon,
                             stringsAsFactors = FALSE)
    cast_tab <- rbind(cast_tab, cast_tab_s)
  }
  metadata <- update_list(metadata, models = "ESSS",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}


#' @rdname prefab_model_functions
#'
#' @export
#'
nbGARCH <- function(main = ".", data_set = "all_interp",  
                    control_files = files_control(), quiet = FALSE,
                    verbose = FALSE){
  
  data_set <- tolower(data_set)
  messageq("  -nbGARCH for ", data_set, quiet = quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  data_set_controls <- metadata$controls_rodents[[data_set]]

  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, quiet = !verbose)
    abund_s <- rodents_table[ , s]
    if(sum(abund_s, na.rm = TRUE) == 0){
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
      casts[[i]] <- predict(mods[[i]], nmoons, level = CL)
      casts[[i]]$moon <- cast_moons
    }    

    estimate <- as.numeric(casts[[i]]$pred)
    lower_pi <- as.numeric(casts[[i]]$interval[ , 1]) 
    upper_pi <- as.numeric(casts[[i]]$interval[ , 2])

    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons,
                             currency = data_set_controls$output,
                             model = "nbGARCH", data_set = data_set, 
                             species = ss, 
                             estimate = estimate, 
                             lower_pi = lower_pi, 
                             upper_pi = upper_pi, 
                             start_moon = metadata$start_moon,
                             end_moon = metadata$end_moon,
                             stringsAsFactors = FALSE)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }
  metadata <- update_list(metadata, models = "nbGARCH",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}

#' @rdname prefab_model_functions
#'
#' @export
#'
nbsGARCH <- function(main = ".", data_set = "all_interp",  
                     control_files = files_control(), quiet = FALSE,
                     verbose = FALSE){
  
  data_set <- tolower(data_set)
  messageq("  -nbsGARCH for ", data_set, quiet = quiet)
  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  moons <- read_moons(main = main, control_files = control_files)
  moon_foys <- foy(dates = moons$moondate)
  sin2pifoy <- sin(2 * pi * moon_foys)
  cos2pifoy <- cos(2 * pi * moon_foys)
  fouriers <- data.frame(sin2pifoy, cos2pifoy)
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  data_set_controls <- metadata$controls_rodents[[data_set]]

  for_hist <- which(moons$moon %in% rodents_table$moon)
  for_cast <- which(moons$moon %in% cast_moons) 
  predictors <- fouriers[for_hist, ]
  cast_predictors <- fouriers[for_cast, ]

  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, !verbose)
    abund_s <- rodents_table[ , s]
    if(sum(abund_s, na.rm = TRUE) == 0){
      next()
    }
    past <- list(past_obs = 1, past_mean = 12)

    mods[[i]] <- tryCatch(
                   tsglm(abund_s, model = past, distr = "nbinom", 
                         xreg = predictors, link = "log"),
                   warning = function(x){NA}, 
                   error = function(x){NA})
    if(all(is.na(mods[[i]])) || AIC(mods[[i]]) == Inf){
      mods[[i]] <- tryCatch(
                     tsglm(abund_s, model = past, distr = "poisson", 
                           xreg = predictors, link = "log"),
                     warning = function(x){NA}, 
                     error = function(x){NA})
    }
    if(!all(is.na(mods[[i]]))){
      casts[[i]] <- predict(mods[[i]], nmoons, level = CL,
                            newxreg = cast_predictors)
      casts[[i]]$moon <- cast_moons
    }    

    estimate <- as.numeric(casts[[i]]$pred)
    lower_pi <- as.numeric(casts[[i]]$interval[ , 1]) 
    upper_pi <- as.numeric(casts[[i]]$interval[ , 2])

    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons,
                             currency = data_set_controls$output,
                             model = "nbsGARCH", data_set = data_set, 
                             species = ss, 
                             estimate = estimate, 
                             lower_pi = lower_pi, 
                             upper_pi = upper_pi, 
                             start_moon = metadata$start_moon,
                             end_moon = metadata$end_moon,
                             stringsAsFactors = FALSE)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }
  metadata <- update_list(metadata, models = "nbsGARCH",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}


#' @rdname prefab_model_functions
#' 
#' @export
#'
pevGARCH <- function(main = ".", data_set = "all_interp", lag = 6,  
                     control_files = files_control(), quiet = FALSE, 
                     verbose = FALSE){
  
  data_set <- tolower(data_set)
  messageq("  -pevGARCH for ", data_set, quiet = quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level

  covariates <- read_covariates(main = main, control_files = control_files)
  covar_lag <- lag_covariates(covariates, lag, tail = TRUE)
  for_hist <- which(covar_lag$moon %in% rodents_table$moon)
  for_cast <- which(covar_lag$moon %in% cast_moons) 
  covar_hist <- covar_lag[for_hist, ]
  if (metadata$end_moon == metadata$last_moon){
    covar_cast <- covar_lag[for_cast, ]
  } else {
    covariate_casts <- read_covariate_casts(main = main, 
                                            control_files = control_files,
                                            quiet = quiet, verbose = verbose)
    covar_casts_lag <- lag_covariates(covariate_casts, lag, tail = TRUE)
    last_cov_nm <- max(covar_hist$moon) - lag
    nm_in <- covar_casts_lag$cast_moon == last_cov_nm
    s_in <- covar_casts_lag$source == metadata$covariate_source
    dm_in <- covar_casts_lag$date_made == metadata$covariate_date_made
    covar_cast <- covar_casts_lag[nm_in & dm_in & s_in, ]
  }
  mods <- named_null_list(species)
  casts <- named_null_list(species)
  cast_tab <- data.frame()
  data_set_controls <- metadata$controls_rodents[[data_set]]      
  models <- covariate_models("pevGARCH")
  nmodels <- length(models)

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq("   -", ss, !verbose)
    abund_s <- rodents_table[ , s]
    if(sum(abund_s, na.rm = TRUE) == 0){
      next()
    }
    past <- list(past_obs = 1, past_mean = 12)
    model_count <- 1

    mods_i <- named_null_list(models)
    casts_i <- named_null_list(models)
    AICs <- rep(NA, nmodels)
    for(j in 1:nmodels){
      m <- models[j]
      model_name <- paste(m[[1]], collapse = ", ")
      model_name <- ifnull(model_name, "<intercept only>")
      messageq("    -", j, ": ", model_name, !verbose)
      predictors <- NULL
      cast_predictors <- NULL
      if (!(is.null(unlist(m)))){
        cols_in <- unlist(m) 
        predictors <- covar_hist[ , cols_in]
        cast_predictors <- covar_cast[ , cols_in]
      }
      mods_i[[j]] <- tryCatch(tsglm(abund_s, model = past, distr = "poisson",
                                    xreg = predictors, link = "log"), 
                             warning = function(x){NA}, 
                              error = function(x) {NA})
      if(!all(is.na(mods_i[[j]]))){
        casts_i[[j]] <-  tryCatch(predict(mods_i[[j]], nmoons, level = CL, 
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

    estimate <- as.numeric(casts[[i]]$pred)
    lower_pi <- as.numeric(casts[[i]]$interval[, 1]) 
    upper_pi <- as.numeric(casts[[i]]$interval[, 2])

    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons,
                             currency = data_set_controls$output,
                             model = "pevGARCH", data_set = data_set, 
                             species = ss, 
                             estimate = estimate, 
                             lower_pi = lower_pi, 
                             upper_pi = upper_pi, 
                             start_moon = metadata$start_moon,
                             end_moon = metadata$end_moon,
                             stringsAsFactors = FALSE)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }
  metadata <- update_list(metadata, models = "pevGARCH",
                              data_sets = data_set,
                              controls_r = data_set_controls)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}

