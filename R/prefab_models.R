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
#'  and NDVI) using \code{\link[tscount]{tsglm}}.
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. 
#'
#' @return \code{list} of [1] model metadata \code{list} (\code{"metadata"}), 
#'  [2] cast summary \code{data.frame} (\code{"cast_tab"}),
#'  [3] \code{list} of model fit objects (\code{"model_fits"}), and 
#'  [4] \code{list} of model cast objects (\code{"model_casts"}).
#'
#' @references 
#'  Hyndman R., Bergmeir C., Caceres G., Chhay L., O'Hara-Wild M., Petropoulos
#'  F., Razbash S., Wang E., and Yasmeen F. 2018. forecast: Forecasting 
#'  functions for time series and linear models. 
#'  \href{http://pkg.robjhyndman.com/forecast}{R package version 8.3}. 
#'
#'  Liboschik T., Fokianos K., and Fried R. 2017. tscount: An R Package for 
#'  Analysis of Count Time Series Following Generalized Linear Models. 
#'  \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'  \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
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
#'  }
#'
#' @name prefab_model_functions
NULL

#' @rdname prefab_model_functions
#'
#' @export
#'
AutoArima <- function(main = ".", data_set = "all", quiet = FALSE, 
                      arg_checks = TRUE){
  check_args(arg_checks)
  data_set <- tolower(data_set)

  messageq(paste0(" -AutoArima for ", data_set), quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, arg_checks = arg_checks)
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
    messageq(paste0("  -", ss), quiet)
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
  metadata <- update_list(metadata, model = "AutoArima",
                              data_set = data_set,
                              data_set_controls = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}





#' @rdname prefab_model_functions
#'
#' @export
#'
NaiveArima <- function(main = ".", data_set = "all", quiet = FALSE, 
                      arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_set <- tolower(data_set)

  messageq(paste0(" -NaiveArima for ", data_set), quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, arg_checks = arg_checks)
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
    messageq(paste0("  -", ss), quiet)
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
  metadata <- update_list(metadata, model = "NaiveArima",
                              data_set = data_set,
                              data_set_controls = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}




#' @rdname prefab_model_functions
#' 
#' @export
#'
ESSS <- function(main = ".", data_set = "all_interp", quiet = FALSE,
                 arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_set <- tolower(data_set)
  messageq(paste0(" -ESSS for ", data_set), quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, arg_checks = arg_checks)
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
    messageq(paste0("  -", ss), quiet)
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
  metadata <- update_list(metadata, model = "ESSS",
                              data_set = data_set,
                              data_set_controls = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}


#' @rdname prefab_model_functions
#'
#' @export
#'
nbGARCH <- function(main = ".", data_set = "all_interp", quiet = FALSE,
                    arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_set <- tolower(data_set)
  messageq(paste0(" -nbGARCH for ", data_set), quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, arg_checks = arg_checks)
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
    messageq(paste0("  -", ss), quiet)
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
  metadata <- update_list(metadata, model = "nbGARCH",
                              data_set = data_set,
                              data_set_controls = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}

#' @rdname prefab_model_functions
#'
#' @export
#'
nbsGARCH <- function(main = ".", data_set = "all_interp", quiet = FALSE,
                    arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_set <- tolower(data_set)
  messageq(paste0(" -nbsGARCH for ", data_set), quiet)
  rodents_table <- read_rodents_table(main = main, data_set = data_set,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, arg_checks = arg_checks)
  moons <- read_moons(main = main, arg_checks = arg_checks)
  moon_foys <- foy(dates = moons$moondate, arg_checks = arg_checks)
  sin2pifoy <- sin(2 * pi * moon_foys)
  cos2pifoy <- cos(2 * pi * moon_foys)
  fouriers <- data.frame(sin2pifoy, cos2pifoy)
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  data_set_controls <- metadata$controls_r[[data_set]]

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
    messageq(paste0("  -", ss), quiet)
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
  metadata <- update_list(metadata, model = "nbsGARCH",
                              data_set = data_set,
                              data_set_controls = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}


#' @rdname prefab_model_functions
#' 
#' @export
#'
pevGARCH <- function(main = ".", data_set = "all_interp", lag = 6, 
                     quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  data_set <- tolower(data_set)
  messageq(paste0(" -pevGARCH for ", data_set), quiet)

  rodents_table <- read_rodents_table(main = main, data_set = data_set,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, arg_checks = arg_checks)
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level

  covariates <- read_covariates(main = main, arg_checks = arg_checks)
  covar_lag <- lag_covariates(covariates, lag, tail = TRUE)
  for_hist <- which(covar_lag$moon %in% rodents_table$moon)
  for_cast <- which(covar_lag$moon %in% cast_moons) 
  covar_hist <- covar_lag[for_hist, ]
  if (metadata$end_moon == metadata$last_moon){
    covar_cast <- covar_lag[for_cast, ]
  } else {
    covariate_casts <- read_covariate_casts(main)
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
  data_set_controls <- metadata$controls_r[[data_set]]      
  models <- covariate_models("pevGARCH")
  nmodels <- length(models)

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq(paste0("  -", ss), quiet)
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
      messageq(paste0("   -", j, ": ", model_name), quiet)
      predictors <- NULL
      cast_predictors <- NULL
      if (!(is.null(unlist(m)))){
        predictors <- select(covar_hist, unlist(m))
        cast_predictors <- select(covar_cast, unlist(m))
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
  metadata <- update_list(metadata, model = "pevGARCH",
                              data_set = data_set,
                              data_set_controls = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}

