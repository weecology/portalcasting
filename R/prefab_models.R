#' @title Flexible Auto-Regressive Integrated Moving Average model for Portal 
#'  Predictions
#'
#' @description Fit an AutoArima model in the portalcasting pipeline. 
#'
#' @details Model "AutoArima" is a flexible auto-regressive integrated 
#'  moving average model fit to the data using 
#'  \code{\link[forecast]{auto.arima}} in the
#'  \href{http://pkg.robjhyndman.com/forecast/}{\code{forecast} package}
#'  (Hyndman \emph{et al}. 2018). \cr
#'  Because the seasonality and sampling occurring with different frequencies,
#'  which \code{\link[forecast]{auto.arima}} cannot accommodate, seasonal 
#'  models are not included. \cr
#'  Although \code{\link[forecast]{auto.arima}} can handle missing data, the
#'  other models used currently cannot, so we interpolate missing data here 
#'  for comparison.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param level \code{character} value name of the type of plots included 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @return \code{list} of [1] \code{"cast"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references 
#'  Hyndman R., Bergmeir C., Caceres G., Chhay L., O'Hara-Wild M., Petropoulos
#'  F., Razbash S., Wang E., and Yasmeen F. 2018. forecast: Forecasting 
#'  functions for time series and linear models. 
#'  \href{http://pkg.robjhyndman.com/forecast}{R package version 8.3}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   AutoArima()
#'  }
#' 
#' @export
#'
AutoArima <- function(main = ".", level = "All", quiet = FALSE){
  messageq(paste0("### Fitting AutoArima model for ", level, " ###"), quiet)

  abundances <- read_rodents(main, level = tolower(level))
  metadata <- read_metadata(main)
 
  nmoons <- length(metadata$rodent_cast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "newmoon")]
  cast <- data.frame()
  aic <- data.frame()

  for (s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting AutoArima model for ", ss), quiet)
    abund_s <- abundances[[s]]

    if (sum(abund_s) == 0){
      model_cast <- cast0(nmoons, "mean")
      model_aic <- 1e6
      estimate <- as.numeric(model_cast$mean)
      LowerPI <- rep(0, nmoons)
      UpperPI <- rep(0, nmoons)
    } else{
      model <- auto.arima(abund_s)
      model_cast <- forecast(model, h = nmoons, level = CL, fan = TRUE)
      model_aic <- model$aic
      estimate <- as.numeric(model_cast$mean)
      CI_match <- which(model_cast$level == CL * 100)
      LowerPI <- as.numeric(model_cast$lower[ , CI_match]) 
      UpperPI <- as.numeric(model_cast$upper[ , CI_match])
    }    

    cast_s <- data.frame(date = metadata$cast_date, 
                         castmonth = metadata$rodent_cast_months,
                         castyear = metadata$rodent_cast_years, 
                         newmoonnumber = metadata$rodent_cast_newmoons,
                         currency = "abundance", model = "AutoArima", 
                         level = level, species = ss, estimate = estimate, 
                         LowerPI = LowerPI, UpperPI = UpperPI, 
                         fit_start_newmoon = min(abundances$newmoon),
                         fit_end_newmoon = max(abundances$newmoon),
                         initial_newmoon = max(abundances$newmoon),
                         stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$cast_date, currency = "abundance", 
                        model = "AutoArima", level = level, species = ss, 
                        aic = model_aic, 
                        fit_start_newmoon = min(abundances$newmoon),
                        fit_end_newmoon = max(abundances$newmoon),
                        initial_newmoon = max(abundances$newmoon),
                        stringsAsFactors = FALSE)

    cast <- rbind(cast, cast_s)
    aic <- rbind(aic, aic_s)
  }
  list("cast" = cast, "aic" = aic)
}


#' @title Exponential Smoothing State Space model for Portal Predictions
#'
#' @description Fit an ESSS model in the portalcasting pipeline.
#'
#' @details Model "ESSS" is a flexible exponential smoothing state space 
#'  model with the possibility of multiplicative trends fit using 
#'  \code{\link[forecast]{ets}} in the
#'  \href{http://pkg.robjhyndman.com/forecast/}{\code{forecast} package}
#'  (Hyndman \emph{et al}. 2018).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param level \code{character} value name of the type of plots included 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @return \code{list} of [1] \code{"cast"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references 
#'  Hyndman R., Bergmeir C., Caceres G., Chhay L., O'Hara-Wild M., Petropoulos
#'  F., Razbash S., Wang E., and Yasmeen F. 2018. forecast: Forecasting 
#'  functions for time series and linear models. 
#'  \href{http://pkg.robjhyndman.com/forecast}{R package version 8.3}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   ESSS()
#'  }
#' 
#' @export
#'
ESSS <- function(main = ".", level = "All", quiet = FALSE){
  messageq(paste0("### Fitting ESSS model for ", level, " ###"), quiet)

  abundances <- read_rodents(main, level = tolower(level))
  metadata <- read_metadata(main)

  nmoons <- length(metadata$rodent_cast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "newmoon")]
  cast <- data.frame()
  aic <- data.frame()

  for (s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting ESSS model for ", ss), quiet)
    abund_s <- abundances[[s]]
  
    if (sum(abund_s) == 0){
      model_cast <- cast0(nmoons, "mean")
      model_aic <- 1e6
      estimate <- as.numeric(model_cast$mean)
      LowerPI <- rep(0, nmoons)
      UpperPI <- rep(0, nmoons)
    } else{
      model <- ets(abund_s)
      model_cast <- forecast(model, h = nmoons, level = CL, 
                       allow.multiplicative.trend = TRUE)
      model_aic <- model$aic
      estimate <- as.numeric(model_cast$mean)
      CI_match <- which(model_cast$level == CL * 100)
      LowerPI <- as.numeric(model_cast$lower[ , CI_match]) 
      UpperPI <- as.numeric(model_cast$upper[ , CI_match])
    }    

    cast_s <- data.frame(date = metadata$cast_date, 
                         castmonth = metadata$rodent_cast_months,
                         castyear = metadata$rodent_cast_years, 
                         newmoonnumber = metadata$rodent_cast_newmoons,
                         currency = "abundance", model = "ESSS", 
                         level = level, species = ss, estimate = estimate, 
                         LowerPI = LowerPI, UpperPI = UpperPI, 
                         fit_start_newmoon = min(abundances$newmoon),
                         fit_end_newmoon = max(abundances$newmoon),
                         initial_newmoon = max(abundances$newmoon),
                         stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$cast_date, currency = "abundance", 
                        model = "ESSS", level = level, species = ss, 
                        aic = model_aic, 
                        fit_start_newmoon = min(abundances$newmoon),
                        fit_end_newmoon = max(abundances$newmoon),
                        initial_newmoon = max(abundances$newmoon),
                        stringsAsFactors = FALSE)

    cast <- rbind(cast, cast_s)
    aic <- rbind(aic, aic_s)
  }
  list("cast" = cast, "aic" = aic)
}

#' @title negative binomial Generalized AutoRegressive Conditional
#'  Heteroscedasticity model for Portal Predictions
#'
#' @description Fit an nbGARCH model in the portalcasting pipeline.
#'
#' @details Model "nbGARCH" is a generalized autoregresive conditional 
#'  heteroscedasticity model with overdispersion (\emph{i.e.}, a negative 
#'  binomial response variable) fit to the data using 
#'  \code{\link[tscount]{tsglm}} in the
#'  \href{http://tscount.r-forge.r-project.org/}{\code{tscount} package}
#'  (Liboschik \emph{et al}. 2017). 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param level \code{character} value name of the type of plots included 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @return \code{list} of [1] \code{"cast"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references
#'  Liboschik T., Fokianos K., and Fried R. 2017. tscount: An R Package for 
#'  Analysis of Count Time Series Following Generalized Linear Models. 
#'  \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'  \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   nbGARCH()
#'  }
#'
#' @export
#'
nbGARCH <- function(main = ".", level = "All", quiet = FALSE){

  messageq(paste0("### Fitting nbGARCH model for ", level, " ###"), quiet)
  abundances <- read_rodents(main, level = tolower(level))
  metadata <- read_metadata(main)

  nmoons <- length(metadata$rodent_cast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "newmoon")]
  cast <- data.frame()
  aic <- data.frame()

  for (s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting nbGARCH model for ", ss), quiet)
    abund_s <- abundances[[s]]

    past <- list(past_obs = 1, past_mean = 12)
    if (sum(abund_s) == 0){
      model_cast <- cast0(nmoons)
      model_aic <- 1e6
    } else{
      model <- tryCatch(
                 tsglm(abund_s, model = past, distr = "nbinom", link = "log"),
                 warning = function(x){NULL}, error = function(x){NULL})
      if(is.null(model) || AIC(model) == Inf){
          model <- tryCatch(
                     tsglm(abund_s, model = past,
                           distr = "poisson", link = "log"),
                     warning = function(x){NULL}, error = function(x){NULL})
      }
      if(is.null(model) || AIC(model) == Inf){
        model_cast <- cast0(nmoons)
        model_aic <- 1e6
      } else {
        model_cast <- predict(model, nmoons, level = CL)
        model_aic <- AIC(model)
      }
    }    

    estimate <- as.numeric(model_cast$pred)
    LowerPI <- as.numeric(model_cast$interval[ , 1]) 
    UpperPI <- as.numeric(model_cast$interval[ , 2])

    cast_s <- data.frame(date = metadata$cast_date, 
                         castmonth = metadata$rodent_cast_months,
                         castyear = metadata$rodent_cast_years, 
                         newmoonnumber = metadata$rodent_cast_newmoons,
                         currency = "abundance", model = "nbGARCH", 
                         level = level, species = ss, estimate = estimate, 
                         LowerPI = LowerPI, UpperPI = UpperPI, 
                         fit_start_newmoon = min(abundances$newmoon),
                         fit_end_newmoon = max(abundances$newmoon),
                         initial_newmoon = max(abundances$newmoon),
                         stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$cast_date, currency = "abundance", 
                        model = "nbGARCH", level = level, species = ss, 
                        aic = model_aic, 
                        fit_start_newmoon = min(abundances$newmoon),
                        fit_end_newmoon = max(abundances$newmoon),
                        initial_newmoon = max(abundances$newmoon),
                        stringsAsFactors = FALSE)

    cast <- rbind(cast, cast_s)
    aic <- rbind(aic, aic_s)
  }
  list("cast" = cast, "aic" = aic)
}



#' @title negative binomial seasonal Generalized AutoRegressive Conditional
#'  Heteroscedasticity model for Portal Predictions
#'
#' @description Fit an nbsGARCH model in the portalcasting pipeline.
#'
#' @details Model "nbsGARCH" is a seasonal generalized autoregresive 
#'  conditional heteroscedasticity model with overdispersion 
#'  (\emph{i.e.}, a negative binomial response variable) fit to the data
#'  using \code{\link[tscount]{tsglm}} in the
#'  \href{http://tscount.r-forge.r-project.org/}{\code{tscount} package}
#'  (Liboschik \emph{et al}. 2017). Seasonal dynamics are modeled using
#'  a two-term Fourier transformation.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param level \code{character} value name of the type of plots included 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @return \code{list} of [1] \code{"cast"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references
#'  Liboschik T., Fokianos K., and Fried R. 2017. tscount: An R Package for 
#'  Analysis of Count Time Series Following Generalized Linear Models. 
#'  \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'  \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   nbsGARCH()
#'  }
#'
#' @export
#'
nbsGARCH <- function(main = ".", level = "All", quiet = FALSE){

  messageq(paste0("### Fitting nbGARCH model for ", level, " ###"), quiet)
  abundances <- read_rodents(main, level = tolower(level))
  metadata <- read_metadata(main)
  moons <- read_moons(main)
  moon_foys <- foy(moons$newmoondate)
  sin2pifoy <- sin(2 * pi * moon_foys)
  cos2pifoy <- cos(2 * pi * moon_foys)
  fouriers <- data.frame(sin2pifoy, cos2pifoy)
  cast_moons <- metadata$rodent_cast_newmoons
  nmoons <- length(metadata$rodent_cast_newmoons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "newmoon")]
  for_hist <- which(moons$newmoonnumber %in% abundances$newmoon)
  for_cast <- which(moons$newmoonnumber %in% cast_moons) 
  predictors <- fouriers[for_hist, ]
  cast_predictors <- fouriers[for_cast, ]

  cast <- data.frame()
  aic <- data.frame()
  
  for (s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting nbsGARCH model for ", ss), quiet)
    abund_s <- abundances[[s]]

    past <- list(past_obs = 1, past_mean = 12)
    if (sum(abund_s) == 0){
      model_cast <- cast0(nmoons)
      model_aic <- 1e6
    } else{
      model <- tryCatch(
                 tsglm(abund_s, model = past, distr = "nbinom", 
                       xreg = predictors, link = "log"),
                 warning = function(x){NULL}, error = function(x){NULL})
      if(is.null(model) || AIC(model) == Inf){
          model <- tryCatch(
                     tsglm(abund_s, model = past, distr = "poisson", 
                           xreg = predictors, link = "log"),
                     warning = function(x){NULL}, error = function(x){NULL})
      }
      if(is.null(model) || AIC(model) == Inf){
        model_cast <- cast0(nmoons)
        model_aic <- 1e6
      } else {
        model_cast <- predict(model, nmoons, level = CL, 
                              newxreg = cast_predictors)
        model_aic <- AIC(model)
      }
    }    

    estimate <- as.numeric(model_cast$pred)
    LowerPI <- as.numeric(model_cast$interval[ , 1]) 
    UpperPI <- as.numeric(model_cast$interval[ , 2])

    cast_s <- data.frame(date = metadata$cast_date, 
                         castmonth = metadata$rodent_cast_months,
                         castyear = metadata$rodent_cast_years, 
                         newmoonnumber = metadata$rodent_cast_newmoons,
                         currency = "abundance", model = "nbsGARCH", 
                         level = level, species = ss, estimate = estimate, 
                         LowerPI = LowerPI, UpperPI = UpperPI, 
                         fit_start_newmoon = min(abundances$newmoon),
                         fit_end_newmoon = max(abundances$newmoon),
                         initial_newmoon = max(abundances$newmoon),
                         stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$cast_date, currency = "abundance", 
                        model = "nbsGARCH", level = level, species = ss, 
                        aic = model_aic, 
                        fit_start_newmoon = min(abundances$newmoon),
                        fit_end_newmoon = max(abundances$newmoon),
                        initial_newmoon = max(abundances$newmoon),
                        stringsAsFactors = FALSE)

    cast <- rbind(cast, cast_s)
    aic <- rbind(aic, aic_s)
  }
  list("cast" = cast, "aic" = aic)
}


#' @title poisson environmental variable Generalized Auto-Regressive 
#'  Conditional Heteroscedasticity model for Portal Predictions
#'
#' @description Fit a pevGARCH model in the portalcasting pipeline.
#'
#' @details Model "pevGARCH" is a generalized autoregresive conditional 
#'  heteroscedasticity model with a Poisson response variable  fit to the 
#'  data using \code{\link[tscount]{tsglm}} in the
#'  \href{http://tscount.r-forge.r-project.org/}{\code{tscount} package}
#'  (Liboschik \emph{et al}. 2017). \cr \cr
#'  For this model, environmental data are included as predictors of 
#'  abundance with an optional \code{lag} between the covariate values and 
#'  abundances (defaults to 6 newmoons).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param level \code{character} value name of the type of plots included 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param lag \code{integer} (or integer \code{numeric}) of the lag time to
#'  use for the covariates.
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'  quiet.
#'
#' @return \code{list} of [1] \code{"cast"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references
#'  Liboschik T., Fokianos K., and Fried R. 2017. tscount: An R Package for 
#'  Analysis of Count Time Series Following Generalized Linear Models. 
#'  \emph{Journal of Statistical Software} \strong{82}:5, 1-51. 
#'  \href{http://doi.org/10.18637/jss.v082.i05}{URL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   pevGARCH()
#'  }
#' 
#' @export
#'
pevGARCH <- function(main = ".", level = "All", lag = 6, quiet = FALSE){
  messageq(paste0("### Fitting pevGARCH model for ", level, " ###"), quiet)

  abundances <- read_rodents(main, level = tolower(level))
  metadata <- read_metadata(main)
  covariates <- read_covariates(main)

  cast_moons <- metadata$rodent_cast_newmoons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level
  abundances <- interpolate_abundance(abundances)
  species <- colnames(abundances)[-which(colnames(abundances) == "newmoon")]

  covar_lag <- lag_covariates(covariates, lag, tail = TRUE)
  for_hist <- which(covar_lag$newmoonnumber %in% abundances$newmoon)
  for_cast <- which(covar_lag$newmoonnumber %in% cast_moons) 
  covar_hist <- covar_lag[for_hist, ]

  if (metadata$cast_type == "forecast"){
    covar_cast <- covar_lag[for_cast, ]
  } 
  if (metadata$cast_type == "hindcast"){
    covariate_casts <- read_covariate_casts(main)
    covar_casts_lag <- lag_covariates(covariate_casts, lag, tail = TRUE)
    last_cov_nm <- max(covar_hist$newmoonnumber) - lag
    nm_in <- covar_casts_lag$cast_newmoon == last_cov_nm
    s_in <- covar_casts_lag$source == metadata$covariate_source
    dm_in <- covar_casts_lag$date_made == metadata$covariate_date_made
    covar_cast <- covar_casts_lag[nm_in & dm_in & s_in, ]
  }
        
  cast <- data.frame()
  aic <- data.frame()

  models <- covariate_models("pevGARCH")

  for(s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting pevGARCH models for ", ss), quiet)
    abund_s <- abundances[[s]]
  
    if (sum(abund_s) == 0){
      spec_cast <- cast0(nmoons)
      spec_aic <- 1e6
    } else{

      best_aic <- Inf
      best_model <- NA
      model_count <- 1

      for(m in models){
        model_name <- paste(m, collapse = ", ")
        msg <- paste0("Fitting Model ", model_count, ": ", model_name)
        messageq(msg, quiet)
        predictors <- NULL
        cast_predictors <- NULL
        if (!(is.null(unlist(m)))){
          predictors <- select(covar_hist, unlist(m))
          cast_predictors <- select(covar_cast, unlist(m))
        }
        setup <- list(past_obs = 1, past_mean = 12)
        proposed_model <- tryCatch(
                            tsglm(abund_s, model = setup, distr = "poisson",
                              xreg = predictors, link = "log"), 
                            error = function(x) {NA})
        proposed_aic <- tryCatch(
                          AIC(proposed_model), 
                          error = function(x) {Inf})

        proposed_cast <- tryCatch(
                            predict(proposed_model, nmoons, level = CL, 
                              newxreg = cast_predictors),
                            error = function(x) {NA})

        if(proposed_aic < best_aic){
          best_model <- proposed_model
          best_aic <- proposed_aic

          spec_forecast <- proposed_cast
          spec_aic <- proposed_aic
        }

        model_count <- model_count + 1
      }

      fit_fails <- length(which(is.na(best_model) == TRUE))    
      if(fit_fails > 0){
        spec_forecast <- cast0(nmoons)
        spec_aic <- 1e6
      } 

    }

    estimate <- as.numeric(spec_forecast$pred)
    LowerPI <- as.numeric(spec_forecast$interval[, 1]) 
    UpperPI <- as.numeric(spec_forecast$interval[, 2])

    cast_s <- data.frame(date = metadata$cast_date, 
                         castmonth = metadata$rodent_cast_months,
                         castyear = metadata$rodent_cast_years, 
                         newmoonnumber = metadata$rodent_cast_newmoons,
                         currency = "abundance", model = "pevGARCH", 
                         level = level, species = ss, estimate = estimate, 
                         LowerPI = LowerPI, UpperPI = UpperPI, 
                         fit_start_newmoon = min(abundances$newmoon),
                         fit_end_newmoon = max(abundances$newmoon),
                         initial_newmoon = max(abundances$newmoon),
                         stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$cast_date, currency = "abundance", 
                        model = "pevGARCH", level = level, species = ss, 
                        aic = spec_aic, 
                        fit_start_newmoon = min(abundances$newmoon),
                        fit_end_newmoon = max(abundances$newmoon),
                        initial_newmoon = max(abundances$newmoon),
                        stringsAsFactors = FALSE)

    cast <- rbind(cast, cast_s)
    aic <- rbind(aic, aic_s)
  }
  list("cast" = cast, "aic" = aic)
}

