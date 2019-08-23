library(rEDM)
library(tidyr)
library(dplyr)

#' @title Empirical Dynamical Model for Portal Predictions
#'
#' @description Fit an EDM model in the portalcasting pipeline.
#'
#' @details Model "EDM" is a...
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param level \code{character} value name of the type of plots included 
#'   (\code{"All"} or \code{"Controls"}).
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'   quiet.
#'
#' @return \code{list} of [1] \code{"forecast"} (the forecasted abundances)
#'   and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @references 
#'  Sugihara et al. 20XX
#'
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' EDM()
#' }
#' 
#' @export
#'
EDM <- function(tree = dirtree(), level = "All", quiet = FALSE){
  check_args()
  messageq(paste0("### Fitting EDM model for ", level, " ###"), quiet)

  abundances <- read_data(tree, tolower(level))
  metadata <- read_metadata(tree)

  nfcnm <- length(metadata$rodent_forecast_newmoons)
  CL <- metadata$confidence_level
  # Calculate the probability for estimating the prediction interval from the CL
  # CL gives the probabilty range we want to cover, so to get the +/- from qnorm
  # we want 1 - ((1 - CL) / 2) == 0.5 + CL/2
  pr_for_CL <- 0.5 + CL/2

  abundances <- interpolate_abundance(abundances)
  abundances_long <- gather(abundances, "species", "abundance", -moons)
  species <- colnames(abundances)[-which(colnames(abundances) == "moons")]
  fcast <- data.frame()
  aic <- data.frame()
  #species = c("DO")
  for (s in species){

    ss <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting EDM model for ", ss), quiet)

    abund_s <- filter(abundances_long, species == s) %>% select(-species)

    n <- nrow(abund_s)
    models <- simplex(abund_s, E = 1:7, stats_only = FALSE, silent = TRUE)
    best_e <- models$E[which.min(models$mae)]
    pred_data <- rbind(abund_s, data.frame(moons = max(abund_s$moons) + 1, abundance = NA))

    estimate = c()
    LowerPI = c()
    UpperPI = c()
    for (horizon in seq(nfcnm)) {
      model <- simplex(time_series = pred_data,
                       lib = c(1, nrow(abund_s)),
                       pred = c(nrow(pred_data) - best_e, nrow(pred_data)),
                       E = best_e,
                       stats_only = FALSE,
                       silent = TRUE)
      pred_row <- model$model_out[[1]][best_e, ]
      pt_est <- pred_row$pred
      estimate <- c(estimate, pt_est)
      LowerPI <- c(LowerPI, pt_est - qnorm(pr_for_CL) * pred_row$pred_var)
      UpperPI <- c(UpperPI, pt_est + qnorm(pr_for_CL) * pred_row$pred_var)
      pred_data <- rbind(head(pred_data, -1), c(pred_row$time, pred_row$pred))
      pred_data <- rbind(pred_data, data.frame(moons = max(pred_data$moons) + 1, abundance = NA))
    }

    model_aic <- NA

    fcast_s <- data.frame(date = metadata$forecast_date, 
                 forecastmonth = metadata$rodent_forecast_months,
                 forecastyear = metadata$rodent_forecast_years, 
                 newmoonnumber = metadata$rodent_forecast_newmoons,
                 currency = "abundance", model = "EDM", level = level, 
                 species = ss, estimate = estimate, LowerPI = LowerPI, 
                 UpperPI = UpperPI, fit_start_newmoon = min(abundances$moons),
                 fit_end_newmoon = max(abundances$moons),
                 initial_newmoon = max(abundances$moons),
                 stringsAsFactors = FALSE)

    aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
               model = "EDM", level = level, species = ss, aic = model_aic, 
               fit_start_newmoon = min(abundances$moons),
               fit_end_newmoon = max(abundances$moons),
               initial_newmoon = max(abundances$moons),
               stringsAsFactors = FALSE)

    fcast <- rbind(fcast, fcast_s)
    aic <- rbind(aic, aic_s)
  }
  output <- list("forecast" = fcast, "aic" = aic)
  return(output)
}

tree <- dirtree(".", "", subdirs());
f_a <- EDM(tree, level = "All", quiet = FALSE);
f_c <- EDM(tree, level = "Controls", quiet = FALSE);
save_forecast_output(f_a, f_c, "EDM", tree)
