library(rEDM)

#' @title Empirical Dynamical Model for Portal Predictions
#'
#' @description Fit an EDM model in the portalcasting pipeline.
#'
#' @details Model "EDM" is a...
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} value indicating if the function should be 
#'   quiet.
#'
#' @return \code{list} of [1] \code{"forecast"} (the forecasted abundances)
#'   and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @inheritParams read_rodents_table
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
EDM <- function(main = here::here(), data_set = "all_interp", quiet = FALSE, 
                E_range = 1:7, save_output = TRUE)
{
  messageq(paste0("### Fitting EDM model for ", data_set, " ###"), quiet)
  
  #### determine args and settings for making forecasts
  check_args()
  
  model_name <- "EDM-simplex"
  metadata <- read_metadata(main)
  data_set_controls <- metadata$controls_rodents[[data_set]]
  moons_to_cast <- metadata$rodent_cast_moons
  num_moons <- length(moons_to_cast)
  CL <- metadata$confidence_level
  # Calculate the probability for estimating the prediction interval from the CL
  # CL gives the probabilty range we want to cover, so to get the +/- from qnorm
  # we want 1 - ((1 - CL) / 2) == 0.5 + CL/2
  pr_for_CL <- 0.5 + CL/2
  
  #### get the data
  abundances <- read_rodents_table(main = main, data_set = data_set)
  species <- setdiff(colnames(abundances), "moon")
  
  cast_tab <- data.frame()
  aic <- data.frame()
  model_casts <- data.frame()
  
  #species = c("DO")
  for (s in species)
  {
    formatted_species_code <- gsub("NA.", "NA", s)
    messageq(paste0("Fitting ", model_name, " model for ", formatted_species_code), quiet)
    
    abund_s <- abundances[, c("moon", s)]
    
    n <- nrow(abund_s)
    models <- simplex(abund_s, 
                      lib = c(1, n), 
                      pred = c(1, n), 
                      E = E_range, 
                      stats_only = FALSE, silent = TRUE)
    best_e <- models$E[which.min(models$mae)]
    
    # set up data structures to hold forecasts
    preds <- data.frame(moon = moons_to_cast, 
                        estimate = numeric(num_moons), 
                        lower_pi = numeric(num_moons), 
                        upper_pi = numeric(num_moons))
    preds$species <- formatted_species_code
    to_add <- data.frame(moons_to_cast, NA)
    colnames(to_add) <- c("moon", s)
    pred_data <- rbind(abund_s, to_add)
    
    # make forecasts
    for (i in seq_along(moons_to_cast))
    {
      model <- simplex(pred_data,
                       lib = c(1, n),
                       pred = c(n + i - best_e, n + i),
                       E = best_e,
                       stats_only = FALSE,
                       silent = TRUE)
      pred_row <- model$model_out[[1]][best_e, ]
      pt_est <- pred_row$pred
      preds$estimate[i] <- pt_est
      preds$lower_pi[i] <- qnorm(1 - pr_for_CL, mean = pt_est, sd = sqrt(pred_row$pred_var))
      preds$upper_pi[i] <- qnorm(pr_for_CL, mean = pt_est, sd = sqrt(pred_row$pred_var))
      pred_data[n + i, s] <- pt_est
    }
    
    # generate cast_tab to add
    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons, 
                             currency = data_set_controls$output, 
                             model = model_name, 
                             data_set = data_set, 
                             species = formatted_species_code, 
                             estimate = preds$estimate, 
                             lower_pi = preds$lower_pi, upper_pi = preds$upper_pi, 
                             start_moon = metadata$start_moon, 
                             end_moon = metadata$end_moon, 
                             stringsAsFactors = FALSE)
    
    # model_aic <- NA
    # 
    # aic_s <- data.frame(date = metadata$forecast_date, currency = "abundance", 
    #            model = "EDM", level = level, species = ss, aic = model_aic, 
    #            fit_start_newmoon = min(abundances$moons),
    #            fit_end_newmoon = max(abundances$moons),
    #            initial_newmoon = max(abundances$moons),
    #            stringsAsFactors = FALSE)
    
    cast_tab <- rbind(cast_tab, cast_tab_s)
    model_casts <- rbind(model_casts, preds)
    #    aic <- rbind(aic, aic_s)
  }
  
  # generate output to save
  metadata <- update_list(metadata, 
                          models = model_name, 
                          data_sets = data_set, 
                          controls_rodents = data_set_controls)
  cast_out <- list(metadata = metadata, cast_tab = cast_tab, 
                   model_fits = model)
  
  if (save_output)
  {
    save_cast_output(cast = cast_out, main = main)
  }
  #  output <- list("forecast" = fcast, "aic" = aic)
  return(cast_out)
}

main <- here::here()
EDM(main = main, data_set = "all_interp", quiet = FALSE)
EDM(main = main, data_set = "controls_interp", quiet = FALSE)
