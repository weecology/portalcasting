# @rdname prefab_model_functions
# 
# @export
#
simplexEDM <- function(main = ".", data_set = "all_interp", 
                       control_files = files_control(), 
                       quiet = FALSE, 
                       verbose = FALSE, 
                       max_E = 7 )
{
  
  data_set <- tolower(data_set)
  model_name <- "simplexEDM"
  
  messageq(paste0("  -", model_name, " model for ", data_set), quiet)
  
  #### determine args and settings for making forecasts
  metadata <- read_metadata(main = main, 
                            control_files = control_files)
  E_range <- seq(from = 1, to = max_E)
  data_set_controls <- metadata$controls_rodents[[data_set]]
  moons_to_cast <- metadata$rodent_cast_moons
  num_moons <- length(moons_to_cast)
  CL <- metadata$confidence_level
  # Calculate the probability for estimating the prediction interval
  #   from the CL
  # CL gives the probabilty range we want to cover, so to get the +/- 
  #  from qnorm
  # we want 1 - ((1 - CL) / 2) == 0.5 + CL/2
  pr_for_CL <- 0.5 + CL/2
  
  #### get the data
  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & 
    rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)

  cast_tab <- data.frame()
  #  aic <- data.frame()
  model_casts <- data.frame()
  
  # species = c("DO")
  for (s in species)
  {
    species_name <- gsub("NA.", "NA", s)
    messageq(paste0("   -", species_name), !verbose)

    abund_s <- rodents_table[, c("moon", s)]
    
    n <- nrow(abund_s)
    models <- simplex(abund_s,
                            lib = c(1, n),
                            pred = c(1, n),
                            E = E_range,
                            stats_only = FALSE, silent = TRUE)
    best_idx <- which.min(models$mae)
    best_e <- models$E[best_idx]
    
    # set up data structures to hold forecasts
    preds <- data.frame(moon = moons_to_cast, 
                        estimate = numeric(num_moons), 
                        lower_pi = numeric(num_moons), 
                        upper_pi = numeric(num_moons))
    preds$species <- species_name
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
      preds$lower_pi[i] <- qnorm(1 - pr_for_CL, mean = pt_est, 
                                 sd = sqrt(pred_row$pred_var))
      preds$upper_pi[i] <- qnorm(pr_for_CL, mean = pt_est, 
                                 sd = sqrt(pred_row$pred_var))
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
                             species = species_name, 
                             estimate = preds$estimate, 
                             lower_pi = preds$lower_pi, 
                             upper_pi = preds$upper_pi, 
                             start_moon = metadata$start_moon, 
                             end_moon = metadata$end_moon, 
                             stringsAsFactors = FALSE)
    
    # model_aic <- NA
    # 
    # aic_s <- data.frame(date = metadata$forecast_date, 
    #            currency = "abundance", 
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
  cast_out <- list(metadata = metadata, 
                   cast_tab = cast_tab, 
                   model_fits = NULL, 
                   model_casts = model_casts)
}

# @rdname prefab_model_functions
# 
# @export
#
GPEDM <- function(main = ".", data_set = "all_interp", 
                  control_files = files_control(), 
                  quiet = FALSE, 
                  verbose = FALSE, 
                  max_E = 7)
{
  
  data_set <- tolower(data_set)
  model_name <- "GPEDM"
  
  messageq(paste0("  -", model_name, " model for ", data_set), quiet)
  
  #### determine args and settings for making forecasts
  metadata <- read_metadata(main = main, 
                            control_files = control_files)
  E_range <- seq(from = 1, to = max_E)
  data_set_controls <- metadata$controls_rodents[[data_set]]
  moons_to_cast <- metadata$rodent_cast_moons
  num_moons <- length(moons_to_cast)
  CL <- metadata$confidence_level
  # Calculate the probability for estimating the prediction interval
  #   from the CL
  # CL gives the probabilty range we want to cover, so to get the +/- 
  #   from qnorm
  # we want 1 - ((1 - CL) / 2) == 0.5 + CL/2
  pr_for_CL <- 0.5 + CL/2
  
  #### get the data
  rodents_table <- read_rodents_table(main = main, data_set = data_set)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & 
    rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE)
  
  cast_tab <- data.frame()
  model_casts <- data.frame()
  
  for (s in species)
  {
    species_name <- gsub("NA.", "NA", s)
    messageq(paste0("   -", species_name), !verbose)
    
    abund_s <- rodents_table[, c("moon", s)]
    preds <- data.frame(moon = moons_to_cast, 
                        estimate = mean(abund_s[, s]), 
                        lower_pi = NA, 
                        upper_pi = NA, 
                        species = species_name)
    
    # check for low variance in time series
    ts <- utils::head(rodents_table[, s], -1)
    ts_var <- stats::var(ts)
    if (is.finite(ts_var) &&
        ts_var > 2 * .Machine$double.eps)
    {
      n <- nrow(abund_s)
      models <- tde_gp(abund_s, 
                             lib = c(1, n), 
                             pred = c(1, n), 
                             E = E_range, 
                             fit_params = TRUE, 
                             stats_only = FALSE, silent = TRUE)
      best_idx <- which.min(models$mae)
      best_phi <- models$phi[best_idx]
      best_v_e <- models$v_e[best_idx]
      best_eta <- models$eta[best_idx]
      best_e <- models$E[best_idx]
      
      # set up data structures to hold forecasts
      to_add <- data.frame(moons_to_cast, NA)
      colnames(to_add) <- c("moon", s)
      pred_data <- rbind(abund_s, to_add)
      
      # make forecasts
      for (i in seq_along(moons_to_cast))
      {
        model <- tde_gp(pred_data,
                              lib = c(1, n),
                              pred = c(n + i - best_e, n + i),
                              E = best_e,
                              phi = best_phi, 
                              v_e = best_v_e, 
                              eta = best_eta, 
                              fit_params = FALSE, 
                              stats_only = FALSE,
                              silent = TRUE)
        
        pred_row <- model$model_out[[1]][best_e, ]
        pt_est <- pred_row$pred
        preds$estimate[i] <- pt_est
        preds$lower_pi[i] <- qnorm(1 - pr_for_CL, mean = pt_est, 
                                   sd = sqrt(pred_row$pred_var))
        preds$upper_pi[i] <- qnorm(pr_for_CL, mean = pt_est, 
                                   sd = sqrt(pred_row$pred_var))
        pred_data[n + i, s] <- pt_est
      }
    }
    
    # generate cast_tab to add
    cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
                             cast_month = metadata$rodent_cast_months,
                             cast_year = metadata$rodent_cast_years, 
                             moon = metadata$rodent_cast_moons, 
                             currency = data_set_controls$output, 
                             model = model_name, 
                             data_set = data_set, 
                             species = species_name, 
                             estimate = preds$estimate, 
                             lower_pi = preds$lower_pi,
                             upper_pi = preds$upper_pi, 
                             start_moon = metadata$start_moon, 
                             end_moon = metadata$end_moon, 
                             stringsAsFactors = FALSE)
    
    cast_tab <- rbind(cast_tab, cast_tab_s)
    model_casts <- rbind(model_casts, preds)
  }
  
  # generate output to save
  metadata <- update_list(metadata, 
                          models = model_name, 
                          data_sets = data_set, 
                          controls_rodents = data_set_controls)
  cast_out <- list(metadata = metadata, 
                   cast_tab = cast_tab, 
                   model_fits = NULL, 
                   model_casts = model_casts)
}

