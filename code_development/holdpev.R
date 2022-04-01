
pevGARCH <- function (main     = ".", 
                      dataset  = "all_interp",
                      lag      = 6,
                      settings = directory_settings(), 
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  dataset <- tolower(dataset)
  messageq(paste0("  -pevGARCH for ", dataset), quiet = quiet)

  rodents_table <- read_rodents_table(main = main, dataset  = dataset,
                                      arg_checks = arg_checks)
  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, 
                                nadot = TRUE, arg_checks = arg_checks)
  nspecies <- length(species)

  metadata <- read_metadata(main = main, control_files = control_files,
                            arg_checks = arg_checks)
  start_moon <- metadata$start_moon
  end_moon <- metadata$end_moon
  moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= end_moon
  rodents_table <- rodents_table[moon_in, ]
  cast_moons <- metadata$rodent_cast_moons
  nmoons <- length(cast_moons)
  CL <- metadata$confidence_level

  covariates <- read_covariates(main = main, control_files = control_files,
                                arg_checks = arg_checks)
  covar_lag <- lag_covariates(covariates, lag, tail = TRUE)
  for_hist <- which(covar_lag$moon %in% rodents_table$moon)
  for_cast <- which(covar_lag$moon %in% cast_moons) 
  covar_hist <- covar_lag[for_hist, ]
  if (metadata$end_moon == metadata$last_moon){
    covar_cast <- covar_lag[for_cast, ]
  } else {
    covariate_casts <- read_covariate_casts(main = main, 
                                            control_files = control_files,
                                            quiet = quiet, verbose = verbose,
                                            arg_checks = arg_checks)
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
  dataset_controls <- metadata$controls_rodents[[dataset]]      
  models <- covariate_models("pevGARCH")
  nmodels <- length(models)

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq(paste0("   -", ss), quiet = !verbose)
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
      messageq(paste0("    -", j, ": ", model_name), quiet = !verbose)
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
                             currency = dataset_controls$output,
                             model = "pevGARCH", dataset  = dataset, 
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
                              datasets = dataset,
                              controls_r = dataset_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}