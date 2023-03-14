












  moons      <- read_newmoons(main     = main, 
                           settings = settings)
  rodents    <- read_rodents(main     = main, 
                             datasets = datasets, 
                             settings = settings)
  covariates <- read_covariates(main     = main, 
                                settings = settings)

  dataset_controls_list <- dataset_controls(main     = main, 
                                            settings = settings, 
                                            datasets = datasets)



   
  which_last_moon <- max(which(moons$newmoondate < origin))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moon        <- ifnull(end_moon, last_moon)


  ncontrols_r      <- length(rodents)
  last_rodent_moon <- 0

  for (i in 1:ncontrols_r) {

    rodent_moon_i      <- rodents[[i]]$newmoonnumber
    last_rodent_moon_i <- max(rodent_moon_i[rodent_moon_i <= end_moon], na.rm = TRUE)
    last_rodent_moon   <- max(c(last_rodent_moon, last_rodent_moon_i, na.rm = TRUE))

  }

  last_covar_moon        <- max(covariates$newmoonnumber[covariates$source == "historic"], na.rm = TRUE)
  first_cast_covar_moon  <- last_covar_moon + 1
  first_cast_rodent_moon <- last_rodent_moon + 1
  last_cast_moon         <- end_moon + lead_time
  rodent_cast_moons      <- first_cast_rodent_moon:last_cast_moon

  which_r_nms        <- which(moons$newmoonnumber%in% rodent_cast_moons)
  rodent_nm_dates    <- as.Date(moons$newmoondate[which_r_nms])
  rodent_cast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_cast_years  <- as.numeric(format(rodent_nm_dates, "%Y"))
  rodent_lead_time   <- length(rodent_cast_moons)

  covar_cast_moons  <- first_cast_covar_moon:last_cast_moon
  which_c_nms       <- which(moons$newmoonnumber %in% covar_cast_moons)
  covar_nm_dates    <- as.Date(moons$newmoondate[which_c_nms])
  covar_cast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_cast_years  <- as.numeric(format(covar_nm_dates, "%Y"))
  covar_lead_time   <- length(covar_cast_moons)

  cast_type  <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  cast_meta  <- read_casts_metadata(main     = main, 
                                    settings = settings,
                                    quiet    = quiet)
  cast_group <- max(c(0, cast_meta$cast_group)) + 1

  covariates_origin_date <- max(covariates$date[covariates$source == "historic"])




cast_group              = cast_group,
              cast_type               = cast_type,
              models                  = models, 
              datasets                = datasets, 
              dataset_controls        = dataset_controls_list,
              time                    = list(start_moon            = start_moon,
                                             end_moon              = end_moon,
                                             last_moon             = last_moon,
                                             origin             = as.character(origin), 
                                             lead_time             = lead_time,
                                             covariate_cast_moons  = covar_cast_moons, 
                                             covariate_cast_months = covar_cast_months, 
                                             covariate_cast_years  = covar_cast_years,
                                             covariate_lead_time   = covar_lead_time,
                                             rodent_cast_moons     = rodent_cast_moons, 
                                             rodent_cast_months    = rodent_cast_months, 
                                             rodent_cast_years     = rodent_cast_years, 
                                             rodent_lead_time      = rodent_lead_time),

              )

