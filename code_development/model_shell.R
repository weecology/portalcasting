# working here and with the updated model controls yaml to streamline model construction

site_model <- function (main     = ".", 
                        model    = NULL,
                        dataset  = "all",
                        settings = directory_settings(), 
                        quiet    = FALSE, 
                        verbose  = FALSE) {

  return_if_null(model)

  controls <- model_controls(main     = main, 
                             models   = model,
                             settings = settings)

  dataset <- tolower(dataset)

  messageq("  -", controls$metadata$name, " for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)
  nspecies      <- length(species)

  metadata         <- read_metadata(main     = main,
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

    abundance <- rodents_table[ , s]

    if (sum(abundance, na.rm = TRUE) == 0) {
      next()
    }

    if (is.list(controls$args)) {

      args <- controls$args

    } else {

      args <- list(controls$args)

    }

    nargs <- length(args)

    for (j in 1:nargs) {

      if(grepl("quote", args[[j]])) {

        args[[j]] <- eval(parse(text = args[[j]]))

      }

    }

    if (controls$interpolate$needed) {

      args <- update_list(args, 
                          y = do.call(what = controls$interpolate$fun,
                                      args = list(abundance)))

    }  else {

      args <- update_list(args, 
                          y = abundance)

    }

    mods[[i]]  <- do.call(what = controls$fun, 
                          args = args)

    casts[[i]] <- forecast(object = mods[[i]], 
                           h      = nmoons, 
                           level  = confidence_level)

    casts[[i]] <- data.frame(casts[[i]], 
                             moon = cast_moons)

    cast_tab_s <- data.frame(cast_date        = metadata$time$cast_date, 
                             cast_month       = metadata$time$rodent_cast_months,
                             cast_year        = metadata$time$rodent_cast_years, 
                             moon             = metadata$time$rodent_cast_moons,
                             currency         = dataset_controls$args$output,
                             model            = controls$metadata$name, 
                             dataset          = dataset, 
                             species          = ss, 
                             cast_group       = metadata$cast_group,
                             confidence_level = metadata$confidence_level,
                             estimate         = casts[[i]][ , "Point.Forecast"], 
                             lower_pi         = casts[[i]][ , paste0("Lo.", confidence_level * 100)], 
                             upper_pi         = casts[[i]][ , paste0("Hi.", confidence_level * 100)], 
                             start_moon       = metadata$time$start_moon,
                             end_moon         = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, 
                      cast_tab_s)

  }

  metadata <- update_list(metadata, 
                          models           = controls$metadata$name,
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

    cast_tab_s <- data.frame(cast_date        = metadata$time$cast_date, 
                             cast_month       = metadata$time$rodent_cast_months,
                             cast_year        = metadata$time$rodent_cast_years, 
                             moon             = metadata$time$rodent_cast_moons,
                             currency         = dataset_controls$args$output,
                             model            = "NaiveArima", 
                             dataset          = dataset, 
                             species          = ss, 
                             cast_group       = metadata$cast_group,
                             confidence_level = metadata$confidence_level,
                             estimate         = casts[[i]][ ,"Point.Forecast"], 
                             lower_pi         = casts[[i]][ ,paste0("Lo.", confidence_level * 100)], 
                             upper_pi         = casts[[i]][ ,paste0("Hi.", confidence_level * 100)], 
                             start_moon       = metadata$time$start_moon,
                             end_moon         = metadata$time$end_moon)

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
                  dataset  = "all",
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

    abund_s <- round(na.interp(rodents_table[ , s]))

    if (sum(abund_s, na.rm = TRUE) == 0) {
      next()
    }

    mods[[i]]  <- ets(abund_s)
    casts[[i]] <- forecast(mods[[i]], h = nmoons, level = confidence_level,
                           allow.multiplicative.trend = TRUE)
    casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)

    cast_tab_s <- data.frame(cast_date        = metadata$time$cast_date, 
                             cast_month       = metadata$time$rodent_cast_months,
                             cast_year        = metadata$time$rodent_cast_years, 
                             moon             = metadata$time$rodent_cast_moons,
                             currency         = dataset_controls$args$output,
                             model            = "ESSS", 
                             cast_group       = metadata$cast_group,
                             confidence_level = metadata$confidence_level,
                             dataset          = dataset, 
                             species          = ss, 
                             estimate         = casts[[i]][ , "Point.Forecast"], 
                             lower_pi         = casts[[i]][ , paste0("Lo.", confidence_level * 100)], 
                             upper_pi         = casts[[i]][ , paste0("Hi.", confidence_level * 100)], 
                             start_moon       = metadata$time$start_moon,
                             end_moon         = metadata$time$end_moon)

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