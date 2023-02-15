
AutoArima <- function (main     = ".", 
                       dataset  = "all",
                       settings = directory_settings(), 
                       quiet    = FALSE, 
                       verbose  = FALSE) {



  model_controls_list <- model_controls(main = main, models = "AutoArima", settings = settings)$AutoArima

abundance<-abundance[,"DM"]

?eval
 model_controls_list$args$y<-as.name( model_controls_list$args$y)

  do.call(what = model_controls_list$fun, args = model_controls_list$args)


 # existing code >>


  dataset <- tolower(dataset)

  messageq("  -AutoArima for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  species       <- species_from_table(rodents_tab = rodents_table, 
                                      total       = TRUE, 
                                      nadot       = TRUE)

  # PATCH START >>

  temp_species  <- read_model_controls(main = main, settings = settings)$AutoArima$species
  if (length(temp_species) == 1 && temp_species == "all") {
    species <- species
  } else {
    species <- species[species %in% temp_species]
  }

  # << END PATCH

  nspecies      <- length(species)

  metadata <- read_metadata(main     = main,
                            settings = settings)

  # THIS NEEDS TO BE GENERALIZED TIME >> 

  start_moon       <- metadata$time$start_moon
  end_moon         <- metadata$time$end_moon

  cast_moons       <- metadata$time$rodent_cast_moons

  confidence_level <- metadata$confidence_level
  dataset_controls <- metadata$dataset_controls[[dataset]]

  moon_in          <- rodents_table$newmoonnumber >= start_moon & rodents_table$newmoonnumber <= end_moon
  rodents_table    <- rodents_table[moon_in, ]
  nmoons           <- length(cast_moons)

  # << END

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

    cast_tab_s <- data.frame(cast_date        = metadata$time$cast_date, 
                             cast_month       = metadata$time$rodent_cast_months,
                             cast_year        = metadata$time$rodent_cast_years, 
                             moon             = metadata$time$rodent_cast_moons,
                             currency         = dataset_controls$args$output,
                             model            = "AutoArima", 
                             dataset          = dataset, 
                             species          = ss, 
                             cast_group       = metadata$cast_group,
                             confidence_level = metadata$confidence_level,
                             estimate         = casts[[i]][ , "Point.Forecast"], 
                             lower_pi         = casts[[i]][ , paste0("Lo.", confidence_level * 100)], 
                             upper_pi         = casts[[i]][ , paste0("Hi.", confidence_level * 100)], 
                             start_moon       = metadata$time$start_moon,
                             end_moon         = metadata$time$end_moon)

    cast_tab <- rbind(cast_tab, cast_tab_s)
  }

  metadata <- update_list(metadata, 
                          models           = "AutoArima",
                          datasets         = dataset,
                          dataset_controls = dataset_controls)

  list(metadata    = metadata, 
       cast_tab    = cast_tab, 
       model_fits  = mods, 
       model_casts = casts)

}


