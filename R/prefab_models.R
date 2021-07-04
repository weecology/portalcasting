
AutoArima <- function (main = ".", 
                       data_set = "all",
                       quiet = FALSE, 
                       verbose = FALSE) {

  for (i in 1:nspecies){
    s <- species[i]
    ss <- gsub("NA.", "NA", s)
    messageq(paste0("   -", ss), !verbose)
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
                              controls_r = data_set_controls,
                              arg_checks = arg_checks)
  list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
       model_casts = casts)
}



