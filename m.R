AutoArima  <-  function (main = ".", data_set = "all", control_files = files_control(), 
AutoArima  <-      quiet = FALSE, verbose = FALSE, arg_checks = TRUE) 
AutoArima  <-  {
AutoArima  <-      check_args(arg_checks)
AutoArima  <-      data_set <- tolower(data_set)
AutoArima  <-      messageq(paste0("  -AutoArima for ", data_set), quiet)
AutoArima  <-      rodents_table <- read_rodents_table(main = main, data_set = data_set, 
AutoArima  <-          arg_checks = arg_checks)
AutoArima  <-      species <- species_from_table(rodents_tab = rodents_table, 
AutoArima  <-          total = TRUE, nadot = TRUE, arg_checks = arg_checks)
AutoArima  <-      nspecies <- length(species)
AutoArima  <-      metadata <- read_metadata(main = main, control_files = control_files, 
AutoArima  <-          arg_checks = arg_checks)
AutoArima  <-      start_moon <- metadata$start_moon
AutoArima  <-      end_moon <- metadata$end_moon
AutoArima  <-      moon_in <- rodents_table$moon >= start_moon & rodents_table$moon <= 
AutoArima  <-          end_moon
AutoArima  <-      rodents_table <- rodents_table[moon_in, ]
AutoArima  <-      cast_moons <- metadata$rodent_cast_moons
AutoArima  <-      nmoons <- length(cast_moons)
AutoArima  <-      CL <- metadata$confidence_level
AutoArima  <-      data_set_controls <- metadata$controls_r[[data_set]]
AutoArima  <-      mods <- named_null_list(species)
AutoArima  <-      casts <- named_null_list(species)
AutoArima  <-      cast_tab <- data.frame()
AutoArima  <-      for (i in 1:nspecies) {
AutoArima  <-          s <- species[i]
AutoArima  <-          ss <- gsub("NA.", "NA", s)
AutoArima  <-          messageq(paste0("   -", ss), !verbose)
AutoArima  <-          abund_s <- rodents_table[, s]
AutoArima  <-          if (sum(abund_s, na.rm = TRUE) == 0) {
AutoArima  <-              (next)()
AutoArima  <-          }
AutoArima  <-          mods[[i]] <- auto.arima(abund_s)
AutoArima  <-          casts[[i]] <- forecast(mods[[i]], h = nmoons, level = CL)
AutoArima  <-          casts[[i]] <- data.frame(casts[[i]], moon = cast_moons)
AutoArima  <-          cast_tab_s <- data.frame(cast_date = metadata$cast_date, 
AutoArima  <-              cast_month = metadata$rodent_cast_months, cast_year = metadata$rodent_cast_years, 
AutoArima  <-              moon = metadata$rodent_cast_moons, currency = data_set_controls$output, 
AutoArima  <-              model = "AutoArima", data_set = data_set, species = ss, 
AutoArima  <-              estimate = casts[[i]][, "Point.Forecast"], lower_pi = casts[[i]][, 
AutoArima  <-                  paste0("Lo.", CL * 100)], upper_pi = casts[[i]][, 
AutoArima  <-                  paste0("Hi.", CL * 100)], start_moon = metadata$start_moon, 
AutoArima  <-              end_moon = metadata$end_moon, stringsAsFactors = FALSE)
AutoArima  <-          cast_tab <- rbind(cast_tab, cast_tab_s)
AutoArima  <-      }
AutoArima  <-      metadata <- update_list(metadata, models = "AutoArima", data_sets = data_set, 
AutoArima  <-          controls_r = data_set_controls, arg_checks = arg_checks)
AutoArima  <-      list(metadata = metadata, cast_tab = cast_tab, model_fits = mods, 
AutoArima  <-          model_casts = casts)
AutoArima  <-  }
