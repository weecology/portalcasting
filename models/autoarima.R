
rodents <- prep_rodent_data()
covariates <- prep_covariate_data()
metadata <- prep_metadata(rodents, covariates)

aa_a <- autoarima(rodents$all, metadata)
aa_c <- autoarima(rodents$controls, metadata)

forecasts <- rbind(aa_a[[1]], aa_c[[1]])
aics <- rbind(aa_a[[2]], aa_c[[2]])

fcast_filename <- paste0("AutoArima", file_suffix, ".csv")
fcast_path <- file.path("tmp", fcast_filename)
write.csv(forecasts, fcast_path, row.names = FALSE)

aic_filename <- paste0("AutoArima", file_suffix, "_model_aic.csv")
aic_path <- file.path("tmp", aic_filename)
write.csv(aics, aic_path, row.names = FALSE)
