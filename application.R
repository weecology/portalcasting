
fcast_date <- Sys.Date()
portalr::download_observations()
moons <- get_moon_data()
rodents <- get_rodent_data(moons, Sys.Date())
covariates <- get_covariate_data()
metadata <- prep_metadata(rodents, covariates, fcast_date, "forecasts")
forecast_covariates <- fcast_covariates(metadata, moons, covariates)

append_covariate_fcast(forecast_covariates, "historical_covariates.csv")

# note:
# forecastall is now replaced with combine_forecasts and add_ensemble
#



aa_a <- autoarima(rodents$all, metadata)
esss_a <- esss(rodents$all, metadata)
nbg_a <- nbgarch(rodents$all, metadata)
#pevg_a <- pevgarch(rodents$all, covariates, forecast_covariates, metadata)