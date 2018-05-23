
fcast_date <- Sys.Date()
portalr::download_observations()
moons <- get_moon_data()
rodents <- get_rodent_data(moons, Sys.Date())
covariates <- get_covariate_data()
metadata <- prep_metadata(rodents, covariates, fcast_date, "forecasts")
forecast_covariates <- fcast_covariates(metadata, moons, covariates)


# note:
# forecastall is now replaced with combine_forecasts and add_ensemble
#