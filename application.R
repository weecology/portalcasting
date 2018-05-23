
filename_suffix <- "forecasts"
forecast_date <- Sys.Date()
portalr::download_observations()
moons <- get_moon_data()
total_moons <- get_moon_data(future = TRUE)
rodent_data <- get_rodent_data(moons, forecast_date)
covariate_data <- get_covariate_data()
model_metadata <- prep_metadata(rodent_data, total_moons, covariate_data)
forecast_covariates <- fcast_covariates(model_metadata, moons, covariate_data)


# note:
# forecastall is now replaced with combine_forecasts and add_ensemble
#