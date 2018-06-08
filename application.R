
# kinda start back over and figure out what's the tidiest way to work through

portalr::download_observations()

rodents <- prep_rodent_data()
covariates <- prep_covariate_data(forecasts = TRUE)
metadata <- prep_metadata(rodents, covariates)

covariates <- append_covariate_fcast(covariates, moons, metadata)










# note:
# forecastall is now replaced with combine_forecasts and add_ensemble
#

# need to make it so that append checks
# stuff is very much still in progress, tidying all the code as it comes 
#   together

aa_a <- autoarima(rodents$all, metadata)
esss_a <- esss(rodents$all, metadata)
nbg_a <- nbgarch(rodents$all, metadata)
#pevg_a <- pevgarch(rodents$all, covariates, forecast_covariates, metadata)