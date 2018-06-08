
# kinda start back over and figure out what's the tidiest way to work through

download_observations()
rodents <- prep_rodent_data()
covariates <- prep_covariate_data()
metadata <- prep_metadata(rodents, covariates)



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