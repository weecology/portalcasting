
# general data preparation

portalr::download_observations()
rodents <- prep_rodent_data()
covariates <- prep_covariate_data()
metadata <- prep_metadata(rodents, covariates)

# forecast using each model 

aa_a <- autoarima(rodents$all, metadata)
esss_a <- esss(rodents$all, metadata)
nbg_a <- nbgarch(rodents$all, metadata)
pevg_a <- pevgarch(rodents$all, covariates, metadata)


# notes:
# forecastall is now replaced with combine_forecasts and add_ensemble
#

