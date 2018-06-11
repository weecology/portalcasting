pevg_a <- pevgarch(rodents$all, covariates, metadata)
pevg_c <- pevgarch(rodents$controls, covariates, metadata, level = "Controls")
save_forecast_output(pevg_a, pevg_c, "pevGARCH", metadata)
