nbgarch_a <- nbgarch(rodents$all, metadata)
nbgarch_c <- nbgarch(rodents$controls, metadata, level = "Controls")
save_forecast_output(nbgarch_a, nbgarch_c, "nbGARCH", metadata)
