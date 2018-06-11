aa_a <- autoarima(rodents$all, metadata)
aa_c <- autoarima(rodents$controls, metadata, level = "Controls")
save_forecast_output(aa_a, aa_c, "AutoArima", metadata)
