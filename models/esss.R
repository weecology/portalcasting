esss_a <- esss(rodents$all, metadata)
esss_c <- esss(rodents$controls, metadata, level = "Controls")
save_forecast_output(esss_a, esss_c, "ESSS", metadata)
