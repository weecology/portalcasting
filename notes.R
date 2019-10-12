devtools::document()
devtools::load_all()

main <- "~/portalcasting_testing1"
setup_dir(main)
portalcast(main = main, models = c("jags_RW"))
plot_cast_ts(main=main, model = c("jags_RW"))

plot_cast_point(main=main, model = c("jags_RW"))
plot_cast_point(main=main,with_census=T)
plot_casts_err_lead(main)
plot_casts_cov_RMSE(main)


ESSS_controls <- prefab_model_controls()$ESSS
ESSS_controls$data_sets <- c(ESSS_controls$data_sets, "exclosure_interp")
exclosure_interp_controls <- rodents_control(name = "exclosure_interp", 
                                      interpolate = TRUE,
                                      level = "Treatment", 
                                      treatment = "exclosure",
                                      arg_checks = FALSE)
portalcast(main = main, models = "ESSS", controls_model = ESSS_controls, 
           update_prefab_models = TRUE,  
           controls_rodents = exclosure_interp_controls, arg_checks = FALSE)



