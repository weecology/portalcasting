# working here on portalcasting, 

####   roll through all the code front to back for use
####    then update tests


devtools::document()
devtools::load_all()

main <- "~/work_please"
setup_dir(main)




do we use   

remove_incompletes


now anywhere else?



need to manage covariate casts when not from 'today'



REMOVE FROM ALL DOCS

prep_weather_data



  colnames(out)[which(colnames(out) == "newmoonnumber")] <- "moon"
  if (!is.null(end_moon)){
    out <- out[which(out$moon <= end_moon), ]
  }


  cols_to_drop <- c("year", "month", "day", "battery_low", "locally_measured")
  cols_in <- !(colnames(hist_tab) %in% cols_to_drop)
  hist_cov <- hist_cov[ , cols_in]

  cov_table <- add_moons_from_date(df = cov_table, moons = moons, 
                                   arg_checks = arg_checks)




portalcast(main)

rt <- read_rodents_table(main, "all")





head(rt)
rt$total
plot(rt$SH~rt$moon)
nas <- which(is.na(rt$SH == TRUE))
td <- diff(rt$SH[-nas])
t1 <- rt$SH[-nas]
t1 <- t1[-length(t1)]
md <- diff(rt$moon[-nas])
pgr <- td/(t1*md)
m1 <- rt$moon[-nas]
m1 <- m1[-length(m1)]
plot(pgr~m1)





head(rt)






hist(pgr)
plot(pgr)

portalcast(main = main, models = c("pevGARCH"))
plot_cast_ts(main=main, model = c("ESSS"), species = "SH", 
             data_set = "all_interp")

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



