# working here on portalcasting, 


devtools::document()
devtools::load_all()

main <- "./portalcasting"

setup_production(main)
portalcast(main)

# updating to working with the settings list
# minimizing other changes to the degree possible

# remaining figures are in figs script
# cast tab adding obs etc isnt working


##############################################





####   roll through all the code front to back for use
####    then update tests




main <- "./testing"
setup_dir(main)
portalcast(main, models = "ESSS")

plot_cast_ts(main = main, data_set = "controls")
plot_cast_point(main = main, data_set = "controls")
most_ab <- most_abundant_species(main = main, data_set = "controls")
for(i in 1:3){
  plot_cast_ts(main = main, data_set = "controls", species = most_ab[i])
}
plot_casts_err_lead(main = main)
plot_casts_cov_RMSE(main = main)


devtools::build()
devtools::test()



list.files("tests/testthat")
devtools::test(filter = "01")
devtools::test(filter = "02")
devtools::test(filter = "03")
devtools::test(filter = "04")
devtools::test(filter = "05")
devtools::test(filter = "06")
devtools::test(filter = "07")
devtools::test(filter = "08")
devtools::test(filter = "09")
devtools::test(filter = "10")
devtools::test(filter = "11")
devtools::test(filter = "12")
devtools::test(filter = "13")
devtools::test(filter = "14")
devtools::test(filter = "15")
devtools::test(filter = "16")
devtools::test(filter = "17")
devtools::test(filter = "18")
devtools::test(filter = "19")
devtools::test(filter = "20")
devtools::test(filter = "21")
devtools::test(filter = "99")



covr::package_coverage()

main <- "~/what_nowfff"
setup_dir(main)

# set up is good to go!
#

# # tidying up # # 


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



