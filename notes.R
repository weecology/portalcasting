
working on the DM ricker model
have a DM_controls data set which I can run the jags_RW on just fine
now need to build the jags_ricker


devtools::load_all()
main <- "~/builder"
x <- fill_data(main, quiet=F)

jags_RW(main = main, data_set = "dm_controls",
        control_runjags = runjags_control(silent_jags = FALSE))





AutoArima(main, "controls")



devtools::document()
devtools::load_all()

devtools::test(filter="01")
devtools::test(filter="02")
devtools::test(filter="03")
devtools::test(filter="04")
devtools::test(filter="05")
devtools::test(filter="06")
devtools::test(filter="07")
devtools::test(filter="08")
devtools::test(filter="09")
devtools::test(filter="10")
devtools::test(filter="11")
devtools::test(filter="12")
devtools::test(filter="13")
devtools::test(filter="14")
devtools::test(filter="15")
devtools::test(filter="16")
devtools::test(filter="17")
devtools::test(filter="18")
devtools::test(filter="19")
devtools::test(filter="20")
devtools::test(filter="21")


testing of verify data and the function should move out of downloads


test_that("verify_raw_data", {
  skip_on_cran() # downloads take too long for cran checks
  expect_equal(verify_raw_data(main = "./testing"), TRUE)
})


test_that("download", {
  skip_on_cran() # downloads take too long for cran checks
  expect_message(download("PortalData", "zenodo", main = "./testing",
                          concept_rec_id = "1215988"))
})







main <- "./model_testing"
setup_dir(main)


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



summarize_rodent_data("./model_testing/raw")


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



