main <- "~/pct"

devtools::load_all()
setup_production(main = main)

jr   <- jags_RW(main = main)
jl   <- jags_logistic(main = main) 
jlw  <- jags_logistic_covariates(main = main) 
jlc  <- jags_logistic_competition(main = main)
jlcw <- jags_logistic_competition_covariates(main = main)

summary(jr$model_fits$DM)
summary(jl$model_fits$DM)
summary(jlc$model_fits$DM)
summary(jlw$model_fits$DM)
summary(jlcw$model_fits$DM)


x <- read_covariates(main = main)

head(x)

plot(x$warm_precip_3_moon ~ as.Date(x$date), type = "l")
plot(x$ndvi_13_moon ~ as.Date(x$date), type = "l")
plot(x$meantemp ~ as.Date(x$date), type = "l")
plot(x$precipitation ~ as.Date(x$date), type = "l")
plot(x$warm_precip ~ as.Date(x$date), type = "l")
plot(x$ndvi ~ as.Date(x$date), type = "l")
xx <- lm(x$ndvi ~ as.Date(x$date))
abline(xx)
