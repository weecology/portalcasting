main <- "~/pc"

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