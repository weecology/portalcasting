# script for active development
#  pushed to git but ignored in the R package build

# working in the forecast_covariates tests
# within forecast_covariates working on hindcasts

options_all <- all_options(main = "ok")
setup_dir(options_all)
moons <- prep_moons(ops_$options_data$moons)
hist_cov <- prep_hist_covariates(options_all$options_data$covariates)
options_covariates <- options_all$options_data$covariates
up_cov_opts <- update_covfcast_options(options_covariates, hist_cov, moons)

moons2 <- trim_moons_fcast(moons, options_covariates = up_cov_opts)

add tests to make sure covariates are covariates