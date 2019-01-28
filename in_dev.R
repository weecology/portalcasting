# script for active development
#  pushed to git but ignored in the R package build

# working through tests and documentation
# scripts to do:
#  AutoArima, ESSS, nbGARCH, pevGARCH
#  portalcast, prepare_covariates, prepare_metadata, prepare_models,
#  prepare_rodents, prepare_data, process_forecasts
#

devtools::load_all()
options_all <- all_options(main = "ok")
setup_dir(options_all)
portalcast(options_all)

