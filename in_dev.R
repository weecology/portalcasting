# script for active development
#  pushed to git but ignored in the R package build

# working through tests and documentation
# scripts to do:
#  AutoArima, ESSS, nbGARCH, pevGARCH
#  portalcast, prepare_covariates, prepare_metadata, prepare_models,
#  prepare_rodents, prepare_data, process_forecasts
#

devtools::load_all()
options_all <- all_options(main = "testing_casting")
setup_dir(options_all)
portalcast(options_all)


rmarkdown::render_site()

cleanup_dir(options_all)


#
#

tree <- dirtree(main = "testing_casting")


# new functions:
# read_casts
# plot_cast
# select_cast
# castplot_ylab
# castplot_xaxis

plot_cast(tree, species = "NA", castdate = "2019-02-13")
