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

# catch up on tests in figures
# move on to bottom part of index page
#  should be a "top x" function? and then apply the plot function over them

#
#

tree <- dirtree(main = "testing_casting")

read_casts(tree, castdate = "2019-02-13")

# new functions:
# read_casts
# plot_cast
# select_casts
# castplot_ylab
# castplot_xaxis
# plot_species_casts
# sppcastsplot_yaxis

plot_cast(tree, species = "total", castdate = "2019-02-13")
plot_species_casts(tree, castdate = "2019-02-13")