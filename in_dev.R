# script for active development
#  pushed to git but ignored in the R package build

# working on bringing the rest of the function code from portalPrediction over
#   working through page by page on the site
# move on to bottom part of index page
#  should be a "top x" function? and then apply the plot function over them

devtools::load_all()
options_all <- all_options(main = "testing_casting", model = models("ESSS"))
setup_dir(options_all)
portalcast(options_all)


rmarkdown::render_site()

cleanup_dir(options_all)


tree <- dirtree(main = "testing_casting")

read_casts(tree)

# new functions:
# fully integrated and tested
# read_casts
# plot_cast
# select_casts
# castplot_ylab
# castplot_xaxis
# plot_species_casts
# sppcastsplot_yaxis

plot_cast(tree, species = "total")
plot_species_casts(tree)