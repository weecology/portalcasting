# script for active development
#  pushed to git but ignored in the R package build

# working on bringing the rest of the function code from portalPrediction over
#   working through page by page on the site



devtools::load_all()
options_all <- all_options(main = "testing_casting")
setup_dir(options_all)
portalcast(options_all)
tree <- dirtree(main = "testing_casting")

rmarkdown::render_site()

cleanup_dir(options_all)



plot_species_casts(tree = tree, level = "All", with_census=T)


# new functions:
# fully integrated and tested
# read_casts
# plot_cast
# select_casts
# castplot_ylab
# castplot_xaxis
# plot_species_casts
# sppcastsplot_yaxis
# select_most_ab_spp
# most_recent_cast
# most_recent_census


