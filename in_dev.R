# script for active development
#  pushed to git but ignored in the R package build

# working on bringing the rest of the function code from portalPrediction over
#   working through page by page on the site: just evaluation page now
#  uh oh, that's not RMSE


devtools::load_all()
options_all <- all_options(main = "testing_casting")
tree <- dirtree(main = "testing_casting")

setup_dir(options_all)
portalcast(options_all)

rmarkdown::render_site()

cleanup_dir(options_all)


plot_cast_ts(tree)
plot_cast_point(tree = tree, with_census = T)

casts <- read_casts(tree)

# new functions...some of the names might still change...
#
# fully integrated and tested:
# read_casts -> read_cast
# plot_cast -> plot_cast_ts
# select_casts -> select_cast
# castplot_ylab -> plotcastts_ylab
# castplot_xaxis -> plotcastts_xaxis
# plot_species_casts -> plot_cast_point
# sppcastsplot_yaxis -> plotcastpoint_yaxis
# select_most_ab_spp
# most_recent_cast
# most_recent_census

# in progress:
# plot_eval_recent (figures)
# read_casts (process_forecasts)
# cast_is_valid (process_forecasts)
# verify_cast (process_forecasts)
# na_conformer (utilities)

# need to add in testing for stuff that uses most_recent_census
# unfort that's all requiring of at least some predictions, so hits the 
# test_location issue

# think of compile_casts as wrapper on read_casts for reading in multiples
# and combining them


