# script for active development
#  pushed to git but ignored in the R package build

# working on bringing the rest of the function code from portalPrediction over
#   working through page by page on the site: just evaluation page now
#  uh oh, that's not RMSE, it's absolute point-wise error


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




model = NULL
species = rodent_spp(set = "evalplot")
level = "Controls"
cast_dates = NULL
cast_type = "forecasts"



  casts <- read_casts(tree, cast_type, cast_dates) %>%
           select_casts(species = species, level = level, model = model) %>%
           append_observed_to_cast(tree)
  cast_errors <- measure_cast_error(casts)



# need to add in testing for stuff that uses most_recent_census
# unfort that's all requiring of at least some predictions, so hits the 
# test_location issue


