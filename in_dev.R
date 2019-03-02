# script for active development
#  pushed to git but ignored in the R package build


devtools::load_all()
options_all <- all_options(main = "testing_casting")
tree <- dirtree(main = "testing_casting")

setup_dir(options_all)
portalcast(options_all)

rmarkdown::render_site()

cleanup_dir(options_all)


plot_cast_ts(tree)
plot_cast_point(tree = tree, with_census = T)
plot_err_lead_spp_mods(tree)
plot_cov_RMSE_mod_spp(tree)


