# script for active development
#  pushed to git but ignored in the R package build

#   working through page by page on the site: just evaluation page now


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


casts <- read_casts(tree)



species <- rodent_spp(set = "evalplot")
fcasts <- read_casts(tree) %>%
          select_casts(species = species, level = "Controls") %>%
          append_observed_to_cast(tree)
fcast_errors <- measure_cast_error(fcasts)
hcasts <- read_casts(tree, cast_type = "hindcasts") %>%
          select_casts(species = species, level = "Controls") %>%
          append_observed_to_cast(tree)
hcast_errors <- measure_cast_error(hcasts)

casts <- fcasts

devtools::load_all()



        if (colc == nmodels){

          mtext(side = 4, spt, cex = 0.4, font = spf, line = 0.5, las = 0) 
        }
