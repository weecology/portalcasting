evaluations

devtools::document()
main <- "~/hindcasting"

tiff("1.tiff", width = 10, height = 10, units = "in", res = 400)
plot_casts_err_lead(main)
dev.off()

select_casts(main)

# at the starting point, we want to compare observation to predictions
# at each observation value
# an important consideration is that we want to actually predict the real
# data, not the interpolated data, so we want to make sure to point the
# _interp models to the true data, so we force remove that suffix to align
# predictions appropriately


# constrain things down to a single observation compared with a single 
# predictive distribution 
# we likely want to pull in the model's metadata so we can use the PIs


cast_tab <- read_cast_tab(main = main, cast_id = 1)


add_obs_to_cast_tab(main = main, cast_id = 1)

