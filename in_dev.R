devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

# presently integrating functions into check_args
# now on process_data
# skipping the model scripts for now
#
#  step back to making rodents actually rodents_list
#
# update data to rodents
#changes (add to news)
# prep_rodents to prep_rodents_list
# rodents_data to prep_rodents
# update_rodents to update_rodents_list
#read_ functions

# lag_data to lag_covariates

# dropped the models class, that was unnecessary

# things to align the rest of the way
# ugg that toggle for plotting


#setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev")
portalcast(options_all)



all <- read_data(tree, "all")


plot(all$newmoonnumber, all$total, type = "l")

y <- all$total
x <- all$newmoonnumber

abundances <- all
level <- "All"
metadata <- read_data(tree, "metadata")

mo <- LTAvg(all, metadata, level)

