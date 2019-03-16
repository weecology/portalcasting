devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

options_dir <- options_all$options_dir


setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev1")
portalcast(options_all)

# to dos (later!): 
# check out the eval plots, the top one has a slightly weird x axis
# make the error message hit all faulty arguments and report the function
#  where it happened
# handle cast argument in verify_cast cast_is_valid combo
