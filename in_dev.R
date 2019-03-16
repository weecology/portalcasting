devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev",
                          download_existing_predictions = TRUE)


options_dir <- options_all$options_dir


setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev")
portalcast(options_all)

# to dos (later!): 
# make the error message hit all faulty arguments and report the function
#  where it happened
