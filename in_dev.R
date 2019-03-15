devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev1")
portalcast(options_all)

# to dos : 
# integrate utilities functions into check_args
# make subs_type default to "portalcasting"
# remove the double up of CI_level and confidence_level
# remove the double up of name and model
# set is used for both species (rodent_spp) and models (model_names)


# to dos (later!): 
# check out the eval plots, the top one has a slightly weird x axis
# messageq function that eliminates the boolean calls
# make the error message hit all faulty arguments and report the function
#  where it happened
# handle cast argument in verify_cast cast_is_valid combo




