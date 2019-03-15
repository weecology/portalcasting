devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev1")
portalcast(options_all)


# working status
# integrate utilities functions and the model functions  into check_args
# update model functions and scripts to take better advantage of the tidied 
# calls



# to dos (later!): 
# make subs_type default to "portalcasting"
# messageq function that eliminates the boolean calls
# make the error message hit all faulty arguments and report the function
#  where it happened
# remove the double up of CI_level and confidence_level
# remove the double up of name and model
# handle cast argument in verify_cast cast_is_valid combo
# set is used for both species (rodent_spp) and models (model_names)

check out the eval plots, the top one has a slightly weird x axis



