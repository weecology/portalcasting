devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev1")
portalcast(options_all)

# working on
# make subs_type default to "portalcasting" (maybe call it set? )
# switch order of arguments in model_names 

to do:
remove subdirs class
can tidy up create_tmp
and a test in test 10


# to dos (later!): 
# check out the eval plots, the top one has a slightly weird x axis
# messageq function that eliminates the boolean calls
# make the error message hit all faulty arguments and report the function
#  where it happened
# handle cast argument in verify_cast cast_is_valid combo


#'     \code{\link{model_options}}, \code{\link{save_forecast_output}} \cr \cr


