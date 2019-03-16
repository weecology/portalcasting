devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev1")
portalcast(options_all)

# to dos (later!): 
# check out the eval plots, the top one has a slightly weird x axis
# messageq function that eliminates the boolean calls
# make the error message hit all faulty arguments and report the function
#  where it happened
# handle cast argument in verify_cast cast_is_valid combo
# tidy up the documentation in check_args

#'   \code{msg}: must be a length-1 \code{character} vector in
#'     \code{\link{messageq}} \cr \cr