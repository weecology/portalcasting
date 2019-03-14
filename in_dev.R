devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE,
models = "AutoArima")

# thoughts: 
# make subs_type default to "portalcasting"

# presently integrating functions into check_args
#  what's left for now: a chunk of the process_forecasts functions and the 
#  model functions (both are undergoing some updates)
#
# using the match.call approach
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


#'   \code{}: must be \code{} in
#'     \code{\link{}}  \cr \cr

#setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev")
portalcast(options_all)

options_covariates$fcast_nms <- 12
forecast_covariates(covariates, moons, 
                                options_covariates)

moons <- read_moons(tree)
covariates <- read_covariates(tree)

all <- read_data(tree, "all")


plot(all$newmoonnumber, all$total, type = "l")

y <- all$total
x <- all$newmoonnumber

abundances <- all
level <- "All"
metadata <- read_data(tree, "metadata")

mo <- LTAvg(all, metadata, level)

