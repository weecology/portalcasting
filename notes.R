devtools::document()
main = "~/testing1"
create_dir(main)
fill_dir(main)

rodents <- prep_rodents(main)




moons <- prep_moons(main = "~/testing")

prep_covariates(main = "~/testing")
f_cov <- prep_fcast_covariates(hist_cov = hist_cov, main = "~/testing")
head(hist_cov)
cc <- cast_covariates(main)
covs <- prep_covariates(main=main)


# to do

# use verbose more (re downloads)

# figure out a way to make the pulling or updating of min_lag automated

# still work tidying up

# can transpose_args be replaced?

# I WILL NEED TO MANAGE SOMETHING WITH THE ARCHIVED FILE NAME CHANGE
# the col name is also updated to cast_newmoon

# vignettes

needs test
  forecast_covariates: all of cast_covariates
  prep_covariates: summarize_daily_weather_by_newmoon
  prepare_moons: trim_moons , add_newmoons_from_date, target_moons
  utilities: forecast_window, add_date_from_components combine_hist_and_cast
  prepare_covariates: summarize_daily_weather_by_newmoon, prep_weather_data,
    prep_hist_covariates
  prepare_metadata

needs example
  prep_covariates: summarize_daily_weather_by_newmoon, prep_weather_data,
   prep_hist_covariates
 utilities: combine_hist_and_cast
  prepare_moons: trim_moons, add_newmoons_from_date, target_moons
  forecast_covariates: all cast_covariates
  prepare_metadata




users adding models should permanently add code to model_script_controls()
rather than write their own functions

adding a new setup of the rodents data is possible now through direct
arguments basically to the summarize rodents function
users can set their own controls as they need
but permanent additions can also be made by adding a tmnt_type to 
rodents_control(s)

make matching of forecast and Forecast and forecasts etc etc


THE FORMALS MATCHING IN PREP_RODENTS MIGHT BE BETTER TO FUNCTION
AND USE WITHIN FILL_DATA FOR EXAMPLE

end_step is now out of the controls list, because come on


# we also might want to create a simple repo for testing!
# basically, like the PD and PP, but just for tests


 

# important notes for news
#  directory tree structure simplified
#   dirtree is no more, even tho the structure is still there
#   base is no more, if you want to make that structure use main = "./name"
#  download capacity generalized
#   any zenodo record or concept; any ready-to-go url; (will add any github)
#  PortalData has been removed as a sub and replaced with "raw", which 
#   includes all unzipped raw downloads (portal data and portal predictions)
#  users adding models should permanently add code to model_script_controls()
#   rather than write their own control functions
# all the NMME options! :D
# covariate forecasts are now saved in the raw folder for reference 
#   rather than only ever loaded within R
# (mostly) internal generalization to cast 
#    (there were still lots of "fcast" names

# future ideas
#  a future nice thing will be making update_list work where you pass
#  it a second list
# maybe update_list <- function(orig_list, ..., new_list = NULL)
# and if new_list isn't null then unwind it and use its elements
# that way you dont have to pass in each argument as x = x if you're updating
# a bunch of stuff!
#
# github download urls
