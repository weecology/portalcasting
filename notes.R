devtools::document()
main = "~/testing"
create_dir(main = "~/testing")
fill_raw(main = "~/testing")
moons <- prep_moons(main = "~/testing")
rodents <- prep_rodents(main = "~/testing", moons = moons)
prep_covariates(main = "~/testing")
f_cov <- prep_fcast_covariates(hist_cov = hist_cov, main = "~/testing")
head(hist_cov)

covs <- prep_covariates(main=main)

# prep covariates is done!
# just needs tidying, documentation, and testing, lolz
# then prep_metadata



# figure out a way to make the pulling or updating of min_lag automated



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


# later capacity: 
#  1. make it so download version numbers are retained to allow checking to
#  prevent unneeded downloads
#  2. github
# 

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
