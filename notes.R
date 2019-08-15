devtools::document()
main = "~/testing1"
create_dir(main)
fill_dir(main)

moons <-prep_moons(main)

pc <- prep_covariates(main)
  




# I WILL NEED TO MANAGE SOMETHING WITH THE ARCHIVED FILE NAME CHANGE
# the col name is also updated to cast_newmoon

# vignettes





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
# use verbose more (re downloads)
# figure out a way to make the pulling or updating of min_lag automated
