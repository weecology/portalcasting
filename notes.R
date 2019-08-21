putting lots of tolowers in!


devtools::document()
setup_dir("~/testing4")
create_dir("~/testing4")
fill_dir("~/testing4")

portalcast("~/testing4", models = c("ESSS", "AutoArima"))
portalcast("~/testing4", models = c("ESSS", "AutoArima"), end_moons = 519:520)

hist_cov <- prep_hist_covariates("~/testing4")
prep_cast_covariates("~/testing4", hist_cov = hist_cov)


plot_cast_ts("~/testing4")
plot_cov_RMSE_mod_spp("~/testing4")
plot_err_lead_spp_mods("~/testing4")

plot_cast_point("~/testing4", with_census = TRUE)
plot_cast_point("~/testing4")


ok, so i like the idea, but the execution just isn't quite right
i'm pulling the plug on pass and call for right now.
work this out later



# to do
# tests
# update vignettes
# add morgans model

casts should be saved out as tmnt_type = tmnt_type, rather than 
level = tmnt_type
will need to make the column name matching flexible

funcitons to test
all the models (now in prefab_models
most_recent_census (process data)
most_recent_cast (process casts)
na_conformer (utilities)
verify cast, cast is valid, column conformer, select casts
  measure cast error append observed to cast (process casts)
the figure functions


start_newmoon is now start_moon


 # I WILL NEED TO MANAGE SOMETHING WITH THE ARCHIVED FILE NAME CHANGE
# the col name is also updated to cast_newmoon

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
# adding a new setup of the rodents data is possible now through direct
#   arguments basically to the summarize rodents function
#   users can set their own controls as they need
#   but permanent additions can also be made by adding a tmnt_type to 
#   rodents_control(s)
# cast_type has been removed as an input, it's auto determined now based on 
#  end_moon and the last moon available (if they're equal it's a forecast, 
#  if not, it's a hindcast)
#
# hindcasts are now not skipping from incomplete/missed samples 
work to get the arg_checks arg passed through the code cleanly!

some basic utilities and
the prefab model functions dont have arg_checks capability, intentionally


put in a toggle to turn off arg checking at the top level and down thru?



# future ideas
#  make update_list work where you pass it a second list
#   maybe update_list <- function(orig_list, ..., new_list = NULL)
#   and if new_list isn't null then unwind it and use its elements
#   that way you dont have to pass in each argument as x = x if you're 
#   updating a bunch of stuff!
#
#  github download urls
#
#  make the model script writing and forecast processing work for different
#   treatment levels
#
# can i use check_args in the ... functions (utilities)


