devtools::document()
setup_sandbox("~/sand")
create_dir("~/testingww")
fill_dir("~/testingw")

portalcast("~/testing4", models = c("ESSS", "AutoArima"))
portalcast("~/testing4", models = c("ESSS", "AutoArima"), end_moons = 519:520)

hist_cov <- prep_hist_covariates("~/testingw")
prep_cast_covariates("~/testingpp", hist_cov = hist_cov)


plot_cast_ts("~/sand")
plot_cov_RMSE_mod_spp("~/sand")
plot_err_lead_spp_mods("~/sand")

plot_cast_point("~/sand", with_census = TRUE)
plot_cast_point("~/sand")



devtools::test(filter = "01")
devtools::test(filter = "02")
devtools::test(filter = "03")
devtools::test(filter = "04")
devtools::test(filter = "05")
devtools::test(filter = "06")
devtools::test(filter = "07")
devtools::test(filter = "08")
devtools::test(filter = "09")
devtools::test(filter = "10")
devtools::test(filter = "11")
devtools::test(filter = "12")
devtools::test(filter = "13")
devtools::test(filter = "14")
devtools::test(filter = "15")
devtools::test(filter = "16")
devtools::test(filter = "17")
devtools::test(filter = "18")

# clear the files/folders
devtools::test(filter = "19")


# to do
# update vignettes
# add morgans model
# update pkgdown site
# read through the code documentation again, esp run the examples



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

start_newmoon is now start_moon

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
# 
casts should be saved out as tmnt_type = tmnt_type, rather than 
level = tmnt_type
will need to make the column name matching flexible


