devtools::document()
main = "~/testing2"
setup_dir(main)
fill_data(main)

prep_covariates(main)


# working in the portalcast functions
#
# now onto casts!
#
# also need to bring in model function scripts!

devtools::load_all()
for(i in 1:10){
 if_skip(i == 3)
  print(i)
}




users adding models should permanently add code to model_script_controls()
rather than write their own functions



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


# future ideas
#  make update_list work where you pass it a second list
#   maybe update_list <- function(orig_list, ..., new_list = NULL)
#   and if new_list isn't null then unwind it and use its elements
#   that way you dont have to pass in each argument as x = x if you're 
#   updating a bunch of stuff!
#
#  github download urls


