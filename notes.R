devtools::document()

# run for example testing




devtools::test(filter = "01")


# clear the files/folders
devtools::test(filter = "19")


# to do
add timestamp to forecast filenames
soften model running errors 
# update vignettes
# add morgans model
# update pkgdown site
# read through the code documentation again



 # I WILL NEED TO MANAGE SOMETHING WITH THE ARCHIVED FILE NAME CHANGE
# the col name is also updated to cast_newmoon





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

add examples to process casts
