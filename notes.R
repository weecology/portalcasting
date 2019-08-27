devtools::document()
main <- "~/testing"
setup_dir(main)
portalcast(main)
portalcast(main, end_moon = 440:445)
portalcast(main, end_moon = 515)





obvi need to recheck all of the examples

# run for example testing



setup_sandbox(main)

fill_predictions(main)
cast <- AutoArima(data_set = "all", main = main)

ok, i think the saving machinery is all set up and working on the updated
cast output
autoarima makes the new output
other models need to be made like it

ok so save_cast_output will be key for passing whatever the output of the 
model function is through to the predictions sub, and archiving it


starting with portalcasting 0.9.0, forecast files will have a name that
is an identifier that tracks info about the forecast in the cast_metadata file

starting w 0.9.0, no more interpolation unless needed
enforce rodents summary stuff in args

devtools::test(filter = "05")


# clear the files/folders
devtools::test(filter = "19")


# to do
# update vignettes
# add morgans model
# update pkgdown site
# read through the code documentation again

news
no longer forced interpolation
no longer combining casts
no longer building the ensemble as is

default CL is 0.95

 # I WILL NEED TO MANAGE SOMETHING WITH THE ARCHIVED FILE NAME CHANGE
# the col name is also updated to cast_newmoon

