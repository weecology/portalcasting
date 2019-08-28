argument order alignment


to add a model permanently to prefab

add the controls to the \code{prefab_model_controls} non-exported function
in the \code{prepare_models.R} script
will automatically add the model to the prefab_models 



sames for data sets

path arguments...

path = NULL, dir_level = NULL, arg_checks = TRUE)

path = NULL, dir_level = NULL, quiet = FALSE, 
                   verbose = FALSE, arg_checks = TRUE

combine file path and model path duhhhh

devtools::document()
main <- "~/test_1"
setup_dir(main)

fill_data(main, end_moon = 520)

AA<-AutoArima(main)


main <- "~/test_2"
setup_sandbox(main)

main <- "~/test_3"
setup_production(main)

fill_dir(main)
fill_raw(main)
fill_casts(main)
fill_models(main)
prep_moons(main, end_moon=519)
prep_rodents(main, end_moon=59)

portalcast(main)
portalcast(main, end_moon = 440:445)
portalcast(main, end_moon = 515)


put all of the pertinent information in the cast_metadata


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

