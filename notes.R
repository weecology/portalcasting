# working in the check_args implementation and tidying
# directory set up functions are all clear
casting functions looking good so far

figure out if i can use check_args in the ... functions (utilities)

  fun_call <- match.call.defaults(definition = sys.function(-1), 
                                  call = sys.call(-1))
  fun_class <- class(as.list(fun_call)[[1]])
  if(fun_class == "name"){  
    fun_name <- as.list(fun_call)[[1]]
    arg_values <- as.list(fun_call)[-1]
    arg_names <- names(arg_values)
  } else if(fun_class == "function"){

    fun_call2 <- match.call.defaults(definition = sys.function(-2), 
                                     call = sys.call(-2))
    fun_name <- as.list(fun_call2)[[1]]

print(sys.function(-2))
print(as.list(fun_call)[[1]])
print(as.list(fun_call2)[-1][[1]])

  arg_names <- names(as.list(fun_call)[-1])
  names(arg_values) <- arg_names
print(arg_values)



nargs <- length(fun_call) - 2
arg_values <- vector("list", length=nargs)
for(i in 1:nargs){
print(i)
  arg_values[[i]] <- eval(as.list(fun_call)[-1][[i]], 
                          envir = parent.frame(3L))

}


topx 
most abundant spp?

yup that needs to be added, it gets used in portalpredictions

devtools::document()
main = "~/testing"



#' @details 


some of the utilities cant use check_arg
like list_depth is recursive

make a species species argument for plot_cast_ts
(plot_species)? something thats only length 1


# to do
# tests
# update vignettes
# add morgans model
# check_arg and args

create_dir(main)


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


