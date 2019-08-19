# working in the check_args implementation and tidying
# directory set up functions are all clear

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



#' @details Usage and rules for arguments are as follows: \cr
#'   \code{add_error}: must be a length-1 \code{logical} vector.\cr
#'   \code{add_in_window}: must be a length-1 \code{logical} vector.\cr
#'   \code{add_lead}: must be a length-1 \code{logical} vector.\cr
#'   \code{add_obs}: must be a length-1 \code{logical} vector. \cr
#'   \code{all_casts}: must be a data.frame.\cr
#'   \code{all}: must be a \code{list} with elements named \code{forecast} 
#'     and \code{aic}.\cr
#'   \code{append_cast_csv}: must be a length-1 \code{logical} vector.   \cr
#'   \code{cast_cov}: must be a \code{data.frame}. \cr
#'   \code{cast_covariates}: must be a length-1 \code{logical} vector. \cr
#'   \code{cast_date}: must be \code{NULL} or a length-1 \code{Date} or 
#'     \code{Date}-conformable vector. \cr
#'   \code{cast_type}: must be a length-1 \code{character} vector. \cr
#'   \code{cast}: must be a data.frame.\cr
#'   \code{cast_to_check}: must be a data.frame.\cr
#'   \code{cast_tab}: must be a \code{data.frame}.\cr
#'   \code{casts}: must be a data.frame.\cr
#'   \code{cleanup}: must be a length-1 \code{logical} vector.\cr
#'   \code{colname}: must be a length-1 \code{character} vector. \cr
#'   \code{concept_rec_id} must be a length-1 \code{character} vector.\cr
#'   \code{confidence_level}: must be a length-1 \code{numeric} value between
#'     0 and 1. \cr
#'   \code{controls}: must be a \code{list} with elements named 
#'     \code{forecast} and \code{aic}.\cr
#'   \code{control_cdl}: must be of class \code{list}. \cr
#'   \code{controls_m}: must be of class \code{list}. \cr
#'   \code{controls_r}: must be of class \code{list}. \cr
#'   \code{covariates}: must be a \code{data.frame}.\cr
#'   \code{covariatesTF}: must be a length-1 \code{logical} vector.\cr
#'   \code{data}: must be a length-1 \code{character} vector. \cr
#'   \code{date}: must be \code{NULL} or a \code{Date} or 
#'     \code{Date}-conformable vector. \cr
#'   \code{df}: must be a \code{data.frame}.\cr
#'   \code{dfv}: must be a \code{data.frame} or \code{vector}.\cr
#'   \code{dir_level}: must be a \code{character} vector. \cr
#'   \code{downloads}: must be of class \code{list}. \cr
#'   \code{end}: must be \code{NULL} or a length-1 \code{Date} or 
#'     \code{Date}-conformable vector. \cr
#'   \code{end_moons}: must be \code{NULL} or a positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{end_moon}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{enquote_args}: must be a \code{character} vector. \cr
#'   \code{ensemble}: must be a length-1 \code{logical} vector. \cr
#'   \code{eval_args}: must be a \code{character} vector. \cr
#'   \code{extension}: must be a length-1 \code{character} vector with
#'     a single period. \cr
#'   \code{filename}: must be a length-1 \code{character} vector. \cr
#'   \code{filename_moons}: must be a length-1 \code{character} vector. \cr 
#'   \code{filename_cov}: must be a length-1 \code{character} vector. \cr
#'   \code{filename_meta}: must be a length-1 \code{character} vector. \cr
#'   \code{filename_moons}: must be a \code{character} vector. \cr
#'   \code{freq}: must be a length-1 \code{character} vector. \cr
#'   \code{from_date}: must be \code{NULL} or a length-1 \code{Date} or 
#'     \code{Date}-conformable vector. \cr
#'   \code{hist_cov}: must be a \code{data.frame}. \cr
#'   \code{hist_covariates}: must be a length-1 \code{logical} vector.\cr
#'   \code{hist_tab}: must be a \code{data.frame}.\cr
#'   \code{in_args}: must be a \code{list}.\cr
#'   \code{lag}: must be \code{NULL} or a length-1 non-negative \code{integer}
#'     or \code{integer}-conformable vector. \cr
#'   \code{lat}: must be a length-1 \code{numeric} value. \cr
#'   \code{lead}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{lead_time}: must be a length-1 non-negative \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{lev}: must be a length-1 \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{level}: must be \code{NULL} or a length-1 \code{character} vector
#'    \cr
#'   \code{local_paths}: must be a \code{character} vector. \cr
#'   \code{lon}: must be a length-1 \code{numeric} value. \cr
#'   \code{main} must be a length-1 \code{character} vector. \cr
#'   \code{min_lag}: must be a length-1 non-negative \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{min_observed}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{min_plots}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector.\cr
#'   \code{min_traps}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector.\cr
#'   \code{model}: must be a length-1 \code{character} vector. \cr
#'   \code{models}: must be \code{NULL} or a \code{character} vector. \cr
#'   \code{moons}: must be a \code{data.frame}. \cr
#'   \code{movedTF}: must be a \code{logical} vector. \cr
#'   \code{msg}: must be \code{NULL} or a \code{character} vector.\cr
#'   \code{ndates}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{nadot}: must be a length-1 \code{logical} vector
#'   \code{name} must be a length-1 \code{character} vector.\cr
#'   \code{newmoonnumbers}: must be a \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{nmoons}: must be \code{NULL} or a length-1 non-negative 
#'     \code{integer} or \code{integer}-conformable vector. \cr
#'   \code{NULLname} must be a length-1 \code{character} vector.\cr
#'   \code{output}: must be \code{"abundance"}.\cr
#'   \code{overwrite}: must be a length-1 \code{logical} vector. \cr
#'   \code{path}: must be a \code{character} vector. \cr
#'   \code{plots}: must be a length-1 \code{character} vector of value
#'     \code{"all"} or \code{"longerm"}.\cr
#'   \code{quiet}: must be a length-1 \code{logical} vector.  \cr
#'   \code{rangex}: must be a length-2 positive \code{integer} or 
#'     \code{integer}-conformable vector.\cr
#'   \code{raw_path_archive} must be a length-1 \code{character} vector.\cr
#'   \code{raw_cov_cast_file} must be a length-1 \code{character} vector.\cr
#'   \code{raw_path_data} must be a length-1 \code{character} vector.\cr
#'   \code{raw_path_cov_cast} must be a length-1 \code{character} vector.\cr
#'   \code{raw_path_predictions} must be a length-1 \code{character} vector.
#'     \cr
#'   \code{raw_moons_file} must be a length-1 \code{character} vector.\cr
#'   \code{raw_traps_file} must be a length-1 \code{character} vector.\cr
#'   \code{rec_id} must be a length-1 \code{character} vector.\cr
#'   \code{rec_version} must be a length-1 \code{character} vector.\cr
#'   \code{ref_species}: must be a \code{character} vector.\cr
#'   \code{retain_target_moons}: must be a length-1 \code{logical} vector. \cr
#'   \code{rodents}: must be of class \code{list}.\cr
#'   \code{rodents_tab}: must be a \code{data.table}.\cr
#'   \code{save}: must be a length-1 \code{logical} vector. \cr
#'   \code{sep_char} must be a length-1 \code{character} vector.\cr
#'   \code{set}: must be \code{NULL} or a length-1 \code{character} vector.\cr
#'   \code{source_name}: must be a length-1 \code{character} vector. \cr
#'   \code{source_url} must be a length-1 \code{character} vector.\cr
#'   \code{species}: must be \code{NULL} or a \code{character} vector.\cr
#'   \code{specific_sub} must be a length-1 \code{character} vector.\cr
#'   \code{specific_subs} must be a \code{character} vector.\cr
#'   \code{start}: must be \code{NULL} or a length-1 \code{Date} or 
#'     \code{Date}-conformable vector. \cr
#'   \code{start_newmoon}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{start_moon}: must be a length-1 positive \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{subs} must be a class-\code{character}. \cr
#'   \code{subs_names}: must be \code{NULL} or a \code{character} 
#'     vector. \cr
#'   \code{subs_type}: must be \code{NULL} or a length-1 \code{character} 
#'     vector. \cr
#'   \code{tail}: must be a length-1 \code{logical} vector.\cr
#'   \code{target_moons}: must be a \code{integer} or 
#'     \code{integer}-conformable vector. \cr
#'   \code{target_cols} must be a \code{character} vector.\cr
#'   \code{tmnt_type}: must be \code{NULL} or a length-1 \code{character} 
#'     vector of value \code{"all"} or \code{"controls"} .\cr
#'   \code{total}: must be a length-1 \code{logical} vector. \cr
#'   \code{treatment}: must be \code{NULL} or \code{"control"}.\cr
#'   \code{type} must be a length-1 \code{character} vector.\cr
#'   \code{url} must be a length-1 \code{character} vector.\cr
#'   \code{verbose}: must be a length-1 \code{logical} vector.\cr 
#'   \code{with_census}: must be a length-1 \code{logical} vector. \cr
#'   \code{winner}: must be a length-1 \code{character} vector with value
#'    \code{"hist"} or \code{"cast"}.\cr
#'   \code{zip_destin} must be a length-1 \code{character} vector.\cr
#'
#' @param arg_name \code{character} value of the argument name.
#'
#' @param arg_value Input value for the argument.
#'
#' @param fun_name \code{character} value of the function name or \code{NULL}.
#'
#' @return \code{check_arg}: \code{character} vector of error messages
#'  associated with the specific situation 
#'  (\code{<fun_name>(<arg_name> = arg_value)}).


    if(!is.null(arg_value)){



out1 <- paste0("`", arg_name, "` ")

dates....


length should be 1 or whatever or NULL
can i get rid of the name element???

   = arg_check_(""),
   = arg_check_(""),
   = arg_check_(""),
   = arg_check_(""),
   = arg_check_(""),
   = arg_check_(""),

some of the utilities cant use check_arg
like list_depth is recursive

make a species species argument for plot_cast_ts
(plot_species)? something thats only length 1

can start_newmoon become start_moon ??

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


