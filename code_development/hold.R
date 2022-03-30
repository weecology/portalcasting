#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.


#' @param control_model \code{list} of model-level controls, including
#'  \code{name}, a \code{character} value of the model's name;
#'  \code{covariatesTF}, a \code{logical} indicator for if the model requires 
#'  covariates; and \code{lag}, a \code{integer} (or integer \code{numeric}) 
#'  lag time used for the covariates or \code{NA} if 
#'  \code{covariatesTF = FALSE}. Only used if the specific valued argument
#'  is \code{NULL}.
#'
#' @param covariatesTF \code{logical} indicator for if the model requires 
#'  covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NULL} if \code{covariatesTF} is \code{FALSE}.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  
#' @param data_sets \code{character} vector of the rodent data set names
#'  that the model is applied to. 
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'  
#' @param max_E \code{integer} (or integer \code{numeric}) for the maximum 
#'  embedding dimension to search amongst for EDM models. Not currently used.  
#   See \code{\link[rEDM]{simplex}} for more information.
#'
#' @param control_runjags \code{list} of arguments passed to 
#'  \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}.
#'
#' @param control_list \code{list} of arguments passed to 
#'  \code{list_function}.
#'
#' @param list_function \code{character} value name of the function to 
#'  send \code{control_list} arguments to within the model script.
