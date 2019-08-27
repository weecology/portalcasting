#' @title Determine the most recent data collection
#'
#' @description Determine the most recent census.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{Date} of the most recent census.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   most_recent_census()
#'  }
#'
#' @export
#'
most_recent_census <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  moons <- read_moons(main)
  as.Date(max(moons$censusdate, na.rm = TRUE))
}



#' @title Read in a data file and format it for specific class
#'
#' @description Read in a specified data file. \cr \cr
#'  Current options include \code{"rodents"} (produces a list), 
#'  \code{"rodents_table"} (produces a rodents table), \code{"covariates"},
#'  \code{"covariate_casts"}, \code{"moons"}, and \code{"metadata"}, which
#'  are available as calls to \code{read_data} with a specified 
#'  \code{data_name} or as calls to the specific \code{read_<data_name>} 
#'  functions (like \code{read_moons}). \cr \cr
#'  If the requested data do not exist, an effort is made to prepare them
#'  using the associated \code{prep_<data_name>} functions (like
#'  \code{prep_moons}).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'  
#' @param data_name \code{character} representation of the data needed.
#'  Current options include \code{"rodents"}, \code{"rodents_table"}, 
#'  \code{"covariates"}, \code{"covariate_forecasts"}, \code{"moons"}, and 
#'  \code{"metadata"}.
#'
#' @param data_set,data_sets \code{character} representation of the grouping
#'  name(s) used to define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}. \code{data_set} can only be length 1, 
#'  \code{data_sets} is not restricted in length.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'  
#' @return Data requested.
#' 
#' @examples
#' \donttest{
#'  setup_dir()
#'  read_data(data_name = "rodents")
#'  read_data(data_name = "rodents_table")
#'  read_data(data_name = "rodents_table", data_set = "controls")
#'  read_data(data_name = "covariates")
#'  read_data(data_name = "covariate_casts")
#'  read_data(data_name = "moons")
#'  read_data(data_name = "metadata")
#'  read_data(data_name = "cast_metadata")
#'
#'  read_rodents()
#'  read_rodents_table()
#'  read_covariates()
#'  read_covariate_casts()
#'  read_moons()
#'  read_metadata()
#'  read_cast_metadata()
#' }
#'
#' @export
#'
read_data <- function(main = ".", data_name = NULL, data_set = "all", 
                      data_sets = c("all", "controls"), arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(data_name)
  data_name <- tolower(data_name)
  data_set <- tolower(data_set)
  data_sets <- tolower(data_sets)
  if (data_name == "rodents"){
    data <- read_rodents(main, data_sets, arg_checks)
  }
  if (data_name == "rodents_table"){
    data <- read_rodents_table(main, data_set, arg_checks)
  }
  if (data_name == "covariates"){
    data <- read_covariates(main, arg_checks)
  }
  if (data_name == "covariate_casts"){
    data <- read_covariate_casts(main, arg_checks)
  }
  if (data_name == "moons"){
    data <- read_moons(main, arg_checks)
  }
  if (data_name == "metadata"){
    data <- read_metadata(main, arg_checks)
  }
  if (data_name == "cast_metadata"){
    data <- read_cast_metadata(main, arg_checks)
  }
  data
}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function(main = ".", data_set = "all", 
                               arg_checks = TRUE){
  check_args(arg_checks)
  data_set <- tolower(data_set)
  lpath <- paste0("data/rodents_", data_set, ".csv") 
  fpath <- file_paths(main, lpath)
  if(!file.exists(fpath)){
    rodents <- prep_rodents(main = main, data_sets = data_set, 
                            arg_checks = arg_checks)
    rodents_tab <- rodents[[1]]
    return(rodents_tab)
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function(main = ".", data_sets = c("all", "controls"), 
                         arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(data_sets)
  data_sets <- tolower(data_sets)
  ndata_sets <- length(data_sets)
  rodents <- vector("list", length = ndata_sets)
  for(i in 1:ndata_sets){
    rodents[[i]] <- read_rodents_table(main = main, data_set = data_sets[i], 
                                       arg_checks = arg_checks)
  }
  names(rodents) <- data_sets
  rodents
}


#' @rdname read_data
#'
#' @export
#'
read_covariates <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  fpath <- file_paths(main, "data/covariates.csv")
  if(!file.exists(fpath)){
    return(prep_covariates(main = main, arg_checks = arg_checks))
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_covariate_casts <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  fpath <- file_paths(main, "data/covariate_casts.csv")
  if(!file.exists(fpath)){
    return(cast_covariates(main = main, arg_checks = arg_checks))
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_moons <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  fpath <- file_paths(main, "data/moon_dates.csv")
  if(!file.exists(fpath)){
    return(prep_moons(main = main, arg_checks = arg_checks))
  }
  read.csv(fpath, stringsAsFactors = FALSE)
}

#' @rdname read_data
#'
#' @export
#'
read_metadata <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  fpath <- file_paths(main, "data/metadata.yaml")
  if(!file.exists(fpath)){
    md <- prep_metadata(main = main, arg_checks = arg_checks)
    return(md)
  }
  yaml.load_file(fpath) 
}

#' @rdname read_data
#'
#' @export
#'
read_cast_metadata <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  meta_path <- file_paths(main, "casts/cast_metadata.csv")
  if(!file.exists(meta_path)){
    return(prep_cast_metadata(main = main, arg_checks = arg_checks))
  }
  read.csv(meta_path, stringsAsFactors = FALSE)
}
  

 
