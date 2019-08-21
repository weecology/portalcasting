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
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
  all <- read_rodents_table(main, "all")
  moons <- read_moons(main)
  matched <- moons$newmoonnumber == max(all$newmoonnumber)
  as.Date(as.character(moons$censusdate[matched]))
}


#' @title Interpolate missing rodent data
#' 
#' @description Interpolation of missing data in the rodent abundance data 
#'  set using \code{\link[forecast]{na.interp}}. Each species is individually 
#'  linearly interpolated, then the total number of rodents is calculated 
#'  from the sum of the individual species.
#'
#' @param rodents_tab \code{data.frame} of rodents data with a 
#'  \code{newmoonnumber} column. 
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
#' @return \code{data.frame} data table of interpolation-inclusive counts
#'  for each species and total and with the columns trimmed to just 
#'  the species, total, and newmoonnumber (\code{newmoon}).
#'
#' @examples
#'  \donttest{ 
#'   setup_dir()
#'   abundances <- read_rodents_table()
#'   metadata <- read_metadata(main)
#'   nmoons <- length(metadata$rodent_cast_newmoons)
#'   CL <- metadata$confidence_level
#'   abundances <- interpolate_abundance(abundances)
#'  }
#' 
#' @export
#' 
interpolate_abundance <- function(rodents_tab, arg_checks = TRUE){
  check_args(arg_checks)
  newmoon <- (min(rodents_tab$newmoonnumber)):(max(rodents_tab$newmoonnumber))
  nmoons <- length(newmoon)

  rodent_cols <- which(colnames(rodents_tab) %in% base_species(nadot = TRUE))
  species <- colnames(rodents_tab)[rodent_cols]
  nspecies <- length(species)

  abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

  for(i in 1:nmoons){
    if(length(which(rodents_tab$newmoonnumber == newmoon[i])) > 0){
      temp <- rodents_tab[which(rodents_tab$newmoonnumber == newmoon[i]),
                        which(colnames(rodents_tab) %in% species)]
      abunds[i, ] <- as.numeric(temp)
    }
  }

  interpolated_abunds <- abunds
  colnames(interpolated_abunds) <- species

  for(j in 1:nspecies){
    interpolated_abunds[ , j] <- round(na.interp(abunds[ , j]))
  }

  interpolated_total <- apply(interpolated_abunds, 1, sum)

  data.frame(newmoon, interpolated_abunds, total = interpolated_total)
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
#' @param tmnt_type,tmnt_types \code{character} representation of the grouping
#'  name(s) used to define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}. \code{tmnt_type} can only be length 1, 
#'  \code{tmnt_types} is not restricted in length.
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
#'  read_data(data_name = "controls")
#'  read_data(data_name = "covariates")
#'  read_data(data_name = "covariate_casts")
#'  read_data(data_name = "moons")
#'  read_data(data_name = "metadata")
#'
#'  read_rodents()
#'  read_rodents_table()
#'  read_controls()
#'  read_covariates()
#'  read_covariate_casts()
#'  read_moons()
#'  read_metadata()
#' }
#'
#' @export
#'
read_data <- function(main = ".", data_name = NULL, tmnt_type = "all", 
                      tmnt_types = c("all", "controls"), arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(data_name)
  if (data_name == "rodents"){
    data <- read_rodents(main, tmnt_types)
  }
  if (data_name == "rodents_table"){
    data <- read_rodents_table(main, tmnt_type)
  }
  if (data_name == "covariates"){
    data <- read_covariates(main)
  }
  if (data_name == "covariate_casts"){
    data <- read_covariate_casts(main)
  }
  if (data_name == "moons"){
    data <- read_moons(main)
  }
  if (data_name == "metadata"){
    data <- read_metadata(main)
  }
  data
}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function(main = ".", tmnt_type = "all", 
                               arg_checks = TRUE){
  check_args(arg_checks)
  lpath <- paste0("data/rodents_", tmnt_type, ".csv") 
  fpath <- file_paths(main, lpath)
  if(!file.exists(fpath)){
    rodents_tab <- prep_rodents(main, tmnt_types = tmnt_type)[[1]]
    return(rodents_tab)
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function(main = ".", tmnt_types = c("all", "controls"), 
                         arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(tmnt_types)
  ntmnt_types <- length(tmnt_types)
  rodents_list <- vector("list", length = ntmnt_types)
  for(i in 1:ntmnt_types){
    rodents_list[[i]] <- read_rodents_table(main, tmnt_types[i])
  }
  names(rodents_list) <- tmnt_types
  rodents_list
}


#' @rdname read_data
#'
#' @export
#'
read_covariates <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  fpath <- file_paths(main, "data/covariates.csv")
  if(!file.exists(fpath)){
    return(prep_covariates(main))
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
    return(cast_covariates(main))
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
    return(prep_moons(main))
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
    md <- prep_metadata(main)
    return(md)
  }
  yaml.load_file(fpath) 
}