#' @title Determine the most recent data collection
#'
#' @description Determine the most recent census.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
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
most_recent_census <- function(main = "."){
  check_args()
  all <- read_rodents(main, "all")
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
#' @return \code{data.frame} data table of interpolation-inclusive counts
#'  for each species and total and with the columns trimmed to just 
#'  the species, total, and newmoonnumber (\code{newmoon}).
#'
#' @export
#' 
interpolate_abundance <- function(rodents_tab){
  check_args()
  newmoon <- (min(rodents_tab$newmoonnumber)):(max(rodents_tab$newmoonnumber))
  nmoons <- length(newmoon)

  rodent_cols <- which(colnames(rodents_tab) %in% base_species(nadot = TRUE))
  species <- colnames(rodents_tab)[rodent_cols]
  nspecies <- length(species_tab)

  abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

  for(i in 1:nmoons){
    if(length(which(rodents$newmoonnumber == newmoon[i])) > 0){
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
#'  Current options include \code{"rodents"}, \code{"covariates"},
#'  \code{"covariate_casts"}, \code{"moons"}, and \code{"metadata"}, which
#'  are available as calls to \code{read_data} with a specified 
#'  \code{data_name} or as calls to the specific \code{read_<data_name>} 
#'  functions (like \code{read_moons}).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'  
#' @param data_name \code{character} representation of the data needed.
#'  Current options include \code{"rodents"}, \code{"covariates"}, 
#'  \code{"covariate_forecasts"}, \code{"moons"}, and \code{"metadata"}.
#'
#' @param level \code{character} representation of the grouping name used to
#'  define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}.
#'  
#' @return Data requested.
#' 
#' @examples
#' \donttest{
#'  setup_dir()
#'  read_data(data_name = "rodents")
#'  read_data(data_name = "controls")
#'  read_data(data_name = "covariates")
#'  read_data(data_name = "covariate_casts")
#'  read_data(data_name = "moons")
#'  read_data(data_name = "metadata")
#'
#'  read_rodents(level = "all")
#'  read_controls()
#'  read_covariates()
#'  read_covariate_casts()
#'  read_moons()
#'  read_metadata()
#' }
#'
#' @export
#'
read_data <- function(main = ".", data_name = NULL, level = "all"){
  check_args()
  return_if_null(data_name)
  if (data_name == "rodents"){
    data <- read_rodents(main, level)
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
read_rodents <- function(main = ".", level = "all"){
  check_args()
  lpath <- paste0("data/rodents_", level, ".csv") 
  fpath <- file_paths(main, lpath)
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_covariates <- function(main = "."){
  check_args()
  fpath <- file_paths(main, "data/covariates.csv")
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_covariate_casts <- function(main = "."){
  check_args()
  fpath <- file_paths(main, "data/covariate_casts.csv")
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_moons <- function(main = "."){
  check_args()
  fpath <- file_paths(main, "data/moon_dates.csv")
  read.csv(fpath, stringsAsFactors = FALSE)
}

#' @rdname read_data
#'
#' @export
#'
read_metadata <- function(main = "."){
  check_args()
  fpath <- file_paths(main, "data/metadata.yaml")
  yaml.load_file(fpath) 
}