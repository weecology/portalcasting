#' @title Interpolate missing rodent data
#' 
#' @description Interpolation of missing data in the rodent abundance data 
#'   set using \code{\link[forecast]{na.interp}}. Each species is individually 
#'   linearly interpolated, then the total number of rodents is calculated 
#'   from the sum of the individual species.
#'
#' @param rodents Class \code{rodents} \code{data.frame} rodents data table 
#'   with a \code{newmoon} column. 
#'
#' @return \code{data.frame} data table of interpolation-inclusive counts
#'   for each species and total and with the columns trimmed to just 
#'   the species, total, and newmoonnumber (\code{moons}).
#'
#' @export
#' 
interpolate_abundance <- function(rodents){
  check_args(rodents = rodents)
  moons <- (min(rodents$newmoonnumber)):(max(rodents$newmoonnumber))
  nmoons <- length(moons)

  rodent_cols <- which(colnames(rodents) %in% rodent_spp(nadot = TRUE))
  species <- colnames(rodents)[rodent_cols]
  nspecies <- length(species)

  abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

  for(i in 1:nmoons){
    if(length(which(rodents$newmoonnumber == moons[i])) > 0){
      temp <- rodents[which(rodents$newmoonnumber == moons[i]),
                        which(colnames(rodents) %in% species)]
      abunds[i, ] <- as.numeric(temp)
    }
  }

  interpolated_abunds <- abunds
  colnames(interpolated_abunds) <- species

  for(j in 1:nspecies){
    interpolated_abunds[ , j] <- round(na.interp(abunds[ , j]))
  }

  interpolated_total <- apply(interpolated_abunds, 1, sum)

  data.frame(moons, interpolated_abunds, total = interpolated_total)
}

#' @title Lag covariate data
#'
#' @description Lag the covariate data together based on the new moons
#'
#' @param covariates Class \code{covariates} \code{data.frame} of covariate
#'   data to be lagged. 
#'  
#' @param lag \code{integer} lag between rodent census and covariate data, in
#'   new moons.
#'  
#' @param tail \code{logical} indicator if the data lagged to the tail end 
#'   should be retained.
#'  
#' @return \code{covariates} \code{data.frame} with a \code{newmoonnumber} 
#'   columnn reflecting the lag.
#'
#' @export
#'
lag_covariates <- function(covariates, lag, tail = FALSE){
  check_args(covariates = covariates, lag = lag, tail = tail)
  covariates$newmoonnumber_lag <- covariates$newmoonnumber + lag
  
  if(tail == FALSE){
    oldest_included_newmoon <- covariates$newmoonnumber[1]
    most_recent_newmoon <- covariates$newmoonnumber[nrow(covariates)]
    hist_newmoons <- oldest_included_newmoon:most_recent_newmoon
    hist_moons_table <- data.frame(newmoonnumber = hist_newmoons)
    nm_match <- c("newmoonnumber_lag" = "newmoonnumber")
    data <- right_join(covariates, hist_moons_table, by = nm_match) 
    if (lag > 0){
      covariates <- covariates[-(1:lag), ]
    }
  }
  covariates <- select(covariates, -newmoonnumber)
  cn_covariates <- colnames(covariates)
  cn_nmn_l <- which(cn_covariates == "newmoonnumber_lag")
  colnames(covariates)[cn_nmn_l] <- "newmoonnumber"
  covariates
}

#' @title Read in a data file and format it for specific class
#'
#' @description Read in a specified data file and ensure its class attribute
#'   is appropriate for usage within the portalcasting pipeline. Current 
#'   options include \code{"all"}, \code{"controls"}, \code{"covariates"},
#'   \code{"moons"}, and \code{"metadata"}. And are available as calls to
#'   \code{read_data} with a specified \code{data_name} or as calls to the
#'   specific \code{read_<data_name>} functions (like \code{read_moons}).
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'  
#' @param data_name \code{character} representation of the data needed.
#'   Current options include \code{"all"}, \code{"controls"},
#'   \code{"covariates"}, \code{"moons"}, and \code{"metadata"}.
#'  
#' @return Data requested with appropriate classes.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' read_data(data_name = "all")
#' read_data(data_name = "controls")
#' read_data(data_name = "covariates")
#' read_data(data_name = "moons")
#' read_data(data_name = "metadata")
#'
#' read_all()
#' read_controls()
#' read_covariates()
#' read_moons()
#' read_metadata()
#' }
#'
#' @export
#'
read_data <- function(tree = dirtree(), data_name){
  check_args(tree = tree, data_name = data_name)
  if (data_name == "all"){
    data <- read.csv(file_path(tree, "data/all.csv")) %>%
            classy(c("data.frame", "rodents"))
  }
  if (data_name == "controls"){
    data <- read.csv(file_path(tree, "data/controls.csv")) %>%
            classy(c("data.frame", "rodents"))
  }
  if (data_name == "covariates"){
    data <- read.csv(file_path(tree, "data/covariates.csv")) %>%
            classy(c("data.frame", "covariates"))
  }
  if (data_name == "moons"){
    data <- read.csv(file_path(tree, "data/moons.csv")) %>%
            classy(c("data.frame", "moons"))
  }
  if (data_name == "metadata"){
    data <- yaml.load_file(file_path(tree, "data/metadata.yaml")) %>%
            classy(c("list", "metadata"))
  }
  data
}


#' @title Determine the most recent data collection
#'
#' @description Determine the most recent census.
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'
#' @return \code{Date} of the most recent census.
#'
#' @export
#'
most_recent_census <- function(tree = dirtree()){
  check_args(tree = tree)
  all <- read_data(tree, "all")
  moons <- read_data(tree, "moons")
  matched <- moons$newmoonnumber == max(all$newmoonnumber)
  as.Date(as.character(moons$censusdate[matched]))
}
