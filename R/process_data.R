#' @title Interpolate missing rodent data
#' 
#' @description Interpolation of missing data in the rodent abundance data 
#'   set using \code{\link[forecast]{na.interp}}. Each species is individually 
#'   linearly interpolated, then the total number of rodents is calculated 
#'   from the sum of the individual species.
#'
#' @param abundance Class \code{rodents} \code{data.frame} rodents data table 
#'   with a \code{newmoon} column. 
#'
#' @return \code{data.frame} data table of interpolation-inclusive counts
#'   for each species and total and with the columns trimmed to just 
#'   the species, total, and newmoonnumber (\code{moons}).
#'
#' @export
#' 
interpolate_abundance <- function(abundance){
  if (!("rodents" %in% class(abundance))){
    stop("`abundance` is not of class rodents")
  }
  moons <- (min(abundance$newmoonnumber)):(max(abundance$newmoonnumber))
  nmoons <- length(moons)

  species <- colnames(abundance)[2:(ncol(abundance) - 4)]
  nspecies <- length(species)

  abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

  for(i in 1:nmoons){
    if(length(which(abundance$newmoonnumber == moons[i])) > 0){
      temp <- abundance[which(abundance$newmoonnumber == moons[i]),
                        which(colnames(abundance) %in% species)]
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
#' @description Lag the weather data together based on the new moons
#'
#' @param data Class \code{covariates} \code{data.frame} of covariate data 
#'   to be lagged. 
#'  
#' @param lag \code{integer} lag between rodent census and covariate data, in
#'   new moons.
#'  
#' @param tail \code{logical} indicator if the data lagged to the tail end 
#'   should be retained.
#'  
#' @return \code{data} \code{data.frame} \code{newmoonnumber} columnn now 
#'   reflecting the lag.
#'
#' @export
#'
lag_data <- function(data, lag, tail = FALSE){

  if (!("covariates" %in% class(data))){
    stop("`data` is not of class covariates")
  }
  if (!("logical" %in% class(tail))){
    stop("`tail` is not of class logical")
  }
  if (length(lag) > 1){
    stop("`lag` can only be of length = 1")
  }
  if (!("numeric" %in% class(lag)) & !("integer" %in% class(lag))){
    stop("`lag` is not of class numeric or integer")
  }
  if(lag < 0 | lag %% 1 != 0){
    stop("`lag` is not a non-negative integer")
  }
  data$newmoonnumber_lag <- data$newmoonnumber + lag
  
  if(tail == FALSE){
    oldest_included_newmoon <- data$newmoonnumber[1]
    most_recent_newmoon <- data$newmoonnumber[nrow(data)]
    hist_newmoons <- oldest_included_newmoon:most_recent_newmoon
    hist_moons_table <- data.frame(newmoonnumber = hist_newmoons)
    nm_match <- c("newmoonnumber_lag" = "newmoonnumber")
    data <- right_join(data, hist_moons_table, by = nm_match) 
    if (lag > 0){
      data <- data[-(1:lag), ]
    }
  }
  data <- select(data, -newmoonnumber)
  cn_data <- colnames(data)
  cn_nmn_l <- which(cn_data == "newmoonnumber_lag")
  colnames(data)[cn_nmn_l] <- "newmoonnumber"
  data
}

#' @title Read in a data file and format it for specific class
#'
#' @description Read in a specified data file and ensure its class attribute
#'   is appropriate for usage within the portalcasting pipeline. Current 
#'   options include \code{"all"}, \code{"controls"}, \code{"covariates"},
#'   and \code{"metadata"}.
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'  
#' @param data_name \code{character} representation of the data needed.
#'   Current options include \code{"all"}, \code{"controls"},
#'   \code{"covariates"}, and \code{"metadata"}.
#'  
#' @return Data requested with appropriate classes.
#'
#' @export
#'
read_data <- function(tree, data_name){
  valid_names <- c("all", "controls", "covariates", "metadata")
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (length(data_name) > 1){
    stop("`data_name` can only be of length = 1")
  }
  if (!is.character(data_name)){
    stop("`data_name` is not a character")
  }
  if (!any(valid_names %in% data_name)){
    stop("`data_name` is not valid option")
  } 
  if (data_name == "all"){
    data <- read.csv(file_path(tree, "data/all.csv")) %>%
            classy(c("data.frame", "rodents"))
  }
  if (data_name == "controls"){
    data <- read.csv(file_path(tree, "data/all.csv")) %>%
            classy(c("data.frame", "rodents"))
  }
  if (data_name == "covariates"){
    data <- read.csv(file_path(tree, "data/covariates.csv")) %>%
            classy(c("data.frame", "covariates"))
  }
  if (data_name == "metadata"){
    data <- yaml.load_file(file_path(tree, "data/metadata.yaml")) %>%
            classy(c("list", "metadata"))
  }
  data
}