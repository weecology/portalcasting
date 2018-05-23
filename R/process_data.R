#' @title Interpolate Missing Rodent Data
#' 
#' @description Interpolation of missing data in the rodent abundance data 
#'   set. Each species is individually linearly interpolated, then the total 
#'   number of rodents is calculated from the sum of the individual species.
#'
#' @param abundance data table with new moon column
#' @return data table of interpolation-inclusive counts for each species and 
#'  total
#'
#' @export
#' 
interpolate_abundance <- function(abundance){

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

  out <- data.frame(moons, interpolated_abunds, total = interpolated_total)

  return(out)
}

#' @title Lag Covariate Data
#'
#' @description Lag the weather data together based on the new moons
#'
#' @param data dataframe of weather or ndvi data to be lagged
#' @param lag lag between census and weather data, in new moons
#' @param tail logical, if the data lagged to the tail end should be retained
#' @return a dataframe of weather data with newmoonnumber now reflecting the
#'  lag
#'
#' @export
#'
lag_data <- function(data, lag, tail = FALSE){
  newmoonnumber <- NULL
  data$newmoonnumber_lag <- data$newmoonnumber + lag
  
  if(tail == FALSE){
    oldest_included_newmoon <- data$newmoonnumber[1]
    most_recent_newmoon <- data$newmoonnumber[nrow(data)]
    hist_newmoons <- oldest_included_newmoon:most_recent_newmoon
    hist_moons_table <- data.frame(newmoonnumber = hist_newmoons)
    nm_match <- c("newmoonnumber_lag" = "newmoonnumber")
    data <- right_join(data, hist_moons_table, by = nm_match) 
    data <- data[-(1:lag), ]
  }
  data <- select(data, -newmoonnumber)
  cn_data <- colnames(data)
  cn_nmn_l <- which(cn_data == "newmoonnumber_lag")
  colnames(data)[cn_nmn_l] <- "newmoonnumber"

  return(data)
}