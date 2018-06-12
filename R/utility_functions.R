#' @title Save data out to a csv, appending if existing
#'
#' @description Appending a csv without re-writing the header. If file doesn't
#'   exist, it will be created.
#'
#' @param df data table to be written out
#'
#' @param filename filename of existing csv to be appended
#'
#' @return Nothing
#'
#' @export
#'
append_csv <- function(df, filename){
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
}

#' @title Zero-abundance forecast
#'
#' @description Create a 0-abundance forecast for fill-in usage
#'
#' @param nfcnm number of forecast newmoons
#'
#' @param pred_name name for the predictor column (to match variable model
#'   output names)
#'
#' @return list of mean and interval values for a 0-abundance filler forecast
#'
#' @export
#'
fcast0 <- function(nfcnm, pred_name = "pred"){
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list(mean_0, "interval" = int_0)
  names(out)[1] <- pred_name
  return(out)
}

#' @title Normalize path
#'
#' @description Return normalized path for all operating systems
#'
#' @param reference a path to join with current working directory
#'
#' @param base Current working directory else path given
#'
#' @return normalized path
#'
#' @examples
#' full_path("PortalData/Rodents/Portal_rodent.csv")
#'
#' full_path("PortalData/Rodents/Portal_rodent.csv", "~")
#'
#' @export
#'
full_path <- function(reference, base = getwd()){
  base <- normalizePath(base)
  path <- normalizePath(file.path(base, reference), mustWork = FALSE)
  return(path)
}

#' @title Portalcasting full path
#'
#' @description Simplification wrapper to use \code{full_path} to the 
#'   \code{portalcasting} folder within the base folder
#'
#' @param reference reference
#'
#' @param base base
#'
#' @return normalized path to portalcasting
#'
#' @export
#'
pc_path <- function(reference, base = getwd()){

  path <- full_path(paste0("portalcasting/", reference), base)
  return(path)
}
