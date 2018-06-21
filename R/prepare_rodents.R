#' @title Prepare historic rodent data
#'
#' @description Prepare the rodent dataset, tailored for forecasting. 
#'   Specifically, return data for all plots and control plots only, for each 
#'   species except PI and the total
#'
#' @param moons current newmoon table
#'
#' @param options_rodents data options list for rodents, which is forced to 
#'   conform internally for the two data types (all and controls)
#' 
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
prep_rodents <- function(moons = prep_moons(), 
                         options_rodents = rodents_options()){
  if (!options_rodents$quiet){
    cat("Loading the rodents data files into the data subdirectory. \n")
  }
  verify_PortalData(options_rodents$tree, "Portal_rodent.csv")

  options_a <- enforce_rodents_options(options_rodents, "all")
  all <- rodents_data(moons, options_a)
  options_c <- enforce_rodents_options(options_rodents, "controls")
  ctls <- rodents_data(moons, options_c)

  list("all" = all, "controls" = ctls)
}

#' @title Collect and process historic rodent data
#'
#' @description Prepare a particular version of the rodent dataset, tailored 
#'   for forecasting. 
#'
#' @param moons current newmoon table
#'
#' @param options_rodents data options list for rodents
#' 
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
rodents_data <- function(moons = prep_moons(), 
                         options_rodents = rodents_options()){
  get_rodent_data(path = main_path(options_rodents$tree), 
                  clean = FALSE, type = "Rodents", 
                  level = options_rodents$level, 
                  length = options_rodents$length, 
                  min_plots = options_rodents$min_plots, 
                  output = options_rodents$output) %>%
  remove_spp(options_rodents$drop_spp) %>%
  mutate(total = rowSums(.[ , is.spcol(.)])) %>%
  trim_treatment(options_rodents) %>%
  inner_join(moons, by = c("period" = "period")) %>%
  subset(newmoonnumber >= options_rodents$start) %>%
  select(-newmoondate, -censusdate) %>%
  dataout(options_rodents)
}

#' @title Enforce the specific data options for the two rodent data types
#'
#' @description Force a list of control options for the rodent data based on
#'   the rodent data type
#'
#' @param data_options existing control options list for rodents
#'
#' @param type "all" or "controls"
#'
#' @return control options list for rodents
#'
#' @export
#'
enforce_rodents_options <- function(data_options, type = "all"){
  if (!is.null(type)){
    if (type == "all"){
      data_options$level <- "Site"
      data_options$length <- "all"
      data_options$output <- "abundance"
      data_options$filename <- "all.csv"
    }
    if (type == "controls"){
      data_options$level <- "Treatment"
      data_options$length <- "Longterm"
      data_options$output <- "abundance"
      data_options$treatment <- "control"
      data_options$filename <- "controls.csv"
    }  
  }
  return(data_options)
}

#' @title Remove species from the rodent data table
#'
#' @description Remove species from the rodent data table
#'
#' @param data rodent data table
#'
#' @param drop_spp species name (column header) to drop from data
#'
#' @return trimmed data
#'
#' @export
#'
remove_spp <- function(data, drop_spp = rodents_options()$drop_spp){
  if (!is.null(drop_spp)){
    data <- select(data, -drop_spp)
  }
  return(data)
}        
 
#' @title Rodent species abbreviations
#'
#' @description Create a simple vector of species abbreviations
#'
#' @return vector of species abbreviations
#'
#' @export
#'
rodent_spp <- function(){
  c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PI", 
    "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
}

#' @title Which columns are species columns
#'
#' @description Check column names to verify if they're species
#'
#' @param x data table of interest
#'
#' @param spp_names species name abbreviations
#'
#' @return logical vector corresponding to the columns in x
#'
#' @export
#'
is.spcol <- function(x, spp_names = rodent_spp()){
  colnames(x) %in% spp_names
}

#' @title Remove the treatment column from the rodent data table
#'
#' @description Remove the treatment column from the rodent data table if 
#'   needed (in the case of controls, for example, where the data have been
#'   reduced already so the column is vestigial)
#'
#' @param data rodent data table
#'
#' @param options_rodents rodent data options list
#'
#' @return trimmed data
#'
#' @export
#'
trim_treatment <- function(data, options_rodents = rodents_options()){
  if (options_rodents$level == "Treatment"){
    data <-  data %>%
             filter(treatment == options_rodents$treatment) %>%
             select(-treatment)
  }
  return(data)
}


