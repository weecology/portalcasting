#' @title Prepare historic rodent data
#'
#' @description Prepare the rodent dataset, tailored for forecasting. 
#'   Specifically, return data for all plots and control plots only, for each 
#'   species except PI and the total
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param moons current newmoon table
#'
#' @param data_options data options list for rodents, which is forced to 
#'   conform internally for the two data types (all and controls)
#' 
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
prep_rodents <- function(tree = dirtree(), moons = prep_moons(), 
                         data_options = rodents_options()){
  verify_PortalData(tree = tree, "Portal_rodent.csv")

  all_options <- enforce_rodents_options(data_options, "all")
  all <- rodents_data(tree = tree, moons = moons, data_options = all_options)
  ctl_options <- enforce_rodents_options(data_options, "controls")
  ctls <- rodents_data(tree = tree, moons = moons, data_options = ctl_options)

  list("all" = all, "controls" = ctls)
}

#' @title Collect and process historic rodent data
#'
#' @description Prepare a particular version of the rodent dataset, tailored 
#'   for forecasting. 
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param moons current newmoon table
#'
#' @param data_options data options list for rodents
#' 
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
rodents_data <- function(tree = dirtree(), moons = prep_moons(), 
                         data_options = rodents_options()){
  get_rodent_data(path = main_path(tree), clean = FALSE, type = "Rodents", 
                  level = data_options$level, 
                  length = data_options$length, 
                  min_plots = data_options$min_plots, 
                  output = data_options$output) %>%
  remove_spp(data_options$drop_spp) %>%
  mutate(total = rowSums(.[ , is.spcol(.)])) %>%
  trim_treatment(data_options = data_options) %>%
  inner_join(moons, by = c("period" = "period")) %>%
  subset(newmoonnumber >= data_options$start) %>%
  select(-newmoondate, -censusdate) %>%
  dataout(tree = tree, data_options = data_options)
}

#' @title Prepare the data options for rodent data
#'
#' @description Create a list of control options for the rodent data
#'
#' @param type "all" or "controls"
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param drop_spp species names to drop from the table
#'
#' @param min_plots minimum number of plots surveyed for a survey to be used
#'
#' @param level for get_rodent_data, automatically set by type
#'
#' @param treatment for get_rodent_data, automatically set by type
#'
#' @param length for get_rodent_data, automatically set by type
#'
#' @param output for get_rodent_data, automatically set by type
#'
#' @param save logical if the data should be saved out
#'
#' @param filename the name of the file for the saving, automatically set by
#'   type
#'
#' @return control options list for rodents
#'
#' @export
#'
rodents_options <- function(type = NULL, start = 217, drop_spp = "PI", 
                            min_plots = 24, level = "Site", 
                            treatment = NULL, length = "all",
                            output = "abundance", save = TRUE, 
                            filename = "all.csv"){
  if (!is.null(type)){
    if (type == "all"){
      level <- "Site"
      length <- "all"
      output <- "abundance"
      filename <- "all.csv"
    }
    if (type == "controls"){
      level <- "Treatment"
      length <- "Longterm"
      output <- "abundance"
      treatment <- "control"
      filename <- "controls.csv"
    }  
  }
  list("start" = start, "drop_spp" = drop_spp, "min_plots" = min_plots, 
       "level" = level, "length" = length, "output" = output, "save" = save,
       "filename" = filename, "treatment" = treatment)
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
#' @param data_options rodent data options list
#'
#' @return trimmed data
#'
#' @export
#'
trim_treatment <- function(data, data_options = rodents_options()){
  if (data_options$level == "Treatment"){
    data <-  data %>%
             filter(treatment == data_options$treatment) %>%
             select(-treatment)
  }
  return(data)
}


