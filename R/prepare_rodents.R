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
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#' 
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
prep_rodents <- function(tree = dirtree(), moons = prep_moons(), start = 217){
  all_options <- rodents_options("all", start = start)
  all <- rodents_data(tree = tree, moons = moons, data_options = all_options)
  ctl_options <- rodents_options("controls", start = start)
  ctls <- rodents_data(tree = tree, moons = moons, data_options = ctl_options)
  out <- list("all" = all, "controls" = ctls)
  return(out)
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
#' @param data_options
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
  subset(newmoonnumber >= start) %>%
  select(-newmoondate, -censusdate) %>%
  dataout(tree = tree, data_options = data_options)
}





dataout <- function(data, tree = dirtree(), data_options = rodents_options()){

  if (!is.null(data_options$save)){
    if (data_options$save){
      path <- file_path(tree = tree, paste0("data/", data_options$filename))
      write.csv(data, path, row.names = FALSE)
    }
  }
  return(data)
}

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

remove_spp <- function(data, drop_spp = rodent_options()$drop_spp){
  if (!is.null(drop_spp)){
    data <- select(data, -drop_spp)
  }
  return(data)
}        
 
rodent_spp <- function(){
  c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PI", 
    "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
}

is.spcol <- function(x, spp_names = rodent_spp()){
   colnames(x) %in% spp_names
}

trim_treatment <- function(data, data_options = rodents_options()){
  if (data_options$level == "Treatment"){
    data <-  data %>%
             filter(treatment == data_options$treatment) %>%
             select(-treatment)
  }
  return(data)
}


