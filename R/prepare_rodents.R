#' @title Prepare historic rodent data
#'
#' @description Prepare the rodent dataset, tailored for forecasting. 
#'   Specifically, return data for all plots and control plots only, for each 
#'   species except PI and the total
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param options_rodents Class-\code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation. See 
#'   \code{\link{rodents_options}}.
#'
#' @return A class-\code{rodents_list} \code{list} of two class-
#'   \code{rodents} \code{data.frame}s, \code{all} (abundances on all plots)
#'   and \code{controls} (abundances on control plots only).
#'
#' @export
#'
prep_rodents <- function(moons = prep_moons(), 
                         options_rodents = rodents_options()){
  if (!("moons" %in% class(moons))){
    stop("`moons` is not of class moons")
  }
  if (!("rodents_options") %in% class(options_rodents)){
    stop("`options_rodents` is not a rodents_options list")
  }
  if (!options_rodents$quiet){
    message("Loading rodents data files into data subdirectory")
  }
  verify_PortalData(options_rodents$tree, "Portal_rodent.csv")

  options_a <- enforce_rodents_options(options_rodents, "all")
  all <- rodents_data(moons, options_a)
  options_c <- enforce_rodents_options(options_rodents, "controls")
  ctls <- rodents_data(moons, options_c)

  classy(list("all" = all, "controls" = ctls), c("rodents_list", "list"))
}

#' @title Collect and process historic rodent data for -casting
#'
#' @description Prepare a particular version of the rodent dataset, tailored 
#'   for forecasting or hindcasting. Available versions of the data currently
#'   include \code{all} (abundances on all plots) and \code{controls} 
#'   (abundances on control plots only)
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param options_rodents Class-\code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation. See 
#'   \code{\link{rodents_options}} for general creation and 
#'   \code{\link{enforce_rodents_options}} for bulk setting of options for
#'   each of the possible data table outputs. 
#' 
#' @return One of two class-\code{rodents} \code{data.frame}s, 
#'   \code{all} (abundances on all plots) or \code{controls} (abundances on
#'   control plots only).
#'
#' @export
#'
rodents_data <- function(moons = prep_moons(), 
                         options_rodents = rodents_options()){
  if (!("moons" %in% class(moons))){
    stop("`moons` is not of class moons")
  }
  if (!("rodents_options") %in% class(options_rodents)){
    stop("`options_rodents` is not a rodents_options list")
  }
  end_step <- options_rodents$end[options_rodents$hind_step]
  if (options_rodents$quiet){
    suppressMessages(
      summarize_rodent_data(path = main_path(options_rodents$tree), 
                            clean = FALSE, type = "Rodents", 
                            level = options_rodents$level, 
                            plots = options_rodents$plots, 
                            min_plots = options_rodents$min_plots, 
                            output = options_rodents$output) %>%
      classy(c("data.frame", "rodents")) %>% 
      remove_spp(options_rodents$drop_spp) %>%
      mutate(total = rowSums(.[ , is.spcol(.)])) %>%
      classy(c("data.frame", "rodents")) %>% 
      trim_treatment(options_rodents) %>%
      inner_join(moons, by = c("period" = "period")) %>%
      subset(newmoonnumber >= options_rodents$start) %>%
      subset(newmoonnumber <= min(c(end_step, max(newmoonnumber)))) %>%
      select(-newmoondate, -censusdate) %>%
      dataout(options_rodents)
    )
  } else{
    summarize_rodent_data(path = main_path(options_rodents$tree), 
                          clean = FALSE, type = "Rodents", 
                          level = options_rodents$level, 
                          plots = options_rodents$plots, 
                          min_plots = options_rodents$min_plots, 
                          output = options_rodents$output) %>%
    classy(c("data.frame", "rodents")) %>% 
    remove_spp(options_rodents$drop_spp) %>%
    mutate(total = rowSums(.[ , is.spcol(.)])) %>%
    classy(c("data.frame", "rodents")) %>% 
    trim_treatment(options_rodents) %>%
    inner_join(moons, by = c("period" = "period")) %>%
    subset(newmoonnumber >= options_rodents$start) %>%
    subset(newmoonnumber <= min(c(end_step, max(newmoonnumber)))) %>%
    select(-newmoondate, -censusdate) %>%
    dataout(options_rodents)
  }
}

#' @title Enforce the specific options for a given rodent data type
#'
#' @description Enforce a list of control options for the rodent data 
#'   (\code{rodents_options}, see \code{\link{rodents_options}}) based on
#'   the requested data type. Specifically, the \code{tmnt_type}, 
#'   \code{level}, \code{length}, \code{plots}, \code{output}, 
#'   \code{treatment}, and \code{filename} elements are set by the selection.
#'
#' @details If \code{tmnt_type = "all"}, the elements are \code{tmnt_type} as
#'   \code{"all"}, \code{level} as \code{"Site"}, \code{length} as 
#'   \code{"all"}, \code{plots} as \code{"all"}, \code{output} as 
#'   \code{"abundance"}, \code{treatment} as \code{NULL}, and \code{filename} 
#'   as \code{"all.csv"}. \cr \cr
#'   If \code{tmnt_type = "controls"}, the elements are \code{tmnt_type} as
#'   \code{"controls"}, \code{level} as \code{"Treatment"}, \code{length} as 
#'   \code{"Longterm"}, \code{plots} as \code{"Longterm"}, \code{output} as 
#'   \code{"abundance"}, \code{treatment} as \code{controls}, and 
#'   \code{filename} as \code{"controls.csv"}, 
#'
#' @param options_rodents Class-\code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation. See 
#'   \code{\link{rodents_options}}.
#'
#' @param tmnt_type \code{character}-value representation of treatment type 
#'   (\code{"all"} or \code{"controls"}) used to enforce certain elements of 
#'   the \code{options_rodents} \code{list}. Alternatively, use \code{NULL}
#'   to not change the options at all.
#'
#' @return Updated \code{options_rodents} input, still a class-
#'   \code{rodents_options} \code{list}.
#'
#' @export
#'
enforce_rodents_options <- function(options_rodents = rodents_options(),
                                    tmnt_type = NULL){
  if (!("rodents_options") %in% class(options_rodents)){
    stop("`options_rodents` is not a rodents_options list")
  }
  if (!is.null(tmnt_type) & !("character") %in% class(tmnt_type)){
    stop("`tmnt_type` is not NULL or a character")
  }
  if (!is.null(tmnt_type)){
    if (!(tmnt_type %in% c("all", "controls"))){
      stop("`tmnt_type` must be 'all' or 'controls'")
    }
    if (tmnt_type == "all"){
      options_rodents$tmnt_type <- "all"
      options_rodents$level <- "Site"
      options_rodents$length <- "all"
      options_rodents$plots <- "all"
      options_rodents$output <- "abundance"
      options_rodents$treatment <- NULL
      options_rodents$filename <- "all.csv"
    }
    if (tmnt_type == "controls"){
      options_rodents$tmnt_type <- "controls"
      options_rodents$level <- "Treatment"
      options_rodents$length <- "Longterm"
      options_rodents$plots <- "Longterm"
      options_rodents$output <- "abundance"
      options_rodents$treatment <- "control"
      options_rodents$filename <- "controls.csv"
    }  
  }
  options_rodents
}

#' @title Remove species from the rodent data table
#'
#' @description Remove species from the rodent data table
#'
#' @param data Class-\code{rodents} \code{data.table} of rodent data.
#'
#' @param drop_spp \code{character} vector of species name(s) (column header)
#'   to drop from \code{data} or \code{NULL} to not drop any species.
#'
#' @return Trimmed \code{data}.
#'
#' @export
#'
remove_spp <- function(data, drop_spp = rodents_options()$drop_spp){
  if (!("rodents" %in% class(data))){
    stop("`data` is not of class rodents")
  }
  if (!is.null(drop_spp) & !("character") %in% class(drop_spp)){
    stop("`drop_spp` is not NULL or a character")
  }  
  if (!is.null(drop_spp)){
    data <- select(data, -one_of(drop_spp))
  }
  data
}        
 
#' @title Rodent species abbreviations
#'
#' @description Creates a simple \code{character} vector of abbreviations for
#'   the \href{https://portal.naturecast.org/profiles.html}{Portal Rodents}.
#'
#' @param nadot \code{logical} indicator if the dot should be added to the 
#'   \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @return \code{character} vector of species abbreviations.
#'
#' @export
#'
rodent_spp <- function(nadot = FALSE){
  xNAx <- "NA"
  if (nadot){
    xNAx <- "NA."
  }
  c("BA", "DM", "DO", "DS", xNAx, "OL", "OT", "PB", "PE", "PF", "PH", "PI", 
    "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
}

#' @title Which columns are species columns
#'
#' @description Check column names to verify if they are species names.
#'
#' @param x data table of interest
#'
#' @param spp_names \code{character} vector of species name abbreviations.
#'
#' @return \code{logical} vector corresponding to the columns in \code{x}.
#'
#' @export
#'
is.spcol <- function(x, spp_names = rodent_spp()){
  if (!("data.frame" %in% class(x))){
    stop("`x` is not of class data.frame")
  }
  if (!("character" %in% class(spp_names))){
    stop("`spp_names` is not of class character")
  }
  colnames(x) %in% spp_names
}

#' @title Remove the treatment column from the rodent data table
#'
#' @description Remove the treatment column from the rodent data table if 
#'   needed (in the case of controls, for example, where the data have been
#'   reduced already so the column is vestigial).
#'
#' @param data Class-\code{rodents} \code{data.table} of rodent data.
#'
#' @param options_rodents Class-\code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation. See 
#'   \code{\link{rodents_options}}
#'
#' @return Trimmed \code{data}.
#'
#' @export
#'
trim_treatment <- function(data, options_rodents = rodents_options()){
  if (!("rodents" %in% class(data))){
    stop("`data` is not of class rodents")
  }
  if (!("rodents_options") %in% class(options_rodents)){
    stop("`options_rodents` is not a rodents_options list")
  }
  if (options_rodents$level == "Treatment"){
    data <-  data %>%
             filter(treatment == options_rodents$treatment) %>%
             select(-treatment)
  }
  classy(data, c("data.frame", "rodents"))
}

#' @title Transfer the trapping table to the data folder
#'
#' @description Move the trapping table from the PortalData sub directory to 
#'   the data subdirectory for use.
#'
#' @param options_data Class-\code{data_options} list containing available
#'   for controlling the set up and population of the data folder in the 
#'   portalcasting directory (see \code{\link{data_options}}). 
#'
#' @export
#'
transfer_trapping_table <- function(options_data = data_options()){
  if (!("data_options" %in% class(options_data))){
    stop("`options_data` is not a data_options list")
  }
  tree <- options_data$tree
  from <- file_path(tree, "PortalData/Rodents/Portal_rodent_trapping.csv")
  to <- file_path(tree, "data/trapping.csv")
  if (file.exists(from)){
    if (!options_data$quiet){
      message("Loading rodent trapping table into data subdirectory")
    }
    ttable <- read.csv(from, stringsAsFactors = FALSE)
    write.csv(ttable, to, row.names = FALSE)
  }
}
