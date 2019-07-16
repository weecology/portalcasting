#' @title Prepare historic rodent data
#'
#' @description Prepare the rodent dataset, tailored for forecasting. 
#'   Specifically, \code{prep_rodents_list} returns data for all plots and 
#'   control plots only, for each species except PI and the total.
#'   \code{prep_rodents} prepares a particular version of the rodent dataset,
#'   tailored for forecasting or hindcasting. Available versions of the data 
#'   currently include \code{"all"} (abundances on all plots) and 
#'   \code{"controls"} (abundances on control plots only).
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param control A \code{list} of arguments to 
#'   \code{\link[portalr]{summarize_rodent_data}}. Preppared via 
#'   \code{\link{enforce_rodents_control}}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#
#' @param tmnt_type \code{character}-value representation of treatment type 
#'   (\code{"all"} or \code{"controls"}) used to enforce certain elements of 
#'   the \code{control} \code{list} in \code{prep_rodents}. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @return \code{prep_rodents_list}: 
#'   A class-\code{rodents_list} \code{list} of two class-
#'   \code{rodents} \code{data.frame}s, \code{all} (abundances on all plots)
#'   and \code{controls} (abundances on control plots only) (only returned if
#'   \code{update = TRUE}). \cr \cr
#'   \code{prep_rodents} 
#'   One of two class-\code{rodents} \code{data.frame}s, as defined by
#'   \code{tmnt_type}: \code{"all"} (abundances on all plots) or 
#'   \code{"controls"} (abundances on control plots only). 
#'
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' prep_rodents()
#' }
#'
#' @export
#'
prep_rodents_list <- function(moons = prep_moons(), tree = dirtree(), 
                              quiet = FALSE, control = list()){

  messageq("Loading rodents data files into data subdirectory", quiet)
  verify_PortalData(tree, quiet = quiet)
  all <- prep_rodents(moons, tree, control, "all")
  ctls <- prep_rodents(moons, tree, control, "controls")
  classy(list("all" = all, "controls" = ctls), c("rodents_list", "list"))
}

#' @rdname prep_rodents_list
#'
#' @export
#'
prep_rodents <- function(moons = prep_moons(), tree = dirtree(), 
                        control = list(), tmnt_type = NULL){
  control <- do.call("rodents_control", control)
  control <- enforce_rodents_control(control, tmnt_type)
  end_step <- control$end[control$hind_step]
  summarize_rodent_data(path = main_path(tree), 
                        clean = FALSE, type = "Rodents", 
                        level = control$level, 
                        plots = control$plots, 
                        min_traps = control$min_traps,
                        min_plots = control$min_plots, 
                        output = control$output) %>%
  classy(c("data.frame", "rodents")) %>% 
  remove_spp(control$drop_spp) %>%
  mutate(total = rowSums(.[ , is.spcol(.)])) %>%
  classy(c("data.frame", "rodents")) %>% 
  trim_treatment(control$level, control$treatment) %>%
  inner_join(moons, by = c("period" = "period")) %>%
  subset(newmoonnumber >= control$start) %>%
  subset(newmoonnumber <= min(c(end_step, max(newmoonnumber)))) %>%
  select(-newmoondate, -censusdate) %>%
  dataout(tree, control)
}

#' @title Enforce the specific options for a given rodent data type
#'
#' @description Enforce a list of control options for the rodent data 
#'   (\code{rodents_control}, see \code{\link{rodents_control}}) based on
#'   the requested data type. Specifically, the \code{tmnt_type}, 
#'   \code{level}, \code{length}, \code{plots}, \code{output}, 
#'   \code{treatment}, and \code{filename} elements are set by the selection.
#'
#' @details If \code{tmnt_type = "all"}, the elements are \code{tmnt_type} as
#'   \code{"all"}, \code{level} as \code{"Site"}, \code{length} as 
#'   \code{"all"}, \code{plots} as \code{"all"}, \code{output} as 
#'   \code{"abundance"}, \code{treatment} as \code{NULL}, 
#'   and \code{filename} as \code{"all.csv"}. \cr \cr
#'   If \code{tmnt_type = "controls"}, the elements are \code{tmnt_type} as
#'   \code{"controls"}, \code{level} as \code{"Treatment"}, \code{length} as 
#'   \code{"Longterm"}, \code{plots} as \code{"Longterm"}, \code{output} as 
#'   \code{"abundance"}, \code{treatment} as \code{controls}, 
#'   and \code{filename} as \code{"controls.csv"}. \cr \cr
#'
#' @param control \code{list} of settings controlling the rodents data 
#'   creation. See \code{\link{rodents_control}}.
#'
#' @param tmnt_type \code{character}-value representation of treatment type 
#'   (\code{"all"} or \code{"controls"}) used to enforce certain elements of 
#'   the \code{control} \code{list}. Alternatively, use \code{NULL}
#'   to not overwrite any options.
#'
#' @return An updated \code{list} of control options for rodents.
#'
#' @export
#'
enforce_rodents_control <- function(control = rodents_control(),
                                    tmnt_type = NULL){
  if (!is.null(tmnt_type)){
    if (tmnt_type == "all"){
      control$tmnt_type <- "all"
      control$level <- "Site"
      control$length <- "all"
      control$plots <- "all"
      control$output <- "abundance"
      control$treatment <- NULL
      control$r_filename <- "all.csv"
    }
    if (tmnt_type == "controls"){
      control$tmnt_type <- "controls"
      control$level <- "Treatment"
      control$length <- "Longterm"
      control$plots <- "Longterm"
      control$output <- "abundance"
      control$treatment <- "control"
      control$r_filename <- "controls.csv"
    }  
  }
  control
}

#' @title Remove species from the rodent data table
#'
#' @description Remove species from the rodent data table
#'
#' @param rodents Class-\code{rodents} \code{data.table} of rodent data.
#'
#' @param drop_spp \code{character} vector of species name(s) (column header)
#'   to drop from \code{data} or \code{NULL} to not drop any species.
#'
#' @return Trimmed \code{rodents}.
#'
#' @export
#'
remove_spp <- function(rodents, drop_spp = "PI"){
  check_args()
  if (!is.null(drop_spp)){
    rodents <- select(rodents, -one_of(drop_spp))
  }
  rodents
}        
 
#' @title Rodent species abbreviations
#'
#' @description Creates a simple \code{character} vector of abbreviations for
#'   the \href{https://portal.naturecast.org/profiles.html}{Portal Rodents}.
#'
#' @param species_set \code{character} input of a specified set of species. 
#'   Default entry (\code{"base"}) returns the standard set of all species 
#'   included. Other options include \code{"wtotal"} (same as \code{"base"} 
#'   but with "total" as well) and \code{"evalplot"} which only returns a 
#'   subset of common species to be included in the evaluations plots.
#'
#' @param nadot \code{logical} indicator if the dot should be added to the 
#'   \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @return \code{character} vector of species abbreviations.
#' 
#' @examples
#' \dontrun{
#' 
#' rodent_spp()
#' rodent_spp("wtotal")
#' rodent_spp("evalplot")
#' }
#'
#' @export
#'
rodent_spp <- function(species_set = "base", nadot = FALSE){
  check_args()
  xNAx <- "NA"
  if (nadot){
    xNAx <- "NA."
  }
  if (is.null(species_set) || species_set == "base"){
    out <- c("BA", "DM", "DO", "DS", xNAx, "OL", "OT", "PB", "PE", "PF", "PH", 
             "PI", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
  } else if (species_set == "wtotal"){
    out <- c("BA", "DM", "DO", "DS", xNAx, "OL", "OT", "PB", "PE", "PF", "PH", 
             "PI", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO",
              "total")
  } else if (species_set == "evalplot"){
    out <- c("BA", "DM", "DO", "PP", "OT", "NA", "total")
  }
  out
}

#' @title Which columns are species columns
#'
#' @description Check column names to verify if they are species names.
#'
#' @param rodents Class-\code{rodents} \code{data.table} of rodent data.
#'
#' @param spp_names \code{character} vector of species name abbreviations.
#'
#' @return \code{logical} vector corresponding to the columns in \code{x}.
#'
#' @export
#'
is.spcol <- function(rodents, spp_names = rodent_spp()){
  check_args()
  colnames(rodents) %in% spp_names
}

#' @title Remove the treatment column from the rodent data table
#'
#' @description Remove the treatment column from the rodent data table if 
#'   needed (in the case of controls, for example, where the data have been
#'   reduced already so the column is vestigial).
#'
#' @param rodents Class-\code{rodents} \code{data.table} of rodent data.
#'
#' @param level \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @param treatment \code{character} input for 
#'   \code{\link[portalr]{summarize_rodent_data}}, automatically set by 
#'   \code{tmnt_type}.
#'
#' @return Trimmed \code{data}.
#'
#' @export
#'
trim_treatment <- function(rodents, level = "Site", treatment = NULL){
  if (level == "Treatment"){
    rodents <-  rodents %>%
                filter(treatment == !!treatment) %>%
                select(-treatment)
  }
  classy(rodents, c("data.frame", "rodents"))
}

#' @title Transfer the trapping table to the data folder
#'
#' @description Move the trapping table from the PortalData sub directory to 
#'   the data subdirectory for use.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}. An optional format for inputting the file tree
#'   structure. If used, overrides \code{base}, \code{main}, and \code{subs}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'   quieted.
#'
#' @export
#'
transfer_trapping_table <- function(tree = dirtree(), quiet = FALSE){
  from <- file_paths(tree, "PortalData/Rodents/Portal_rodent_trapping.csv")
  to <- file_paths(tree, "data/trapping.csv")
  if (file.exists(from)){
    msg <- "Loading rodent trapping table into data subdirectory"
    messageq(msg, quiet)
    ttable <- read.csv(from, stringsAsFactors = FALSE)
    write.csv(ttable, to, row.names = FALSE)
  }
}
