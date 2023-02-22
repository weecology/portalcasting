
#' @title Determine if Columns in a Table are Species Columns
#'
#' @description Given a table, returns a \code{logical} vector indicating if each column is a species' column or not.
#'
#' @param rodents_tab \code{data.frame} of columns to be checked. 
#'
#' @param species \code{character} vector indicating species' name abbreviations to use in determining if columns are species columns. Defaults to \code{\link{all_species}}.
#'
#' @param total \code{logical} indicator if the total should be included.
#'
#' @param nadot \code{logical} indicator if the dot should be added to the \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @return 
#'  \code{is_sp_col}: \code{logical} vector indicating if each column is a species' column or not. \cr \cr
#'  \code{species_from_table}: \code{character} vector of species names from the column names in the \code{rodents_tab} data table. 
#'  
#' @name species_in_tables
#'
NULL 

#' @rdname species_in_tables
#'
#' @export
#'
is_sp_col <- function (rodents_tab = NULL, 
                       species     = NULL,
                       total       = FALSE, 
                       nadot       = FALSE) {

  species <- ifnull(species, all_species(total = total, nadot = nadot))
  colnames(rodents_tab) %in% species

}

#' @rdname species_in_tables
#'
#' @export
#'
species_from_table <- function(rodents_tab = NULL, 
                               total       = FALSE, 
                               nadot       = FALSE){

  sp_col <- is_sp_col(rodents_tab = rodents_tab,
                      total       = total, 
                      nadot       = nadot)

  colnames(rodents_tab)[sp_col]

}


#' @title Rodent species abbreviations
#'
#' @description Creates a simple \code{character} vector of abbreviations for the Portal \href{https://portal.naturecast.org/profiles.html}{Rodents}.
#'
#' @param set \code{character} input of a specified set of species. Options include \code{all} (all species included) and \code{base} (all but PI). 
#'
#' @param nadot \code{logical} indicator if the dot should be added to the \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @param total \code{logical} value indicating if \code{"total"} should be added or not.
#'
#' @param type \code{character} value indicating the output type. Current options include \code{'abbreviation'} (default, two-letter abbreviation), \code{'Latin'} (full scientific names), and  \code{'table'} a \code{data.frame} of the abbreviations and Latin names.
#'
#' @return \code{character} vector of species abbreviations.
#' 
#' @examples 
#'  rodent_species()
#'  rodent_species(type = "Latin")
#'  rodent_species(type = "table")
#'  rodent_species(type = "table", total = TRUE)
#'  rodent_species(set = "all")
#'  all_species()
#'  all_species(nadot = TRUE)
#'  all_species(total = TRUE)
#'  base_species()
#'  eval_species()
#'
#' @export
#'
rodent_species <- function (set   = NULL, 
                            nadot = FALSE, 
                            total = FALSE,
                            type  = "abbreviation") {

  return_if_null(set)

  out <- NULL

  type <- tolower(type)


  if (set == "all") {

    out_abb <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PI", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
    out_lat <- c("Baiomys taylori", "Dipodomys merriami", "Dipodomys ordii", "Dipodomys spectabilis", "Neotoma albigula", "Onychomys leucogaster", "Onychomys torridus", "Chaetodipus baileyi", "Peromyscus eremicus", "Perognathus flavus", "Chaetodipus hispidus", "Chaetodipus intermedius", "Peromyscus leucopus", "Peromyscus maniculatus", "Chaetodipus penicillatus", "Reithrodontomys fulvescens", "Reithrodontomys megalotis", "Reithrodontomys montanus", "Sigmodon fulviventer", "Sigmodon hispidus", "Sigmodon ochrognathus")

  } else if (set == "base") {

    out_abb <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
    out_lat <- c("Baiomys taylori", "Dipodomys merriami", "Dipodomys ordii", "Dipodomys spectabilis", "Neotoma albigula", "Onychomys leucogaster", "Onychomys torridus", "Chaetodipus baileyi", "Peromyscus eremicus", "Perognathus flavus", "Chaetodipus hispidus", "Peromyscus leucopus", "Peromyscus maniculatus", "Chaetodipus penicillatus", "Reithrodontomys fulvescens", "Reithrodontomys megalotis", "Reithrodontomys montanus", "Sigmodon fulviventer", "Sigmodon hispidus", "Sigmodon ochrognathus")

  } else if (set == "eval") {

    out_abb <- c("BA", "DM", "DO", "PP")
    out_lat <- c("Baiomys taylori", "Dipodomys merriami", "Dipodomys ordii", "Chaetodipus penicillatus")

  } else {

    stop ("`type` must be `NULL`, 'all', 'base', or 'eval'")    
 
  }

  if (total) {

    out_abb[which(out_abb == "NA")] <- "NA."

  }

  if (total) {

    out_abb <- c(out_abb, "total")
    out_lat <- c(out_lat, "total")

  }

  
  if (type == "latin") {

    out <- out_lat

  } else if (type == "abbreviation") {

    out <- out_abb

  } else if (type == "table") {

    out <- data.frame(abbreviation = out_abb, Latin = out_lat)

  } else {

    stop ("`type` must be 'abbreviation', 'Latin', or 'table'")    

  } 

  out

}

#' @rdname rodent_species
#'
#' @export
#'
all_species <- function (nadot   = FALSE, 
                         total   = FALSE,
                         type    = "abbreviation") {

  rodent_species(set   = "all", 
                 nadot = nadot,
                 total = total,
                 type  = type)

}


#' @rdname rodent_species
#'
#' @export
#'
base_species <- function (nadot   = FALSE, 
                          total   = FALSE,
                          type    = "abbreviation") {

  rodent_species(set   = "base", 
                 nadot = nadot,
                 total = total,
                 type  = type)

}


#' @rdname rodent_species
#'
#' @export
#'
eval_species <- function (nadot   = FALSE, 
                          total   = FALSE,
                          type    = "abbreviation") {

  rodent_species(set   = "eval", 
                 nadot = nadot,
                 total = total,
                 type  = type)

}



#' @title Conform NA entries to "NA" entries
#'
#' @description Given the species abbreviation NA, when data are read in, there can be an \code{NA} when it should be an \code{"NA"}. This function conforms the entries to be proper character values. 
#'
#' @param dfv Either [1] a \code{data.frame} containing \code{colname} as a column with \code{NA}s that need to be conformed to \code{"NA"}s or [2] a vector with \code{NA}s that need to be conformed to \code{"NA"}s.
#'
#' @param colname \code{character} value of the column name in \code{tab} to conform the \code{NA}s to \code{"NA"}s.
#'
#' @return \code{x} with any \code{NA} in \code{colname} replaced with \code{"NA"}.
#'
#' @examples
#'  na_conformer(c("a", "b", NA, "c"))
#'
#' @export
#'
na_conformer <- function(dfv, colname = "species"){
  
  if (is.vector(dfv)) {

    naentries <- which(is.na(dfv))
    dfv[naentries] <- "NA"

  } else if (is.data.frame(dfv)) {

    nasppname <- which(is.na(dfv[ , colname]))

    if (length(nasppname) > 0) {

      dfv[nasppname, colname] <- "NA"

    }

  } 

  dfv

}


