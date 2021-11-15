
#' @title Determine if Columns in a Table are Species Columns
#'
#' @description Given a table, returns a \code{logical} vector indicating if each column is a species' column or not.
#'
#' @param rodents_tab \code{data.frame} of columns to be checked. 
#'
#' @param species \code{character} vector indicating species names to use in determining if columns are species columns. Defaults to \code{\link{all_species}}.
#'
#' @param total \code{logical} indicator if the total should be included.
#'
#' @param nadot \code{logical} indicator if the dot should be added to the \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @return 
#'  \code{is_sp_col}: \code{logical} vector indicating if each column is a species' column or not. \cr \cr
#'  \code{species_from_table}: \code{character} vector of species names from the column names in the \code{rodents_tab} data table. 
#'  
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   rodents_tab <- prep_rodents_table()
#'   is_sp_col(rodents_tab)
#'   species_from_table(rodents_tab)
#'  }
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
#' @param set \code{character} input of a specified set of species. Default entry (\code{"base"}) returns the standard set of all species included. Other options include \code{"wtotal"} (same as \code{"base"} but with "total" as well) and \code{"evalplot"} which only returns a subset of common species to be included in the evaluations plots.
#'
#' @param nadot \code{logical} indicator if the dot should be added to the \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @param species \code{character}-valued vector of species names to include.
#'
#' @param total \code{logical} value indicating if \code{"total"} should be added or not.
#'
#' @return \code{character} vector of species abbreviations.
#' 
#' @examples 
#'  rodent_species()
#'  rodent_species(set = "all")
#'  rodent_species("BA")
#'  rodent_species("BA", set = "all")
#'  all_species()
#'  all_species(nadot = TRUE)
#'  all_species(total = TRUE)
#'  base_species()
#'  evalplot_species()
#'
#' @export
#'
rodent_species <- function (species = NULL, 
                            set     = NULL, 
                            nadot   = FALSE, 
                            total   = FALSE) {

  return_if_null(c(species, set))

  out <- NULL

  if (!is.null(set) && set == "all") {

    out <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PI", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")

  } else if (!is.null(set) && set == "base") {

    out <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")

  } else if (!is.null(set) && set == "evalplot") {

    out <- c("BA", "DM", "DO", "PP", "OT", "NA")

  }

  if (total) {

    out <- c(out, "total")

  }

  if (nadot) {

    out[which(out == "NA")] <- "NA."

  }

  unique(c(species, out))

}

#' @rdname rodent_species
#'
#' @export
#'
all_species <- function (species = NULL, 
                         nadot   = FALSE, 
                         total   = FALSE) {

  rodent_species(species = species, 
                 set     = "all", 
                 nadot   = nadot,
                 total   = total)

}


#' @rdname rodent_species
#'
#' @export
#'
base_species <- function (species = NULL, 
                         nadot   = FALSE, 
                         total   = FALSE) {

  rodent_species(species = species, 
                 set     = "base", 
                 nadot   = nadot,
                 total   = total)

}

#' @rdname rodent_species
#'
#' @export
#'
evalplot_species <- function (species = NULL, 
                              nadot   = FALSE, 
                              total   = FALSE) {

  rodent_species(species = species, 
                 set     = "evalplot", 
                 nadot   = nadot,
                 total   = total)

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



