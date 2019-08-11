
prep_rodents_list <- function(){

  
}

#' @title Prepare a rodents data table for forecasting
#'
#' @description This set of functions provides a convenient wrapper on
#'  \code{\link[portalr]{summarize_rodent_data}} to generate a specific
#'  data table for use in the forecasting pipeline. Many arguments pipe
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param moons Moons \code{data.frame}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param species \code{character}-valued vector of species names to include 
#'  in the forecasting data table. Defaults to all species but \code{"PI"}
#'  via \code{\link{base_species}}
#'
#' @param ref_species \code{character}-valued vector of all possible species 
#'  names that could be used should be set up via \code{\link{all_species}}.
#'
#' @param total \code{logical} value indicating if a total (sum across 
#'  species) should be added or not. Only available if more than one species
#'  is included. 
#'
#' @param level \code{character} indicating the type of summary:
#'  \code{"Plot"}, \code{"Treatment"}, or \code{"Site"}. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param treatment \code{character} indicating the specific treatment(s) to
#'  trim to if \code{level = "Treatment"}: \code{"control"},
#'  \code{"exclosure"}, \code{"removal"}, or \code{"spectabs"} .
#'
#' @param output \code{character} indicating the type of data:
#'  \code{"abundance"}, \code{"biomass"}, or \code{"energy"}. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param min_traps \code{integer} (or integer \code{numeric}) of the minimum 
#'  number of traps collected for a plot to be used. Pipes directly to 
#'  \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param min_plots \code{integer} (or integer \code{numeric}) of the minimum 
#'  number of plots surveyed for a survey to be used.Pipes directly to 
#'  \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param plots Specification of subset of plots. Can be a vector of 
#'  \code{numeric} plots indicators or specific sets indicated by
#'  \code{character} values: \code{"all"} plots or \code{"Longterm"} plots
#'  (plots that have had the same treatment for the entire time series).
#'
#' @param rodents \code{data.frame} rodents table, with varying levels of 
#'  editing.
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param filename \code{character} name of the file for saving the output.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @return \code{data.frame} rodents table, with varying levels of editing
#'  for use in forecasting. \cr \cr
#'  \code{prep_rodents_table}, \code{trim_time}: a fully appended and 
#'  formatted \code{data.frame} (also saved out if \code{save = TRUE}). 
#'  \cr \cr
#'  \code{trim_species}, \code{add_total}, \code{trim_treatment}, 
#'  \code{add_moons}: appropriately appended rodents \code{data.frame}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   prep_rodents_table()
#'   raw_path <- sub_paths(".", specific_subs = "raw")
#'   dat1 <- portalr::summarize_rodent_data(path = raw_path, clean = FALSE)
#'   dat2 <- trim_species(dat1)
#'   dat3 <- add_total(dat2)
#'   dat4 <- trim_treatment(dat3)
#'   dat5 <- add_moons(dat4)
#'   dat6 <- trim_time(dat5, 217)
#'  }
#'
#' @export
#'
prep_rodents_table <- function(species = base_species(), total = TRUE, 
                               moons = prep_moons(main = main),
                               main = ".", start_moon = 217, end_moon = NULL,
                               level = "Site", treatment = NULL,
                               plots = "all", min_plots = 24, min_traps = 1, 
                               output = "abundance", quiet = TRUE,
                               ref_species = all_species(),
                               save = TRUE, filename = "rodents_all.csv",
                               overwrite = TRUE){
  return_if_null(species) 
  nspecies <- length(species)
  total <- ifelse(nspecies == 1, FALSE, total)
  raw_path <- sub_paths(main, specific_subs = "raw")
  summarize_rodent_data(path = raw_path, clean = FALSE, type = "Rodents", 
                        level = level, plots = plots, min_traps = min_traps,
                        min_plots = min_plots, output = output,
                        quiet = quiet) %>%
  trim_species(species, ref_species) %>%
  add_total(total) %>%
  trim_treatment(level, treatment) %>%
  add_moons(moons) %>%
  trim_time(start_moon, end_moon) %>%
  data_out(main, save, filename, overwrite, quiet)
}

#' @rdname prep_rodents_table
#'
#' @export
#'
trim_time <- function(rodents, start_moon = NULL, end_moon = NULL){
  subset(rodents, newmoonnumber >= start_moon) %>%
  subset(newmoonnumber <= min(c(end_moon, max(newmoonnumber)))) %>%
  select(-newmoondate, -censusdate) 
}

#' @rdname prep_rodents_table
#'
#' @export
#'
add_moons <- function(rodents, moons = prep_moons()){
  inner_join(rodents, moons, by = c("period" = "period"))
}

#' @rdname prep_rodents_table
#'
#' @export
#'
add_total <- function(rodents, total = TRUE){
  if(total){
    total_count <- rowSums(rodents[ , is_sp_col(rodents)])
    rodents <- mutate(rodents, total = total_count)
  }
  rodents
}

#' @rdname prep_rodents_table
#'
#' @export
#'
trim_species <- function(rodents, species = base_species(), 
                         ref_species = all_species()){
  drop_species <- ref_species[which(ref_species %in% species == FALSE)]
  if(length(drop_species) > 0){
    rodents <- select(rodents, -one_of(drop_species))
  }
  rodents
}

#' @rdname prep_rodents_table
#'
#' @export
#'
trim_treatment <- function(rodents, level = "Site", treatment = NULL){
  if(level == "Treatment"){
    rodents <- filter(rodents, treatment == !!treatment)  %>%
               select(-treatment)
  }
  rodents
}

#' @title Determine if columns in a table are species columns
#'
#' @description Given a table, returns a \code{logical} vector indicating
#'  if each column is a species' column or not.
#'
#' @param rodents \code{data.frame} of columns to be checked. 
#'
#' @param species_names \code{character} vector indicating species names to 
#'  use in determining if columns are species columns. Defaults to
#'  \code{\link{all_species}}.
#'
#' @return \code{logical} vector indicating if each column is a species' 
#'  column or not.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   rodents <- prep_rodents_table()
#'   is_sp_col(rodents)
#'  }
#'
#' @export
#'
is_sp_col <- function(rodents, species_names = all_species()){
  colnames(rodents) %in% species_names
}

#' @title Rodent species abbreviations
#'
#' @description Creates a simple \code{character} vector of abbreviations for
#'   the \href{https://portal.naturecast.org/profiles.html}{Portal Rodents}.
#'
#' @param set \code{character} input of a specified set of species. 
#'   Default entry (\code{"base"}) returns the standard set of all species 
#'   included. Other options include \code{"wtotal"} (same as \code{"base"} 
#'   but with "total" as well) and \code{"evalplot"} which only returns a 
#'   subset of common species to be included in the evaluations plots.
#'
#' @param nadot \code{logical} indicator if the dot should be added to the 
#'   \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @param species \code{character}-valued vector of species names to include.
#'
#' @param total \code{logical} value indicating if \code{"total"} should be 
#'  added or not.
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
rodent_species <- function(species = NULL, set = NULL, nadot = FALSE, 
                           total = FALSE){
  return_if_null(c(species, set))
  out <- NULL
  if(!is.null(set) && set == "all"){
    out <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", 
             "PI", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
  } else if(!is.null(set) && set == "base"){
    out <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", 
             "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
  } else if(!is.null(set) && set == "evalplot"){
    out <- c("BA", "DM", "DO", "PP", "OT", "NA")
  }
  if(total){
    out <- c(out, "total")
  }
  if(nadot){
    out[which(out == "NA")] <- "NA."
  }
  unique(c(species, out))
}

#' @rdname rodent_species
#'
#' @export
#'
all_species <- function(species = NULL, nadot = FALSE, total = FALSE){
  rodent_species(species, "all", nadot, total)
}


#' @rdname rodent_species
#'
#' @export
#'
base_species <- function(species = NULL, nadot = FALSE, total = FALSE){
  rodent_species(species, "base", nadot, total)
}

#' @rdname rodent_species
#'
#' @export
#'
evalplot_species <- function(species = NULL, nadot = FALSE, total = TRUE){
  rodent_species(species, "evalplot", nadot, total)
}
