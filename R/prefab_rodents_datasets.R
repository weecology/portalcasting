#' @title Provide the names or controls for the prefab rodents datasets
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) rodent datasets or a \code{list} of their controls
#'
#' @param interpolate \code{logical} value indicating if the interpolated data only should be listed (\code{interpolate = TRUE}), if the non-interpolated data only should be listed (\code{interpolate = FALSE}), or if both should be listed (\code{interpolate = NULL}, default).  
#'
#' @return \code{prefab_rodent_datasets}: \code{character} vector of dataset names. \cr
#'         \code{prefab_rodent_dataset_controls}: \code{list} vector of dataset controls. \cr
#'
#' @examples
#'  prefab_rodent_datasets()
#'  prefab_rodent_dataset_controls()
#'
#' @name prefabricated_rodent_datasets
#'
NULL

#' @rdname prefabricated_rodent_datasets
#'
#' @export
#'
prefab_rodent_datasets <- function(interpolate = NULL){

  names(prefab_rodent_dataset_controls(interpolate = interpolate))

}


#' @rdname prefabricated_rodent_datasets
#'
#' @export
#'
prefab_rodent_dataset_controls <- function(interpolate = NULL) {
  out <- list(
    all                = list(metadata = list(name       = "all", 
                                              descriptor = "classic dataset, all plots combined"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = base_species(), 
                                              total = TRUE,
                                              interpolate = FALSE, 
                                              clean = FALSE, 
                                              type = "Rodents", 
                                              level = "Site", 
                                              plots = "all", 
                                              treatment = NULL, 
                                              min_plots = 24, 
                                              min_traps = 1, 
                                              output = "abundance", 
                                              fillweight = FALSE, 
                                              unknowns = FALSE, 
                                              time = "newmoon", 
                                              na_drop = FALSE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE,
                                              filename = "rodents_all.csv")),
    all_interp         = list(metadata = list(name = "all_interp", 
                                              descriptor = "classic dataset, all plots combined, interpolated for models that cannot have missing data"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = base_species(), 
                                              total = TRUE,
                                              interpolate = TRUE, 
                                              clean = FALSE, 
                                              type = "Rodents", 
                                              level = "Site", 
                                              plots = "all", 
                                              treatment = NULL, 
                                              min_plots = 24, 
                                              min_traps = 1,
                                              output = "abundance", 
                                              fillweight = FALSE, 
                                              unknowns = FALSE, 
                                              time = "newmoon", 
                                              na_drop = TRUE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_all_interp.csv")),
    controls           = list(metadata = list(name = "controls", 
                                              descriptor = "classic dataset, control plots combined"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = base_species(), 
                                              total = TRUE, 
                                              interpolate = FALSE, 
                                              clean = FALSE, 
                                              type = "Rodents", 
                                              level = "Treatment", 
                                              plots = "Longterm", 
                                              treatment = "control", 
                                              min_plots = 24, 
                                              min_traps = 1, 
                                              output = "abundance", 
                                              fillweight = FALSE, 
                                              unknowns = FALSE, 
                                              time = "newmoon", 
                                              na_drop = FALSE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_controls.csv")),
    controls_interp    = list(metadata = list(name = "controls_interp", 
                                              descriptor = "classic dataset, control plots combined, interpolated for models that cannot have missing data"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = base_species(), 
                                              total = TRUE, 
                                              interpolate = TRUE, 
                                              clean = FALSE, 
                                              type = "Rodents", 
                                              level = "Treatment", 
                                              plots = "Longterm", 
                                              treatment = "control", 
                                              min_plots = 24, 
                                              min_traps = 1, 
                                              output = "abundance", 
                                              fillweight = FALSE, 
                                              unknowns = FALSE, 
                                              time = "newmoon", 
                                              na_drop = FALSE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_controls_interp.csv")),
    exclosures         = list(metadata = list(name = "exclosures", 
                                              descriptor = "classic dataset, exclosure plots combined"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = base_species(), 
                                              total = TRUE, 
                                              interpolate = FALSE, 
                                              clean = FALSE, 
                                              type = "Rodents", 
                                              level = "Treatment", 
                                              plots = "Longterm", 
                                              treatment = "exclosure", 
                                              min_plots = 24, 
                                              min_traps = 1, 
                                              output = "abundance", 
                                              fillweight = FALSE, 
                                              unknowns = FALSE, 
                                              time = "newmoon", 
                                              na_drop = FALSE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_exclosures.csv")),
    exclosures_interp  = list(metadata = list(name = "exclosures_interp", 
                                              descriptor = "classic dataset, exclosure plots combined, interpolated for models that cannot have missing data"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = base_species(), 
                                              total = TRUE, 
                                              interpolate = TRUE, 
                                              clean = FALSE, 
                                              type = "Rodents", 
                                              level = "Treatment", 
                                              plots = "Longterm", 
                                              treatment = "exclosure", 
                                              min_plots = 24, 
                                              min_traps = 1, 
                                              output = "abundance", 
                                              fillweight = FALSE, 
                                              unknowns = FALSE, 
                                              time = "newmoon", 
                                              na_drop = FALSE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_exclosures_interp.csv")),
    dm_controls        = list(metadata = list(name = "dm_controls", 
                                              descriptor = "DM only, control plots combined"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = "DM",  
                                              total = FALSE, 
                                              interpolate = FALSE,  
                                              clean = FALSE, 
                                              type = "Rodents",  
                                              level = "Treatment", 
                                              plots = "Longterm",  
                                              treatment = "control", 
                                              min_plots = 24,  
                                              min_traps = 1, 
                                              output = "abundance",  
                                              fillweight = FALSE, 
                                              unknowns = FALSE,  
                                              time = "newmoon", 
                                              na_drop = FALSE,  
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_dm_controls.csv")),
    dm_controls_interp = list(metadata = list(name = "dm_controls_interp",  
                                              descriptor = "DM only, control plots combined, interpolated for models that cannot have missing data"),
                              fun      = prep_rodent_dataset,
                              args     = list(species = "DM", 
                                              total = FALSE,  
                                              interpolate = TRUE, 
                                              clean = FALSE, 
                                              type = "Rodents",  
                                              level = "Treatment", 
                                              plots = "Longterm",  
                                              treatment = "control", 
                                              min_plots = 24,  
                                              min_traps = 1, 
                                              output = "abundance",  
                                              fillweight = FALSE, 
                                              unknowns = FALSE,  
                                              time = "newmoon", 
                                              na_drop = FALSE, 
                                              zero_drop = TRUE, 
                                              effort = TRUE, 
                                              filename = "rodents_dm_controls_interp.csv")))

  if (is.null(interpolate)) {

    out

  } else {

    dsnames <- names(out)

    if (interpolate) {

      out[grepl("_interp", dsnames)]

    } else {

      out[!grepl("_interp", dsnames)]

    }

  }

}