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
                      nadot       = nadot,
                      arg_checks  = arg_checks)

  colnames(rodents_tab)[sp_col]

}

#' @title Provide the List of Prefabricated Rodent Datasets
#'
#' @description Create a \code{list} of dataset lists, each used to create a dataset in \code{\link{fill_data}}. \cr \cr
#'              The currently prefabricated datasets include \code{"all"}, \code{"all_interp"}, \code{"controls"}, \code{"controls_interp"}, \code{"exclosures"}, \code{"exclosures_interp"}, \code{"dm_controls"}, and \code{"dm_controls_interp"}. 
#'
#' @return \code{list} of dataset lists, each containing \code{metadata}, \code{fun}, and \code{args} elements -- the latter two of which are used in \code{\link{do.call}} to create data files.
#'
#' @examples
#'  prefab_datasets()
#'
#' @export
#'
prefab_rodent_datasets <- function () {

  list(

    all = list(

            metadata = list(
                         name        = "all",
                         description = "classic dataset, all plots combined"),

            fun      = prepare_rodent_dataset,

            args     = list(
                         name        = "all",
                         clean       = FALSE,
                         level       = "Site",
                         type        = "Rodents",
                         plots       = "all",
                         treatment   = NULL,
                         unknowns    = FALSE,
                         shape       = "crosstab",
                         time        = "newmoon",
                         output      = "abundance",
                         fillweight  = FALSE,
                         na_drop     = FALSE,
                         zero_drop   = FALSE,
                         min_traps   = 1,
                         min_plots   = 24,
                         species     = base_species(),
                         total       = TRUE,
                         effort      = TRUE,
                         interpolate = FALSE)),

    all_interp = list(


            metadata = list(
                         name        = "all_interp",
                         description = "classic dataset, all plots combined, interpolated for models that cannot have missing data"),

            fun      = prepare_rodent_dataset,

            args     = list(
                         name        = "all_interp",
                         clean       = FALSE,
                         level       = "Site",
                         type        = "Rodents",
                         plots       = "all",
                         treatment   = NULL,
                         unknowns    = FALSE,
                         shape       = "crosstab",
                         time        = "newmoon",
                         output      = "abundance",
                         fillweight  = FALSE,
                         na_drop     = FALSE,
                         zero_drop   = FALSE,
                         min_traps   = 1,
                         min_plots   = 24,
                         species     = base_species(),
                         total       = TRUE,
                         effort      = TRUE,
                         interpolate = TRUE)),
    controls = 
         list(metadata = list(),
              fun = prepare_rodent_dataset,
              args = list(name = "controls",
                          clean = FALSE,
                          level = "Treatment",
                          type = "Rodents",
                          plots = "Longterm",
                          treatment = "control",
                          unknowns = FALSE,
                          shape = "crosstab",
                          time = "newmoon",
                          output = "abundance",
                          fillweight = FALSE,
                          na_drop = FALSE,
                          zero_drop = FALSE,
                          min_traps = 1,
                          min_plots = 24,
                          species = base_species(),
                          total = TRUE,
                          effort = TRUE,
                          interpolate = FALSE)),

    controls_interp = 
         list(metadata = list(),
              fun = prepare_rodent_dataset,
              args = list(name = "controls_interp",
                          clean = FALSE,
                          level = "Treatment",
                          type = "Rodents",
                          plots = "Longterm",
                          treatment = "control",
                          unknowns = FALSE,
                          shape = "crosstab",
                          time = "newmoon",
                          output = "abundance",
                          fillweight = FALSE,
                          na_drop = FALSE,
                          zero_drop = FALSE,
                          min_traps = 1,
                          min_plots = 24,
                          species = base_species(),
                          total = TRUE,
                          effort = TRUE,
                          interpolate = TRUE)),

    exclosures = 
         list(metadata = list(),
              fun = prepare_rodent_dataset,
              args = list(name = "exclosures",
                          clean = FALSE,
                          level = "Treatment",
                          type = "Rodents",
                          plots = "Longterm",
                          treatment = "exclosure",
                          unknowns = FALSE,
                          shape = "crosstab",
                          time = "newmoon",
                          output = "abundance",
                          fillweight = FALSE,
                          na_drop = FALSE,
                          zero_drop = FALSE,
                          min_traps = 1,
                          min_plots = 24,
                          species = base_species(),
                          total = TRUE,
                          effort = TRUE,
                          interpolate = FALSE)),

    exclosures_interp = 
         list(metadata = list(),
              fun = prepare_rodent_dataset,
              args = list(name = "exclosures_interp",
                          clean = FALSE,
                          level = "Treatment",
                          type = "Rodents",
                          plots = "Longterm",
                          treatment = "exclosure",
                          unknowns = FALSE,
                          shape = "crosstab",
                          time = "newmoon",
                          output = "abundance",
                          fillweight = FALSE,
                          na_drop = FALSE,
                          zero_drop = FALSE,
                          min_traps = 1,
                          min_plots = 24,
                          species = base_species(),
                          total = TRUE,
                          effort = TRUE,
                          interpolate = TRUE)),


    dm_controls = 
         list(metadata = list(),
              fun = prepare_rodent_dataset,
              args = list(name = "dm_controls",
                          clean = FALSE,
                          level = "Treatment",
                          type = "Rodents",
                          plots = "Longterm",
                          treatment = "control",
                          unknowns = FALSE,
                          shape = "crosstab",
                          time = "newmoon",
                          output = "abundance",
                          fillweight = FALSE,
                          na_drop = FALSE,
                          zero_drop = FALSE,
                          min_traps = 1,
                          min_plots = 24,
                          species = "DM",
                          total = FALSE,
                          effort = TRUE,
                          interpolate = FALSE)),

    dm_controls_interp = 
         list(metadata = list(),
              fun = prepare_rodent_dataset,
              args = list(name = "dm_controls_interp",
                          clean = FALSE,
                          level = "Treatment",
                          type = "Rodents",
                          plots = "Longterm",
                          treatment = "control",
                          unknowns = FALSE,
                          shape = "crosstab",
                          time = "newmoon",
                          output = "abundance",
                          fillweight = FALSE,
                          na_drop = FALSE,
                          zero_drop = FALSE,
                          min_traps = 1,
                          min_plots = 24,
                          species = "DM",
                          total = FALSE,
                          effort = TRUE,
                          interpolate = TRUE)))

}


#' @title Prepare Rodents Data Tables for Forecasting
#'
#' @description Wraps around \code{\link[portalr]{summarize_rodent_data}} to produce a \code{data.frame} associated with a set of data specifications. 
#'
#' @param name \code{character} name of the data set.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param species \code{character}-valued vector of species names to include. 
#'
#' @param total \code{logical} value indicating if a total (sum across species should be added or not. Only available if more than one species is included. 
#'
#' @param interpolate \code{logical} value indicating if the data should be interpolated to fill in \code{NA} values (using \code{\link[forecast]{na.interp}}). 
#'
#' @param clean \code{logical} indicator of if only the rodent data that passed QA/QC (\code{clean = TRUE}) or if all data (\code{clean = FALSE}) should be loaded.
#'
#' @param type \code{character} value of the rodent data set type, according to pre-existing definitions. An alternative toggle to \code{species}. \cr \cr
#'             Either all species (\code{type = "Rodents"}) or only granivoes (\code{type = "Granivores"}). 
#'
#' @param level \code{character} indicating the type of summary: \code{"Plot"}, \code{"Treatment"}, or \code{"Site"}. Pipes directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param plots Specification of subset of plots. Can be a vector of \code{numeric} plots indicators or specific sets indicated by \code{character} values: \code{"all"} plots or \code{"Longterm"} plots (plots that have had the same treatment for the entire time series).
#'
#' @param treatment \code{character} indicating the specific treatment(s) to trim to if \code{level = "Treatment"}: \code{"control"}, \code{"exclosure"}, \code{"removal"}, or \code{"spectabs"} 
#'
#' @param min_plots \code{integer} (or integer \code{numeric}) of the minimum number of plots surveyed for a survey to be used. 
#'
#' @param min_traps \code{integer} (or integer \code{numeric}) of the minimum number of traps collected for a plot to be used.
#'
#' @param output \code{character} indicating the type of data: \code{"abundance"}, \code{"biomass"}, or \code{"energy"}. 
#'
#' @param fillweight \code{logical} specifier of whether to fill in unknown weights with other records from that individual or species, where possible.
#'
#' @param shape \code{character} value indicating a "crosstab" or "flat" output. 
#'
#' @param verbose \code{logical} indicator if detailed messages should be shown.
#'
#' @param unknowns \code{logical} indicator to either remove all individuals not identified to species (\code{unknowns = FALSE}) or sum them in an additional column (\code{unknowns = TRUE}.
#'
#' @param time \code{character} value specifying the format of the time index in the output. Options are \code{"period"} (sequential Portal surveys), \code{"newmoon"} (lunar cycle numbering), and \code{"date"} (calendar date). \cr \cr
#'             The default \code{time = "newmoon"} produces an equispaced observation timestep, a common format format for discrete-time modeling. 
#'
#' @param na_drop \code{logical} indicator of if \code{NA} values (representing insufficient sampling) should be dropped.
#'
#' @param zero_drop \code{logical} indicator of if \code{0} values (representing sufficient sampling but no detection) should be dropped.
#'
#' @param effort \code{logical} indicator of if the effort columns should be included in the output.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing files should be updated (most users should leave as \code{TRUE}).
#'
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param rodents_tab Rodents \code{data.frame} data table from \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @return \code{data.frame} for the specified data set.
#'
#' @name prepare_rodents
#' 
NULL


#' @rdname prepare_rodents
#'
#' @export
#'
prepare_rodent_datasets <- function (main     = ".",
                                     datasets = prefab_rodent_datasets(),
                                     quiet    = FALSE,
                                     verbose  = TRUE) {


  return_if_null(datasets)

  messageq("   -Rodents Datasets", quiet = quiet)

  ndatasets <- length(datasets)
  out <- vector("list", ndatasets)
  for (i in 1:ndatasets) {

    args <- update_list(orig_list = datasets[[i]]$args, 
                        main      = main, 
                        quiet     = quiet, 
                        verbose   = verbose)
    out[[i]] <- do.call(datasets[[i]]$fun, args)
  
  }
  names(out) <- names(datasets)
  invisible(out)

}


#' @rdname prepare_rodents
#' 
#' @export
#'
prepare_rodent_dataset <- function(name        = NULL,
                                   main        = ".",
                                   clean       = FALSE,
                                   level       = "Site",
                                   type        = "Rodents",
                                   plots       = "all",
                                   unknowns    = FALSE,
                                   shape       = "crosstab",
                                   time        = "newmoon",
                                   output      = "abundance",
                                   fillweight  = FALSE,
                                   treatment   = NULL,
                                   na_drop     = FALSE,
                                   zero_drop   = FALSE,
                                   min_traps   = 1,
                                   min_plots   = 24,
                                   effort      = TRUE,
                                   species     = base_species(),
                                   total       = TRUE,
                                   interpolate = FALSE,
                                   save        = TRUE,
                                   overwrite   = TRUE,
                                   quiet       = FALSE,
                                   verbose     = FALSE) {

  return_if_null(name)

  messageq("   -", name, quiet = quiet)

  rodents_tab <- summarize_rodent_data(path       = file.path(main, "raw"), 
                                       clean      = clean, 
                                       level      = level, 
                                       type       = type, 
                                       plots      = plots, 
                                       unknowns   = unknowns,
                                       shape      = shape, 
                                       time       = time, 
                                       output     = output,
                                       fillweight = fillweight, 
                                       na_drop    = na_drop,
                                       zero_drop  = zero_drop, 
                                       min_traps  = min_traps,
                                       min_plots  = min_plots, 
                                       effort     = effort, 
                                       quiet      = !verbose) 

  out <- process_rodent_dataset(rodents_tab = rodents_tab, 
                                main        = main, 
                                level       = level, 
                                treatment   = treatment, 
                                species     = species, 
                                interpolate = interpolate, 
                                total       = total, 
                                quiet       = quiet, 
                                verbose     = verbose)

  filename <- paste0("rodents_", name, ".csv")

  write_data(out, 
             main      = main, 
             save      = save, 
             filename  = filename, 
             overwrite = overwrite, 
             quiet     = !verbose)

  out
}


#' @rdname prepare_rodents
#' 
#' @export
#'
process_rodent_dataset <- function(rodents_tab, 
                                   main        = ".", 
                                   level       = "Site", 
                                   treatment   = NULL, 
                                   species     = base_species(), 
                                   interpolate = FALSE,
                                   total       = TRUE, 
                                   quiet       = FALSE, 
                                   verbose     = FALSE) {

  nspecies <- length(species)

  total <- ifelse(nspecies == 1, FALSE, total)

  sp_col <- colnames(rodents_tab) %in% all_species()
  which_sp_col <- which(sp_col)
  which_not_sp_col <- which(!sp_col)

  sp_col_in <- colnames(rodents_tab)[sp_col] %in% species
  cols_in <- c(which_not_sp_col, which_sp_col[sp_col_in])
  out_tab <- rodents_tab[ , cols_in] 

  if (total) {
 
    out_tab$total <- rowSums(rodents_tab[ , which_sp_col[sp_col_in]])

  }


  if (level == "Treatment") {

    rows_in <- out_tab$treatment %in% treatment
    cols_in <- colnames(out_tab) != "treatment"
    out_tab <- out_tab[rows_in, cols_in]

  }

  if (interpolate) {

    for(i in 1:nspecies) {

      col_in <- which(colnames(out_tab) == species[i])
      x <- na.interp(out_tab[ , col_in])
      out_tab[ , col_in] <- x

    }

    x <- na.interp(out_tab$total)
    out_tab$total <- x
  }

  out_tab

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





