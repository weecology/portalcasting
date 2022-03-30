#' @title Prepare Rodents Data for the Portalcasting Repository
#'
#' @description Create specified \code{datasets} using their associated function and arguments.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param datasets \code{character} vector of name(s) of rodent dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{list} of prepared \code{datasets}.
#'   
#' @export
#'
prep_rodents <- function (main     = ".",
                          datasets = prefab_rodent_datasets(),
                          settings = directory_settings(), 
                          quiet    = FALSE,
                          verbose  = FALSE) {

  return_if_null(datasets)

  envr <- environment()
  data(rodent_dataset_controls, envir = envr)
  
  datasets_list <- rodent_dataset_controls[datasets]

  messageq("  - rodents", quiet = quiet)
 

  out <- named_null_list(element_names = datasets)

  for (i in 1:length(datasets_list)) {

    out[[i]] <- do.call(what = datasets_list[[i]]$fun, 
                        args = update_list(list      = datasets_list[[i]]$args, 
                                           main      = main, 
                                           quiet     = quiet, 
                                           verbose   = verbose))
  
  }

  invisible(out)

}

#' @title Prepare Rodents Data Tables for Forecasting
#'
#' @description Workhorse function for creating portalcasting rodent datasets using existing functions. \cr \cr
#'              Wraps around \code{\link[portalr]{summarize_rodent_data}} to produce a \code{data.frame} associated with a set of data specifications. \cr \cr
#'              Ready for implementation via \code{\link{prepare_rodents}}.
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
#' @return \code{data.frame} for the specified data set.
#'
#' @export
#'
prep_rodent_dataset <- function(name        = NULL,
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

  messageq("    - ", name, quiet = quiet)

  rodents_table <- summarize_rodent_data(path       = file.path(main, "resources"), 
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


  nspecies <- length(species)

  total <- ifelse(nspecies == 1, FALSE, total)

  sp_col           <- colnames(rodents_table) %in% all_species()
  which_sp_col     <- which(sp_col)
  which_not_sp_col <- which(!sp_col)
  sp_col_in        <- colnames(rodents_table)[sp_col] %in% species
  cols_in          <- c(which_not_sp_col, which_sp_col[sp_col_in])
  out              <- rodents_table[ , cols_in, drop = FALSE] 

  if (total) {
 
    out$total <- rowSums(rodents_table[ , which_sp_col[sp_col_in], drop = FALSE])

  }


  if (level == "Treatment") {

    rows_in <- out$treatment %in% treatment
    cols_in <- colnames(out) != "treatment"
    out     <- out[rows_in, cols_in, drop = FALSE]

  }

  if (interpolate) {

    for (i in 1:nspecies) {

      col_in         <- which(colnames(out) == species[i])
      out[ , col_in] <- na.interp(out[ , col_in])

    }

    out$total <- na.interp(out$total)

  }

  write_data(dfl       = out, 
             main      = main, 
             save      = save, 
             filename  = paste0("rodents_", name, ".csv"), 
             overwrite = overwrite, 
             quiet     = !verbose)

  out

}


#' @title Create control lists for generating rodents data tables
#'
#' @description Given the number of arguments into 
#'  \code{\link[portalr]{summarize_rodent_data}} via 
#'  \code{\link{prep_rodents_table}}, it helps to have a control \code{list}  
#'  to organize them. \cr \cr
#'  \code{rodents_controls} produces the \code{list}s for any user-requested
#'  data_sets from the prefab data_sets (\code{\link{prefab_data_sets}}) and any 
#'  user-defined sets. \code{\link{rodents_control}} provides a template
#'  for the controls \code{list} which can be used to define the specific 
#'  arguments for a user's novel data set. 
#'
#' @details Any data set that is part of the \code{prefab} rodents data sets 
#'  (\code{c("all", "controls", "exclosures", "all_interp", 
#'           "controls_interp", "exclosures_interp", "dm_controls")}) has its 
#'  controls already included internally via the non-exported function
#'  \code{prefab_rodents_controls}. Users only need to include controls for
#'  non-prefab \code{data_sets}. \cr \cr
#'  Any user-defined \code{data_sets} that are not included in 
#'  \code{controls_rodents} will throw an error. \cr \cr 
#'  If any user-defined \code{controls_rodents} duplicate any existing 
#'  controls for the prefab data sets or if \code{controls_r} contains any 
#'  duplicate named elements, an error will be thrown. \cr \cr
#'  Users interested in adding data sets to the prefab rodent data sets
#'  should add the controls to the \code{prefab_rodents_controls} 
#'  non-exported function found in the \code{prepare_rodents.R} script.
#
#' @param data_sets \code{character} value(s) of the rodent data set name(s) 
#'  used to enforce certain arguments. Currently available prefab data
#'  sets are \code{"all"}, \code{"all_interp"}, \code{"controls"}, 
#'  \code{"controls_interp"}, \code{"exclosures_interp"}, 
#'  \code{"exclosures"} and \code{"dm_controls"}.  
#'
#' @param controls_rodents Additional controls for data_sets not in the 
#'  prefab set. \cr
#'  A \code{list} of a single dataset's controls or a \code{list} of 
#'  \code{list}s, each of which is a single dataset's controls. \cr \cr
#'  Presently, each dataset's controls should include 18 elements, as 
#'  our \code{\link{rodents_control}} template shows:
#'  \itemize{
#'   \item \code{name}: \code{character} value of the data set's name,
#'   \item \code{species}: \code{character}-valued vector of species names 
#'   to include.  
#'   \item \code{total}: \code{logical} value indicating if a total 
#'   (sum across species) should be added or not. Only available if more than 
#'   one species is included. 
#'   \item \code{interpolate}: \code{logical} value indicating if the  
#'   \code{NA} values should be interpolated.
#'   \item \code{clean}: \code{logical} indicator of if only the rodent data
#'    that passed QA/QC (\code{clean = TRUE}) or if all data 
#'    (\code{clean = FALSE}) should be loaded.
#'   \item \code{type}: \code{character} value of the rodent data set type, 
#'    according to pre-existing definitions. An alternative toggle to 
#'    \code{species}. \cr
#'    Either all species 
#'    (\code{type = "Rodents"}) or only granivoes 
#'    (\code{type = "Granivores"}).
#'   \item \code{level}: \code{character} indicating the type of summary:
#'    \code{"Plot"}, \code{"Treatment"}, or \code{"Site"}. Pipes 
#'    directly to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{plots}: Specification of subset of plots. Can be a vector of 
#'    \code{numeric} plots indicators or specific sets indicated by
#'    \code{character} values: \code{"all"} plots or \code{"Longterm"} plots
#'    (plots that have had the same treatment for the entire time series).
#'   \item \code{treatment}: \code{character} indicating the specific 
#'    treatment(s) to trim to if \code{level = "Treatment"}: \code{"control"},
#'    \code{"exclosure"}, \code{"removal"}, or \code{"spectabs"} 
#'   \item \code{min_plots}: \code{integer} (or integer \code{numeric}) of the 
#'    minimum number of plots surveyed for a survey to be used. Pipes 
#'    directly to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{min_traps}: \code{integer} (or integer \code{numeric}) of the 
#'    minimum number of traps collected for a plot to be used. Pipes directly
#'    to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{output}: \code{character} indicating the type of data:
#'    \code{"abundance"}, \code{"biomass"}, or \code{"energy"}. Pipes 
#'    directly to \code{\link[portalr]{summarize_rodent_data}}.
#'   \item \code{fillweight}: \code{logical} specifier of whether to fill in
#'    unknown weights with other records from that individual or species,
#'    where possible.
#'   \item \code{unknowns}: \code{logical} indicator to either remove all 
#'    individuals not identified to species (\code{unknowns = FALSE}) or 
#'    sum them in an additional column (\code{unknowns = TRUE}).
#'   \item \code{time}: \code{character} value specifying the format of the 
#'    time index in the output. Options are \code{"period"} (sequential 
#'    Portal surveys), \code{"newmoon"} (lunar cycle numbering), and 
#'    \code{"date"} (calendar date). \cr
#'    The default \code{time = "newmoon"} produces an equispaced observation 
#'    timestep, a common format format for discrete-time modeling. 
#'   \item \code{na_drop}: \code{logical} indicator of if \code{NA} values 
#'    (representing insufficient sampling) should be dropped.
#'   \item \code{zero_drop}: \code{logical} indicator of if \code{0} values 
#'    (representing sufficient sampling but no detection) should be dropped.
#'   \item \code{effort}: \code{logical} indicator of if the effort columns 
#'    should be included in the output.
#'   \item \code{filename}: \code{character} name of the file for saving the 
#'    output.
#'  }
#'  If only a single dataset is added, the name of the set from the element
#'  \code{data_set} will be used to name the model's \code{list} in the 
#'  larger \code{list}. If multiple models are added, each element \code{list} 
#'  must be named according to the dataset and the \code{data_set} element.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return Named \code{list} of length equal to the number of elements in 
#'  \code{data_sets} and with elements that are each 
#'  \code{list}s of those \code{data_set}'s data-generating controls, for
#'  input as \code{controls_rodents} in \code{\link{prep_rodents}}.
#'
#' @examples
#'  rodents_controls(c("all", "controls"))
#'  all_PPonly <- rodents_control("all_PPonly", species = "PP")
#'  rodents_controls("all_PPonly", all_PPonly, arg_checks = FALSE)
#'  rodents_controls(c("all", "all_PPonly"), all_PPonly, arg_checks = FALSE)
#'
#' @export
#'
rodents_controls <- function(data_sets = NULL, controls_rodents = NULL, 
                             arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(data_sets)
  if(list_depth(controls_rodents) == 1){
    controls_rodents <- list(controls_rodents)
    names(controls_rodents) <- controls_rodents[[1]]$name
  }
  nadd <- length(controls_rodents)

  prefab_controls <- prefab_rodents_controls()
  nprefab <- length(prefab_controls)
  for(i in 1:nprefab){
    controls_rodents[nadd + i] <- list(prefab_controls[[i]])
    names(controls_rodents)[nadd + i] <- names(prefab_controls)[i]
  }
  included_data <- which(names(controls_rodents) %in% data_sets)
  missing_controls <- which((data_sets %in% names(controls_rodents)) == FALSE)
  replicates <- table(names(controls_rodents))
  if(length(missing_controls) > 0){
    which_missing <- data_sets[missing_controls]
    all_missing <- paste(which_missing, collapse = ", ")
    msg <- paste0("missing controls for dataset(s): ", all_missing)
    stop(msg, call. = FALSE)
  }
  if(any(replicates > 1)){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of dataset(s): ", all_conflicting)
    stop(msg, call. = FALSE)
  }
  controls_rodents[included_data]

}

#' @title Create a control lists for generating a rodents data table
#'
#' @description Given the number of arguments into 
#'  \code{\link[portalr]{summarize_rodent_data}} via 
#'  \code{\link{prep_rodents_table}}, it helps to have a control \code{list}  
#'  to organize them. \code{rodents_control} provides a template for the 
#'  controls \code{list} used to define the specific arguments for a user's
#'  novel data set, setting the formal arguments to the basic default 
#'  values
#
#' @param name \code{character} value of the rodent data set name
#'  used to enforce certain arguments. Currently used prefab data
#'  sets are \code{"all"}, \code{"all_interp"}, \code{"controls"}, 
#'  and \code{"controls_interp"}. 
#'
#' @param species \code{character}-valued vector of species names 
#'  to include.  
#'
#' @param total \code{logical} value indicating if a total 
#'  (sum across species) should be added or not. Only available if more than 
#'  one species is included. 
#'
#' @param interpolate \code{logical} value indicating if the data should be
#'  interpolated to fill in \code{NA} values (using 
#'  \code{\link[forecast]{na.interp}}). 
#'
#' @param clean \code{logical} indicator of if only the rodent data
#'  that passed QA/QC (\code{clean = TRUE}) or if all data 
#'  (\code{clean = FALSE}) should be loaded.
#'
#' @param type \code{character} value of the rodent data set type, according 
#'  to pre-existing definitions. An alternative toggle to \code{species}. \cr
#'  Either all species 
#'  (\code{type = "Rodents"}) or only granivores (\code{type = "Granivores"}). 
#'
#' @param level \code{character} indicating the type of summary:
#'  \code{"Plot"}, \code{"Treatment"}, or \code{"Site"}. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param plots Specification of subset of plots. Can be a vector of 
#'  \code{numeric} plots indicators or specific sets indicated by
#'  \code{character} values: \code{"all"} plots or \code{"Longterm"} plots
#'  (plots that have had the same treatment for the entire time series).
#'
#' @param treatment \code{character} indicating the specific 
#'  treatment(s) to trim to if \code{level = "Treatment"}: \code{"control"},
#'  \code{"exclosure"}, \code{"removal"}, or \code{"spectabs"} 
#'
#' @param min_plots \code{integer} (or integer \code{numeric}) of the 
#'  minimum number of plots surveyed for a survey to be used. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param min_traps \code{integer} (or integer \code{numeric}) of the 
#'  minimum number of traps collected for a plot to be used. Pipes directly
#'  to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param output \code{character} indicating the type of data:
#'  \code{"abundance"}, \code{"biomass"}, or \code{"energy"}. Pipes 
#'  directly to \code{\link[portalr]{summarize_rodent_data}}.
#'
#' @param fillweight \code{logical} specifier of whether to fill in unknown 
#'  weights with other records from that individual or species, where 
#'  possible.
#'
#' @param unknowns \code{logical} indicator to either remove all 
#'  individuals not identified to species (\code{unknowns = FALSE}) or 
#'  sum them in an additional column (\code{unknowns = TRUE}.
#'
#' @param time \code{character} value specifying the format of the time index 
#'  in the output. Options are \code{"period"} (sequential Portal surveys), 
#'  \code{"newmoon"} (lunar cycle numbering), and \code{"date"} (calendar 
#'  date). \cr
#'  The default \code{time = "newmoon"} produces an equispaced observation 
#'  timestep, a common format format for discrete-time modeling. 
#'
#' @param na_drop \code{logical} indicator of if \code{NA} values 
#'  (representing insufficient sampling) should be dropped.
#'
#' @param zero_drop \code{logical} indicator of if \code{0} values 
#'  (representing sufficient sampling but no detection) should be dropped.
#'
#' @param effort \code{logical} indicator of if the effort columns should
#'  be included in the output.
#'
#' @param filename \code{character} name of the file for saving the 
#'  output. The extension of the filename may be used for identifying
#'  object requirements (for example, if the extension of \code{filename} is
#'  \code{".csv"}, it will trigger use of \code{\link[utils]{write.csv}}). 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return Named \code{list} of \code{data_set}'s data-generating controls, 
#'  for input as part of \code{controls_rodents} in 
#'  \code{\link{prep_rodents}}.
#'
#' @examples
#'  rodents_controls(c("all", "controls"))
#'  all_PPonly <- rodents_control("all_PPonly", species = "PP")
#'  rodents_controls("all_PPonly", all_PPonly, arg_checks = FALSE)
#'  rodents_controls(c("all", "all_PPonly"), all_PPonly, arg_checks = FALSE)
#'
#' @export
#'
rodents_control <- function(name = "name", species = base_species(), 
                            total = TRUE, interpolate = FALSE,
                            clean = FALSE, type = "Rodents", 
                            level = "Site", plots = "all", treatment = NULL, 
                            min_plots = 24, min_traps = 1, 
                            output = "abundance",
                            fillweight = (output != "abundance"), 
                            unknowns = FALSE, time = "newmoon", 
                            na_drop = FALSE,
                            zero_drop = switch(tolower(level), 
                                                    plot = FALSE, 
                                                    treatment = TRUE,
                                                    site = TRUE),
                            effort = FALSE,
                            filename = paste0("rodents_", name, ".csv"),
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(name)
  list(name = name, species = species, total = total, 
       interpolate = interpolate, clean = clean, type = type, level = level, 
       plots = plots, treatment = treatment, min_plots = min_plots, 
       min_traps = min_traps, output = output, fillweight = fillweight, 
       unknowns = unknowns, time = time, na_drop = na_drop, 
       zero_drop = zero_drop, effort = effort, filename = filename)
}




#' @title Determine the most recent data collection
#'
#' @description Determine the most recent census.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{Date} of the last census.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   last_census()
#'  }
#'
#' @export
#'
last_census <- function(main = ".", 
                        control_files = files_control(), arg_checks = TRUE){
  check_args(arg_checks)
  moons <- read_moons(main = main, control_files = control_files,
                      arg_checks = arg_checks)
  as.Date(max(moons$censusdate, na.rm = TRUE))
}

#' @title Select the most abundant species from a data set
#'
#' @description Not including the total, determine the most abundant 
#'  rodent species within a data set.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param data_set \code{character} representation of the grouping
#'  name used to define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @param topx Positive \code{integer} (or \code{integer}-conformable) 
#'  value of how many species to include.
#'  
#' @return \code{character} vector of the species identifiers.
#' 
#' @examples
#' \donttest{
#'  setup_dir()
#'  most_abundant_species()
#' }
#'
#' @export
#'
most_abundant_species <- function(main = ".", data_set = "all", topx = 3,
                                  arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  rodents_tab <- read_rodents_table(main = main, data_set = data_set, 
                                    arg_checks = arg_checks)
  col_keep <- which(colnames(rodents_tab) %in% all_species())
  rodents_tab <- rodents_tab[ , col_keep]
  tots <- apply(rodents_tab, 2, sum, na.rm = TRUE)
  tots_order <- order(tots)
  names(tots) <- colnames(rodents_tab)
  names(sort(tots, decreasing = TRUE)[1:topx])
}

