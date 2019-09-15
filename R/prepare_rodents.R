prefab_rodents_controls <- function(){
  list(
    "all" = list(name = "all", species = base_species(), total = TRUE,
                 interpolate = FALSE, clean = FALSE, type = "Rodents", 
                 level = "Site", plots = "all", treatment = NULL, 
                 min_plots = 24, min_traps = 1, output = "abundance", 
                 fillweight = FALSE, unknowns = FALSE, time = "newmoon", 
                 na_drop = FALSE, zero_drop = TRUE, effort = FALSE,
                 filename = "rodents_all.csv"),
    "all_interp" = list(name = "all_interp", species = base_species(), 
                        total = TRUE, interpolate = TRUE, clean = FALSE, 
                        type = "Rodents", level = "Site", plots = "all", 
                        treatment = NULL, min_plots = 24, min_traps = 1,
                        output = "abundance", fillweight = FALSE, 
                        unknowns = FALSE, time = "newmoon", na_drop = FALSE, 
                        zero_drop = TRUE, effort = FALSE, 
                        filename = "rodents_all_interp.csv"),
    "controls" = list(name = "controls", species = base_species(), 
                      total = TRUE, interpolate = FALSE, clean = FALSE, 
                      type = "Rodents", level = "Treatment", 
                      plots = "Longterm", treatment = "control", 
                      min_plots = 24, min_traps = 1, output = "abundance", 
                      fillweight = FALSE, unknowns = FALSE, time = "newmoon", 
                      na_drop = FALSE, zero_drop = TRUE, effort = FALSE, 
                      filename = "rodents_controls.csv"),
    "controls_interp" = list(name = "controls_interp", 
                             species = base_species(), 
                             total = TRUE, interpolate = TRUE, clean = FALSE, 
                             type = "Rodents", level = "Treatment", 
                             plots = "Longterm", treatment = "control", 
                             min_plots = 24, min_traps = 1, 
                             output = "abundance", fillweight = FALSE, 
                             unknowns = FALSE, time = "newmoon", 
                             na_drop = FALSE, zero_drop = TRUE, 
                             effort = FALSE, 
                             filename = "rodents_controls_interp.csv")
   )
}

#' @title Create control lists for generating rodents data tables
#'
#' @description Given the number of arguments into 
#'  \code{\link[portalr]{summarize_rodent_data}} via 
#'  \code{\link{prep_rodents_table}}, it helps to have a control \code{list}  
#'  to organize them. \cr \cr
#'  \code{rodents_controls} produces the \code{list}s for any user-requested
#'  datasets from the prefab datasets (\code{\link{prefab_data_sets}}) and any 
#'  user-defined sets. \code{\link{rodents_control}} provides a template
#'  for the controls \code{list} which can be used to define the specific 
#'  arguments for a user's novel data set. 
#'
#' @details Any data set that is part of the \code{prefab} rodents data sets 
#'  (\code{c("all", "controls", "all_interp", "controls_interp")}) has its 
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
#'  sets are \code{"all"}, \code{"all_interp"}, \code{"controls"} and
#'  \code{"controls_interp"}. 
#'
#' @param controls_rodents Additional controls for datasets not in the 
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
    stop(msg)
  }
  if(any(replicates > 1)){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of dataset(s): ", all_conflicting)
    stop(msg)
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
#'  (\code{type = "Rodents"}) or only granivoes (\code{type = "Granivores"}). 
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



#' @title Prepare a list of rodents data tables for forecasting
#'
#' @description This function loops around \code{\link{prep_rodents_table}},
#'  which wraps around \code{\link[portalr]{summarize_rodent_data}} to 
#'  produce a list of rodents \code{data.frame}s associated with multiple
#'  sets of specifications. The default settings are for a general 
#'  portalcasting directory, which models the "all" and "controls" data 
#'  subsets.
#'
#' @param controls_rodents Control \code{list} or \code{list} of control 
#'  \code{list}s from \code{\link{rodents_controls}} specifying the 
#'  structuring of the rodents tables. See \code{\link{rodents_controls}}. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#
#' @param data_sets \code{character} values of the treatment 
#'  types. If left as \code{NULL} (default), it is replaced with
#'  the output from \code{\link{prefab_data_sets}}. \cr
#'  Used to set arguments for \code{\link{prep_rodents_table}}.
#'
#' @param ref_species \code{character}-valued vector of all possible species 
#'  names that could be used. Should be set up via \code{\link{all_species}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{list} of \code{data.frame}s, one for each data subset
#'  specified, as generated by \code{\link{prep_rodents_table}}.
#'
#' @examples
#'  \donttest{
#'    create_dir()
#'    fill_raw()
#'    prep_rodents()
#'  }
#'
#' @export
#'
prep_rodents <- function(main = ".", moons = NULL, data_sets = NULL,
                         end_moon = NULL, start_moon = 217,
                         controls_rodents = NULL, 
                         control_files = files_control(), 
                         ref_species = all_species(), quiet = TRUE,
                         verbose = FALSE,  arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  data_sets <- ifnull(data_sets, prefab_data_sets())
  messageq("  -rodents data files", quiet)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  controls_rodents <- rodents_controls(data_sets = data_sets, 
                                 controls_rodents = controls_rodents, 
                                 arg_checks = arg_checks)
  rodents <- named_null_list(data_sets)
  ndata_sets <- length(rodents)
  for(i in 1:ndata_sets){
    ctrl_r_i <- controls_rodents[[data_sets[i]]]
    messageq(paste0("   -", data_sets[i]), quiet)


    rodents[[i]] <- prep_rodents_table(main = main, moons = moons, 
                                       end_moon = end_moon, 
                                       start_moon = start_moon, 
                                       species = ctrl_r_i[["species"]],
                                       total = ctrl_r_i[["total"]],
                                       interpolate = 
                                         ctrl_r_i[["interpolate"]],
                                       clean = ctrl_r_i[["clean"]],
                                       type = ctrl_r_i[["type"]],
                                       level = ctrl_r_i[["level"]],
                                       plots = ctrl_r_i[["plots"]],
                                       treatment = ctrl_r_i[["treatment"]],
                                       min_plots = ctrl_r_i[["min_plots"]],
                                       min_traps = ctrl_r_i[["min_traps"]],
                                       output = ctrl_r_i[["output"]],
                                       fillweight = ctrl_r_i[["fillweight"]],
                                       unknowns = ctrl_r_i[["unknowns"]],
                                       time = ctrl_r_i[["time"]],
                                       na_drop = ctrl_r_i[["na_drop"]],
                                       zero_drop = ctrl_r_i[["zero_drop"]],
                                       ref_species = ref_species,
                                       effort = ctrl_r_i[["effort"]],
                                       quiet = quiet,
                                       verbose = verbose,
                                       save = control_files$save,
                                       overwrite = control_files$overwrite,
                                       filename = ctrl_r_i[["filename"]],
                                       arg_checks = arg_checks)
  }
  rodents
}


#' @title Prepare a rodents data tables for forecasting
#'
#' @description This function wraps around 
#'  \code{\link[portalr]{summarize_rodent_data}} to 
#'  produce a \code{data.frame}s associated with a set of data 
#'  specifications. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param species \code{character}-valued vector of species names 
#'  to include. 
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}. 
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
#'  (\code{type = "Rodents"}) or only granivoes (\code{type = "Granivores"}). 
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
#' @param shape \code{character} value indicating a "crosstab" or "flat" 
#'  output. 
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
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
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param save \code{logical} indicator controlling if the output should 
#'  be saved out.
#'
#' @param ref_species \code{character} vector of the reference species. Sets
#'  a boundary outside of the species included in the model.
#'
#' @param rodents_tab Rodents \code{data.frame} data table.
#'
#' @param filename \code{character} name of the file for saving the 
#'  output. The extension of the filename may be used for identifying
#'  object requirements (for example, if the extension of \code{filename} is
#'  \code{".csv"}, it will trigger use of \code{\link[utils]{write.csv}}).
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}. 
#'  Only used here to point to the moons data.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}. 
#'
#' @return \code{data.frame} for the specified data set.
#'
#' @examples
#'  \donttest{
#'    create_dir()
#'    fill_raw()
#'    prep_rodents_table()
#'  }
#'
#' @name prepare_rodents_table
#'
NULL

#' @rdname prepare_rodents_table
#'
#' @export
#'
prep_rodents_table <- function(main = ".", moons = NULL,
                               end_moon = NULL, start_moon = 217, 
                               species = base_species(), total = TRUE, 
                               interpolate = TRUE,
                               clean = FALSE, type = "Rodents", 
                               level = "Site", plots = "all", 
                               treatment = NULL,
                               min_plots = 24, min_traps = 1, 
                               output = "abundance", shape = "crosstab",
                               fillweight = (output != "abundance"),
                               unknowns = FALSE, time = "newmoon",
                               na_drop = FALSE, 
                               zero_drop = switch(tolower(level), 
                                                  plot = FALSE, 
                                                  treatment = TRUE,
                                                  site = TRUE),
                               ref_species = all_species(),
                               effort = FALSE,  quiet = TRUE, 
                               verbose = FALSE,
                               control_files = files_control(),
                               save = TRUE, overwrite = TRUE, 
                               filename = "rodents_all.csv", 
                               arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(species) 
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  raw_path <- raw_path(main = main, arg_checks = arg_checks)
  summarize_rodent_data(path = raw_path, clean = clean, level = level,
                        type = type, plots = plots, unknowns = unknowns,
                        shape = shape, time = time, output = output,
                        fillweight = fillweight, na_drop = na_drop,
                        zero_drop = zero_drop, min_traps = min_traps,
                        min_plots = min_plots, effort = effort, 
                        quiet = !verbose) %>%
  process_rodent_data(main = main, moons = moons, end_moon = end_moon,
                      start_moon = start_moon, species = species, 
                      total = total, interpolate = interpolate, 
                      clean = clean, level = level, 
                      treatment = treatment, na_drop = na_drop,
                      time = time, ref_species = ref_species, quiet = quiet, 
                      verbose = verbose, arg_checks = arg_checks) %>%
  write_data(main = main, save = save, filename = filename, 
             overwrite = overwrite, quiet = !verbose, arg_checks = arg_checks)
}

#' @rdname prepare_rodents_table
#'
#' @export
#'
process_rodent_data <- function(rodents_tab, main = ".", moons = NULL,
                                end_moon = NULL, start_moon = 217, 
                                species = base_species(), 
                                total = TRUE, interpolate = FALSE,
                                clean = FALSE, level = "Site", 
                                treatment = NULL, na_drop = FALSE,
                                time = "newmoon",
                                ref_species = all_species(),
                                quiet = TRUE, verbose = FALSE,
                                arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  nspecies <- length(species)
  total <- ifelse(nspecies == 1, FALSE, total)
  drop_species <- ref_species[which(ref_species %in% species == FALSE)]
  if(length(drop_species) > 0){
    rodents_tab <- select(rodents_tab, -one_of(drop_species))
    spp <- paste(drop_species, collapse = ", ")
    msg <- paste0("    removing species: ", spp)
    messageq(msg, !verbose)
  }
  if(total){
    spp_col <- is_sp_col(rodents_tab = rodents_tab, species = ref_species,
                         arg_checks = arg_checks)
    total_count <- rowSums(rodents_tab[ , spp_col])
    rodents_tab <- mutate(rodents_tab, total = total_count)
    messageq("    adding total column", !verbose)
  }
  if(level == "Treatment"){
    rodents_tab <- filter(rodents_tab, treatment == !!treatment)  %>%
                   select(-treatment)
  }
  if(time == "newmoon"){
    newmoon_col <- which(colnames(rodents_tab) == "newmoonnumber")
    colnames(rodents_tab)[newmoon_col] <- "moon" 
    if(!na_drop){ 
      present_moons <- rodents_tab[ , "moon"]
      range_moons <- range(present_moons, na.rm = TRUE)
      seq_moons <- seq(range_moons[1], range_moons[2], by = 1)
      needed_moons <- !(seq_moons %in% present_moons)
      which_needed_moons <- seq_moons[needed_moons]
      nneeded_rows <- length(which_needed_moons)
      ndf_moons <- ncol(rodents_tab)
      needed_rows <- matrix(NA, nrow = nneeded_rows, ncol = ndf_moons)
      colnames(needed_rows) <- colnames(rodents_tab)
      needed_rows[ , "moon"] <- seq_moons[needed_moons]
      rodents_tab <- rbind(rodents_tab, needed_rows)
      rodents_tab_ord <- order(rodents_tab[ , "moon"])
      rodents_tab <- rodents_tab[rodents_tab_ord, ]
    }
    if(interpolate){
      for(i in 1:nspecies){
        interped <- round(na.interp(rodents_tab[ , species[i]]))
        rodents_tab[ , species[i]] <- interped

      }
      rodents_tab[ , "total"] <- apply(rodents_tab[ , species], 1, sum)
    }

    return_if_null(c(start_moon, end_moon), rodents_tab)
    start_moon <- ifnull(start_moon, min(rodents_tab$moon))
    end_moon <- ifnull(end_moon, max(rodents_tab$moon))
    rodents_tab <- subset(rodents_tab, moon >= start_moon) %>%
                          subset(moon <= min(c(end_moon, max(moon))))
    messageq("    data trimmed according to moon window", !verbose)
  } else{
    messageq("    no time processing conducted", !verbose)
  }
  rodents_tab
}

#' @title Determine if columns in a table are species columns
#'
#' @description Given a table, returns a \code{logical} vector indicating
#'  if each column is a species' column or not.
#'
#' @param rodents_tab \code{data.frame} of columns to be checked. 
#'
#' @param species \code{character} vector indicating species names to 
#'  use in determining if columns are species columns. Defaults to
#'  \code{\link{all_species}}.
#'
#' @param total \code{logical} indicator if the total should be included.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param nadot \code{logical} indicator if the dot should be added to the 
#'   \code{"NA"} species name. Defaults to \code{FALSE}.
#'
#' @return 
#'  \code{is_sp_col}: \code{logical} vector indicating if each column is a 
#'  species' column or not. \cr \cr
#'  \code{species_from_table}: \code{character} vector of species names from
#'  the column names in the \code{rodents_tab} data table. 
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
is_sp_col <- function(rodents_tab, species = NULL, total = FALSE, 
                      nadot = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  species <- ifnull(species, all_species(total = total, nadot = nadot))
  colnames(rodents_tab) %in% species
}

#' @rdname species_in_tables
#'
#' @export
#'
species_from_table <- function(rodents_tab = NULL, total = FALSE, 
                               nadot = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  sp_col <- is_sp_col(rodents_tab = rodents_tab, total = total, nadot = nadot,
                      arg_checks = arg_checks)
  colnames(rodents_tab)[sp_col]
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
                           total = FALSE, arg_checks = TRUE){
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
all_species <- function(species = NULL, nadot = FALSE, total = FALSE,
                        arg_checks = TRUE){
  rodent_species(species, set = "all", nadot, total)
}


#' @rdname rodent_species
#'
#' @export
#'
base_species <- function(species = NULL, nadot = FALSE, total = FALSE,
                         arg_checks = TRUE){
  rodent_species(species, "base", nadot, total)
}

#' @rdname rodent_species
#'
#' @export
#'
evalplot_species <- function(species = NULL, nadot = FALSE, total = TRUE,
                             arg_checks = TRUE){
  rodent_species(species, "evalplot", nadot, total)
}




#' @title Provide the names of the prefab data sets
#'
#' @description Create a \code{character} vector of the names of the rodents
#'  data sets to be included including the pre-fabricated (prefab) data sets,
#'  extracting them from the \code{prefab_rodents_controls} internal 
#'  function. \cr \cr
#'  The currently prefabricated options include (\code{"all"}, 
#'  \code{"controls"}, \code{"all_interp"}, and \code{"controls_interp"}). 
#'
#' @param interpolate \code{logical} value indicating if the interpolated data
#'  only should be listed (\code{interpolate = TRUE}), if the non-interpolated
#'  data only should be listed (\code{interpolate = FALSE}), or if both
#'  should be listed (\code{interpolate = NULL}, default).  
#'
#' @return \code{character} vector of data set names.
#'
#' @examples
#'  prefab_data_sets()
#'
#' @export
#'
prefab_data_sets <- function(interpolate = NULL){
  dsnames <- names(prefab_rodents_controls())
  return_if_null(interpolate, dsnames)
  if(interpolate){
    dsnames <- dsnames[grepl("_interp", dsnames)]
  }else{
    dsnames <- dsnames[!grepl("_interp", dsnames)]
  }
  dsnames
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
  col_drop <- which(colnames(rodents_tab) %in% c("moon", "total"))
  rodents_tab <- rodents_tab[ , -col_drop]
  tots <- apply(rodents_tab, 2, sum, na.rm = TRUE)
  tots_order <- order(tots)
  names(tots) <- colnames(rodents_tab)
  names(sort(tots, decreasing = TRUE)[1:topx])
}

