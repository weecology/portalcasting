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

  rodent_dataset_controls <- prefab_rodent_dataset_controls()

  #
  # need a way here to access user-generated control lists  
  #

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
#' @param filename \code{character} value of the file for saving the output.
#'
#' @return \code{data.frame} for the specified data set.
#'
#' @export
#'
prep_rodent_dataset <- function(name        = "all",
                                main        = ".",
                                filename    = "rodents_all.csv",
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

  rodents_table <- summarize_rodent_data(path       = file.path(main, "raw"), 
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
             filename  = filename, 
             overwrite = overwrite, 
             quiet     = !verbose)

  out

}


#' @title Determine the most recent data collection
#'
#' @description Determine the most recent census.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param control_files \code{list} of names of the folders and files within the sub directories and saving strategies (save, overwrite, append, etc.). Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be checked using standard protocols via \code{\link{check_args}}. The default (\code{arg_checks = TRUE}) ensures that all inputs are formatted correctly and provides directed error messages if not. 
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
last_census <- function (main          = ".", 
                         control_files = files_control(), 
                         arg_checks    = TRUE) {

  check_args(arg_checks)
  moons <- read_moons(main = main, control_files = control_files,
                      arg_checks = arg_checks)
  as.Date(max(moons$censusdate, na.rm = TRUE))
}


