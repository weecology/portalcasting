# produces a model-ready vector
prepare_rodent_abundance <- function (main     = ".", 
                                      dataset  = NULL,
                                      species  = NULL,
                                      model    = NULL,
                                      settings = directory_settings( ), 
                                      quiet    = FALSE, 
                                      verbose  = FALSE) {

  metadata <- read_metadata(main     = main,
                            settings = settings)
  rodents_table <- read_rodents_table(main     = main, 
                                      dataset  = dataset,
                                      settings = settings)
  moon_in          <- rodents_table$newmoonnumber %in% metadata$time$historic_newmoons
  species_in <- colnames(rodents_table) == gsub("NA", "NA.", species)
    out <- rodents_table[moon_in, species_in]

  model_controls <- read_model_controls(main = main, settings = settings)[[model]]

  if (model_controls$interpolate$needed) {

   out <- do.call(what = model_controls$interpolate$fun,
                  args = list(x = out))

  } 

  out 
}




#' @title Read and Write Rodent Dataset Control Lists
#'
#' @description Input/Output functions for dataset control lists.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param datasets \code{character} vector of name(s) of rodent dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param new_dataset_controls \code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets} that are to be added to the control list and file.
#'
#' @return \code{list} of \code{datasets}' control \code{list}s, \code{\link[base]{invisible}}-ly for \code{write_dataset_controls}.
#'  
#' @name read and write rodent dataset controls
#'
#' @export
#'
read_dataset_controls <- function (main     = ".",
                                   settings = directory_settings( )) {

  read_yaml(file.path(main, settings$subdirectories$data, settings$files$dataset_controls))

}

#' @rdname read-and-write-rodent-dataset-controls
#'
#' @export
#'
dataset_controls <- function (main     = ".",
                              datasets = prefab_datasets( ),
                              settings = directory_settings( )) {

  read_dataset_controls(main     = main,
                        settings = settings)[datasets]

}

#' @rdname read-and-write-rodent-dataset-controls
#'
#' @export
#'
write_dataset_controls <- function (main                 = ".",
                                    new_dataset_controls = NULL,
                                    datasets             = prefab_datasets( ),
                                    settings             = directory_settings( ),
                                    quiet                = FALSE) {

  dataset_controls <- prefab_dataset_controls()
  ndatasets        <- length(dataset_controls)
  nnew_datasets    <- length(new_dataset_controls)

  if (nnew_datasets > 0) {

    for (i in 1:nnew_datasets) {

      dataset_controls <- update_list(dataset_controls, 
                                             x = new_dataset_controls[[i]])

      names(dataset_controls)[ndatasets + i] <- names(new_dataset_controls)[i]

    }

  }

  write_yaml(x    = dataset_controls,
             file = file.path(main, settings$subdirectories$data, settings$files$dataset_controls))

  invisible(dataset_controls)

}


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
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{list} of prepared \code{datasets}.
#'  
#' @name prepare rodents
#'
#' @export
#'
prepare_rodents <- function (main                 = ".",
                             datasets             = prefab_datasets( ),
                             new_dataset_controls = NULL,
                             settings             = directory_settings( ), 
                             quiet                = FALSE,
                             verbose              = FALSE) {

  return_if_null(datasets)

  dataset_controls_list <- write_dataset_controls(main                 = main, 
                                                  settings             = settings, 
                                                  datasets             = datasets, 
                                                  new_dataset_controls = new_dataset_controls,
                                                  quiet                = quiet)

  messageq("  - rodents", quiet = quiet)

  out <- named_null_list(element_names = datasets)

  for (i in 1:length(dataset_controls_list)) {

    out[[i]] <- do.call(what = dataset_controls_list[[i]]$fun, 
                        args = update_list(list             = dataset_controls_list[[i]]$args, 
                                           main             = main, 
                                           settings         = settings, 
                                           quiet            = quiet, 
                                           verbose          = verbose))
  
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
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param species \code{character}-valued vector of species names to include. 
#'
#' @param total \code{logical} value indicating if a total (sum across species should be added or not. Only available if more than one species is included. 
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
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param filename \code{character} value of the file for saving the output.
#'
#' @return \code{data.frame} for the specified data set.
#'
#' @name prepare rodent dataset
#'
#' @export
#'
prepare_dataset <- function(name             = "all",
                            main             = ".",
                            settings         = directory_settings( ),
                            filename         = "rodents_all.csv",
                            clean            = FALSE,
                            level            = "Site",
                            type             = "Rodents",
                            plots            = "all",
                            unknowns         = FALSE,
                            shape            = "crosstab",
                            time             = "newmoon",
                            output           = "abundance",
                            fillweight       = FALSE,
                            treatment        = NULL,
                            na_drop          = FALSE,
                            zero_drop        = FALSE,
                            min_traps        = 1,
                            min_plots        = 24,
                            effort           = TRUE,
                            species          = base_species(),
                            total            = TRUE,
                            save             = TRUE,
                            quiet            = FALSE,
                            verbose          = FALSE) {

  return_if_null(name)

  messageq("    - ", name, quiet = quiet)

  rodents_table <- summarize_rodent_data(path       = file.path(main, settings$subdirectories$resources), 
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


  sp_col           <- colnames(rodents_table) %in% all_species( )
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

  newmoons <- read_newmoons(main     = main, 
                            settings = settings)
  
  rows_in <- out$newmoonnumber >= min(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$timeseries_start_lagged >= 0)]) & 
             out$newmoonnumber <= max(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$origin < 0)])

  out <- out[rows_in, ]


  write_data(x         = out, 
             main      = main, 
             data_sub  = settings$subdirectories$data,
             save      = save, 
             filename  = filename, 
             quiet     = !verbose)

}




