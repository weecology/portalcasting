#' @title Prepare Rodents Data for Forecasting
#'
#' @description `prepare_dataset` is the workhorse function for creating portalcasting rodent datasets using existing functions. \cr 
#'              Wraps around [`portalr::summarize_rodent_data`] to produce a `data.frame` associated with a set of data specifications. Inputs are ready for implementation via [`prepare_rodents`]. \cr
#'              `prepare_rodents` creates specified `datasets` using their associated function (typically `prepare_dataset`) and arguments, according to the [`directory_settings`]. \cr 
#'              `prepare_abundance` creates a model-ready vector of abundances for fitting and casting, according to the model's requirements and time settings.
#'
#' @param name `character` name to be given to the dataset.
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param model `character` value of the model name.
#'
#' @param dataset,datasets `character` value(s) of name(s) of rodent dataset(s) to include.
#'
#' @param new_datasets_controls Optional `list` of controls for new datasets. See [`datasets_controls`].
#'
#' @param species `character`-valued vector of species names to include. 
#'
#' @param total `logical` value indicating if a total (sum across species should be added or not. Only available if more than one species is included. 
#'
#' @param clean `logical` indicator of if only the rodent data that passed QA/QC (`clean = TRUE`) or if all data (`clean = FALSE`) should be loaded.
#'
#' @param type `character` value of the rodent data set type, according to pre-existing definitions. An alternative toggle to `species`. \cr \cr
#'             Either all species (`type = "Rodents"`) or only granivoes (`type = "Granivores"`). 
#'
#' @param level `character` indicating the type of summary: `"Plot"`, `"Treatment"`, or `"Site"`. Pipes directly to [`portalr::summarize_rodent_data`].
#'
#' @param plots Specification of subset of plots. Can be a vector of `numeric` plots indicators or specific sets indicated by `character` values: `"all"` plots or `"Longterm"` plots (plots that have had the same treatment for the entire time series).
#'
#' @param treatment `character` indicating the specific treatment(s) to trim to if `level = "Treatment"`: `"control"`, `"exclosure"`, `"removal"`, or `"spectabs"` 
#'
#' @param min_plots `integer` (or integer `numeric`) of the minimum number of plots surveyed for a survey to be used. 
#'
#' @param min_traps `integer` (or integer `numeric`) of the minimum number of traps collected for a plot to be used.
#'
#' @param output `character` indicating the type of data: `"abundance"`, `"biomass"`, or `"energy"`. 
#'
#' @param fillweight `logical` specifier of whether to fill in unknown weights with other records from that individual or species, where possible.
#'
#' @param shape `character` value indicating a "crosstab" or "flat" output. 
#'
#' @param unknowns `logical` indicator to either remove all individuals not identified to species (`unknowns = FALSE`) or sum them in an additional column (`unknowns = TRUE`.
#'
#' @param time `character` value specifying the format of the time index in the output. Options are `"period"` (sequential Portal surveys), `"newmoon"` (lunar cycle numbering), and `"date"` (calendar date). \cr 
#'             The default `time = "newmoon"` produces an equispaced observation timestep, a common format format for discrete-time modeling. 
#'
#' @param na_drop `logical` indicator of if `NA` values (representing insufficient sampling) should be dropped. 
#'
#' @param zero_drop `logical` indicator of if `0` values (representing sufficient sampling but no detection) should be dropped.
#'
#' @param effort `logical` indicator of if the effort columns should be included in the output.
#'
#' @param save `logical` indicator controlling if the output should be saved out.
#'
#' @param filename `character` value of the file for saving the output.
#'
#' @return `prepare_dataset`: `data.frame` for the specified dataset. \cr
#'         `prepare_rodents`: `list` of `data.frame`s for the specified datasets. \cr
#'         `prepare_abundance`: `numeric` vector of abundance data corresponding to the time articulated in the metadata file. Missing values are interpolated if requested via the model controls. \cr
#'         `read_datasets_controls`, `write_datasets_controls`, `datasets_controls`: `list` of `datasets`' control `list`s, [`invisible`][base::invisible]-ly for `write_datasets_controls`.
#'
#' @name prepare rodents
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "rodents")
#'
#'    create_dir(main = main1)
#'    fill_resources(main = main1)
#'    fill_forecasts(main = main1)
#'    fill_fits(main = main1)
#'    fill_models(main = main1)
#'
#'    prepare_newmoons(main   = main1)
#'    prepare_rodents(main    = main1) 
#'
#'    write_dataset_controls(main = main1)
#'    read_dataset_controls(main = main1)
#'    dataset_controls(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL


#' @rdname prepare-rodents
#'
#' @export
#'
prepare_abundance <- function (main    = ".", 
                               dataset = NULL,
                               species = NULL,
                               model   = NULL) {

  return_if_null(x = dataset)
  return_if_null(x = model)
  return_if_null(x = species)

  settings <- read_directory_settings(main = main)

  model_controls <- models_controls(main   = main, 
                                    models = model)[[model]]
  metadata       <- read_metadata(main         = main)
  rodents_table  <- read_rodents_dataset(main    = main, 
                                          dataset = dataset)

  moon_in       <- rodents_table$newmoonnumber %in% metadata$time$historic_newmoonnumbers
  species_in    <- colnames(rodents_table) == species
  out           <- rodents_table[moon_in, species_in]


  if (model_controls$interpolate$needed) {

   out <- do.call(what = model_controls$interpolate$fun,
                  args = update_list(list(x = out), model_controls$interpolate$args))

  } 

  out 
}

#' @rdname prepare-rodents
#'
#' @export
#'
prepare_rodents <- function (main                  = ".",
                             datasets              = prefab_datasets( ),
                             new_datasets_controls = NULL) {

  return_if_null(x = datasets)
  settings <- read_directory_settings(main = main)

  datasets_controls_list <- write_datasets_controls(main                  = main, 
                                                    datasets              = datasets, 
                                                    new_datasets_controls = new_datasets_controls)

  messageq("  - rodents", quiet = settings$quiet)

  out <- named_null_list(element_names = datasets)

  for (i in 1:length(datasets_controls_list)) {

    out[[i]] <- do.call(what = datasets_controls_list[[i]]$fun, 
                        args = update_list(list = datasets_controls_list[[i]]$args, 
                                           main = main))
  
  }

  invisible(out)

}


#' @rdname prepare-rodents
#'
#' @export
#'
prepare_dataset <- function(name       = "all",
                            main       = ".",
                            filename   = "rodents_all.csv",
                            clean      = FALSE,
                            level      = "Site",
                            type       = "Rodents",
                            plots      = "all",
                            unknowns   = FALSE,
                            shape      = "crosstab",
                            time       = "newmoon",
                            output     = "abundance",
                            fillweight = FALSE,
                            treatment  = NULL,
                            na_drop    = FALSE,
                            zero_drop  = FALSE,
                            min_traps  = 1,
                            min_plots  = 24,
                            effort     = TRUE,
                            species    = forecasting_species(),
                            total      = TRUE,
                            save       = TRUE) {

  return_if_null(x = name)

  settings <- read_directory_settings(main = main)

  messageq("    - ", name, quiet = settings$quiet)

  rodents_table <- summarize_rodent_data(path       = resources_path(main = main), 
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
                                         quiet      = !settings$verbose) 


  sp_col           <- colnames(rodents_table) %in% rodent_species(path = resources_path(main = main), set = "forecasting", type = "code", total = FALSE)
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
    cols_in <- tolower(colnames(out)) != "treatment"
    out     <- out[rows_in, cols_in, drop = FALSE]

  }

  # patch to deal with incomplete tables at the treatment-level
  if (any(diff(out$newmoonnumber) > 1)) {
    na_row         <- out[1, ]
    na_row[ , ]    <- NA
    all_newmoons   <- min(out$newmoonnumber):max(out$newmoonnumber)
    miss_newmoons  <- all_newmoons[!(all_newmoons %in% out$newmoonnumber)]
    nmiss_newmoons <- length(miss_newmoons)
    for (i in 1:nmiss_newmoons) {
      na_row_i               <- na_row
      na_row_i$newmoonnumber <- miss_newmoons[i]
      out                    <- rbind(out, na_row_i)
    } 
    out <- out[order(out$newmoonnumber), ]
  }
  # end patch

  newmoons <- read_newmoons(main = main)
 
# dont cut them out of the actual data tho... 
#  rows_in <- out$newmoonnumber >= min(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$timeseries_start_lagged >= 0)]) & 
#             out$newmoonnumber <= max(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$origin < 0)])
#  out <- out[rows_in, ]

  ### patch to verify the correct moons are in ###
    needed_newmoonnumbers  <- newmoons$newmoonnumber[newmoons$newmoondate < settings$time$origin]
    missing_newmoonnumbers <- needed_newmoonnumbers[!(needed_newmoonnumbers  %in% out$newmoonnumber)]
    nmissing_newmoons      <- length(missing_newmoonnumbers)
    if (nmissing_newmoons > 0) {
      for (i in 1:nmissing_newmoons) {
        row_i <- c(missing_newmoonnumbers[i], rep(NA, ncol(out) - 1))
        out   <- rbind(out,
                       row_i)
      }
    }
  ### end patch ###

  write_data(x            = out, 
             main         = main, 
             subdirectory = settings$subdirectories$data,
             save         = save, 
             overwrite    = settings$overwrite, 
             filename     = filename, 
             quiet        = !settings$verbose)

}



#' @rdname prepare-rodents
#'
#' @export
#'
read_datasets_controls <- function (main = ".") {

  settings <- read_directory_settings(main = main)
  read_yaml(file.path(main, settings$subdirectories$data, settings$files$datasets_controls))

}


#' @rdname prepare-rodents
#'
#' @export
#'
datasets_controls <- function (main     = ".",
                               datasets = prefab_datasets( )) {

  read_datasets_controls(main = main)[datasets]

}


#' @rdname prepare-rodents
#'
#' @export
#'
write_datasets_controls <- function (main                  = ".",
                                     new_datasets_controls = NULL,
                                     datasets              = prefab_datasets( )) {

  settings <- read_directory_settings(main = main)

  messageq("Writing dataset controls ...", quiet = settings$quiet)

  datasets_controls <- c(prefab_datasets_controls( ), new_datasets_controls)[datasets]

  write_yaml(x    = datasets_controls,
             file = rodents_datasets_controls_path(main = main))

  messageq(" ... complete.\n", quiet = settings$quiet)

  invisible(datasets_controls)

}


