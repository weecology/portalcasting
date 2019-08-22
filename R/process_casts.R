#' @title Select the most abundant species from a forecast or hindcast
#'
#' @description Given a forecast or hindcast, determine the most abundant
#'  species predicted. Currently only reliable for forecasts.
#'
#' @param topx \code{integer}-conformable numeric value for the top number of
#'  species to select.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param species \code{character} vector of the species codes (or 
#'  \code{"total"} for the total across species) to be selected from or 
#'  \code{NULL} to include all species and the total.
#'
#' @param tmnt_type \code{character} value of the level of interest 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'  to select the file in the predictions subdirectory. Currently only 
#'  reliably coded for \code{"forecast"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'  file in the predictions subdirectory. If \code{NULL} (default), the
#'  most recently made -cast is selected. 
#'
#' @param model \code{character} value of the name (or \code{"Ensemble"}) of
#'  the model to be plotted.
#'
#' @param lead \code{integer}-conformable lead of the newmoon number used to
#'  select the data plotted. 
#'
#' @param from_date \code{Date} to be used as a reference of when to count
#'  the \code{lead} from. If \code{NULL} (default), for 
#'  \code{cast_type = "forecast"}, \code{from_date = cast_date} and 
#'  \code{plot_cast_point} is not yet reliable for 
#'  \code{cast_type = "hindcast"}.
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
#' @return \code{character} vector of length \code{topx} of the species codes
#'  (see \code{\link{rodent_species}}) of the selected species.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   select_most_ab_spp()
#'  }
#'
#' @export
#'
select_most_ab_spp <- function(main = ".", topx = 3, 
                               species = base_species(),
                               tmnt_type = "Controls", 
                               cast_type = "forecasts", 
                               cast_date = NULL, model = "Ensemble", 
                               lead = 1, from_date = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  if (is.null(cast_date)){
    cast_date <- most_recent_cast(main, cast_type)
  }
  if (is.null(from_date)){
    if(cast_type %in% c("forecast", "forecasts")){
      from_date <- cast_date
    }
  }
  metadata <- read_data(main, "metadata")
  obs <- read_rodents_table(main, tolower(tmnt_type))
  moons <- read_data(main, "moons")
  nmdates <- as.Date(as.character(moons$newmoondate))
  most_recent_nm_spot <- max(which(nmdates <= from_date))
  most_recent_nm_number <- moons$newmoonnumber[most_recent_nm_spot]
  cast_nms_io <- metadata$rodent_cast_newmoons > most_recent_nm_number
  to_include <- which(cast_nms_io)[lead]
  newmoonnumbers <- metadata$rodent_cast_newmoons[to_include]
  pred <- read_cast(main, cast_type = cast_type, cast_date = cast_date) %>%
          select_casts(species = species, tmnt_types = tmnt_type, 
                       models = model, target_moons = newmoonnumbers)  
  pred$species[order(pred$estimate, decreasing = TRUE)[1:topx]]
}

#' @title Append the observed values to a table of -casts
#'
#' @description Add a column of observed values and optionally raw error,
#'  in-prediction-window logical indicator, and lead time columns. Additional
#'  columns are added by default.
#'
#' @param casts \code{data.frame} of requested fore- or hindcasts to have 
#'  observations added to.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param add_error \code{logical} indicator if the \code{error} column should
#'  be added to the output as well. 
#'
#' @param add_in_window \code{logical} indicator if the \code{in_window} 
#'  column should be added to the output as well. 
#'
#' @param add_lead \code{logical} indicator if the \code{lead} column should
#'  be added to the output as well. 
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
#' @return \code{casts} \code{data.frame} with additional columns.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   casts <- read_casts()
#'   casts_a <- select_casts(casts, tmnt_types = "All")
#'   casts_a <- append_observed_to_cast(casts_a) 
#'  }
#'
#' @export
#'
append_observed_to_cast <- function(casts, main = ".", add_error = TRUE,
                                    add_in_window = TRUE, add_lead = TRUE, 
                                    arg_checks = TRUE){
  check_args(arg_checks)
  level <- unique(casts$level)
  if (length(level) != 1){
    stop("`casts` must have (only) one type for `level` column")
  }
  obs <- read_rodents_table(main, tolower(level)) 
  colnames(obs)[which(colnames(obs) == "NA.")] <- "NA"
  casts$observed <- NA
  for(i in 1:nrow(casts)){
    nmmatch <- which(obs$newmoonnumber == casts$newmoonnumber[i])
    sppmatch <- which(colnames(obs) == casts$species[i])
    obsval <- obs[nmmatch, sppmatch]
    if (length(obsval) == 1){
      casts$observed[i] <- obsval
    }
  }
  if (add_error){
    casts$error <- casts$estimate - casts$observed
  }
  if (add_in_window){
    above_lower <- casts$observed >= casts$LowerPI
    below_upper <- casts$observed <= casts$UpperPI
    casts$in_window <- above_lower & below_upper
  }
  if (add_lead){
    casts$lead <- casts$newmoonnumber - casts$initial_newmoon
  }
  casts
}

#' @title Summarize a table of -casts to -cast-level errors
#'
#' @description Summarize error across all of the data points within each 
#'  forecast or hindcast in a set of -casts. 
#'
#' @param casts \code{data.frame} of fore- or hindcasts with observed values 
#'  and errors included.
#'
#' @param min_observed \code{integer} value for the minimum number of observed
#'  values needed for a -cast to be retained in the output table. Default is
#'  \code{1}, which returns all -casts with any observations. To include all
#'  -casts (even those without any evaluations), set to \code{0}. 
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
#' @return \code{data.frame} of errors summarized to the -cast-level. 
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'    casts <- read_casts()
#'    casts_a <- select_casts(casts, tmnt_types = "All")
#'    casts_a <- append_observed_to_cast(casts_a)
#'    measure_cast_error(casts_a)
#' }
#'
#' @export
#'
measure_cast_error <- function(casts, min_observed = 1, arg_checks = TRUE){
  check_args(arg_checks)
  groupcols <- c("model", "species", "level", "date", "initial_newmoon")
  cols <- which(colnames(casts) %in% groupcols)
  castgroup <- apply(casts[ ,cols], 1, paste, collapse = "_")
  ugroups <- unique(castgroup)
  ngroups <- length(ugroups)
  
  RMSE <- rep(NA, ngroups)
  coverage <- rep(NA, ngroups)
  nsamples <- rep(NA, ngroups)
  nsampleso <- rep(NA, ngroups)
  tmodel <- rep(NA, ngroups) 
  tspecies <- rep(NA, ngroups)
  tlevel <- rep(NA, ngroups)
  tdate <- rep(NA, ngroups)
  tdate <- rep(NA, ngroups)
  fit_start_newmoon <- rep(NA, ngroups)
  fit_end_newmoon <- rep(NA, ngroups)
  for(i in 1:ngroups){
    incl <- which(castgroup == ugroups[i])
    nsamples[i] <- length(incl)
    nsampleso[i] <- length(na.omit(casts$observed[incl]))
    splitname <- strsplit(ugroups[i], "_")[[1]]
    tmodel[i] <- splitname[2]
    tspecies[i] <- splitname[4] 
    tlevel[i] <- splitname[3] 
    tdate[i] <- splitname[1] 
    errs <- na.omit(casts$error[incl])
    RMSE[i] <- sqrt(mean(errs^2))
    in_window <- na.omit(casts$in_window[incl])
    coverage[i] <- sum(in_window)/length(in_window)
    fit_start_newmoon[i] <- unique(casts$fit_start_newmoon[incl])
    fit_end_newmoon[i] <- unique(casts$fit_end_newmoon[incl])
  }
  casttab <- data.frame(model = tmodel, species = tspecies, level = tlevel, 
                        date = tdate, fit_start_newmoon = fit_start_newmoon,
                        fit_end_newmoon = fit_end_newmoon, 
                        nsamples = nsamples, nsamples_obs = nsampleso, 
                        RMSE = RMSE, coverage = coverage)
  no_obs <- which(nsampleso < min_observed)
  if (length(no_obs) > 0){
    casttab <- casttab[-no_obs, ]    
  }
  casttab
}


#' @title Select the specific fore- or hindcast from a casts table
#'
#' @description Given a \code{casts}-class \code{data.frame} of many models'
#'  predictions, select a specific subset for use.
#'
#' @param casts \code{data.frame} of requested fore- or hindcasts to be 
#'  selected from.
#'
#' @param species \code{character} value(s) of the species code(s) or
#'  \code{"total"} for the total across species. If \code{NULL}, all species 
#'  and "total" are returned. 
#'
#' @param tmnt_types \code{character} value(s) of the level(s) of interest 
#'  (\code{"All"} or \code{"Controls"}). If \code{NULL}, all levels are 
#'  returned.
#'
#' @param models \code{character} value(s) of the name(s) (or 
#'  \code{"Ensemble"}) of the model(s) of interest. If \code{NULL}, all
#'  models are returned.
#'
#' @param target_moons \code{integer}-conformable value(s) of the 
#'  newmoonnumber(s) of interest. If \code{NULL}, all newmoons are returned.
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
#' @return \code{data.frame} of trimmed fore- or hindcasts.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   casts <- read_casts()
#'   scasts <- select_casts(casts)
#'  }
#'
#' @export
#'
select_casts <- function(casts, species = NULL, tmnt_types = NULL, 
                         models = NULL, target_moons = NULL, 
                         arg_checks = TRUE){
  check_args(arg_checks)
  incl_species <- rep(TRUE, nrow(casts))
  incl_level <- rep(TRUE, nrow(casts))
  incl_model <- rep(TRUE, nrow(casts))
  incl_nmm <- rep(TRUE, nrow(casts))

  casts <- na_conformer(casts)
  if (!is.null(species)){
    incl_species <- casts[ , "species"] %in% species
  }
  if (!is.null(tmnt_types)){
    incl_level <- casts[ , "level"] %in% tmnt_types
  }
  if (!is.null(models)){
    incl_model <- casts[ , "model"] %in% models
  }
  if (!is.null(target_moons)){
    incl_nmm <- casts[ , "newmoonnumber"] %in% target_moons
  }
  incl <- which(incl_species & incl_level & incl_model & incl_nmm)
  casts[incl, ]
}

#' @title Read in a cast file or multiple cast files and set the class
#'
#' @description Read in a specified forecast or hindcast data file and ensure 
#'  its class attribute is appropriate for usage within the portalcasting 
#'  pipeline. Current not reliably coded for hindcasts. \cr \cr 
#'  \code{read_cast} only allows for a single date and defaults to
#'  the most recent. \cr \cr 
#'  \code{read_casts} allows for multiple dates and defaults to all.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'  
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'  to select the file in the predictions subdirectory. Currently only 
#'  reliably coded for \code{"forecasts"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'  file in the predictions subdirectory. Can only be length 1 or \code{NULL}. 
#'  If \code{NULL} (default), selects the most recent -casts.
#'
#' @param cast_dates \code{Date}s the predictions were made. Used to select 
#'   the files in the predictions subdirectory. Can be length 1 or more and if 
#'   \code{NULL} (default), selects all available -casts.
#'
#' @param verbose \code{logical} indicator if all validation errors should
#'  be reported.
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
#' @return \code{data.frame} of requested fore- or hindcast(s).
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   read_cast()
#'   read_casts()
#'  }
#'
#' @export
#'
read_cast <- function(main = ".", cast_type = "forecast", 
                      cast_date = NULL, verbose = FALSE, 
                      arg_checks = TRUE){
  if (is.null(cast_date)){
    cast_date <- most_recent_cast(main, cast_type)
  }
  lpath1 <- paste0("predictions/", cast_date, cast_type, ".csv")
  fpath1 <- file_paths(main, lpath1)
  lpath2 <- paste0("predictions/", cast_date, cast_type, "s.csv")
  fpath2 <- file_paths(main, lpath2)
  if (file.exists(fpath1)){
    read_in <- read.csv(fpath1, stringsAsFactors = FALSE)
  } else if (file.exists(fpath2)){
    read_in <- read.csv(fpath2, stringsAsFactors = FALSE)
  } else{
    stop(paste0(cast_type, " from ", cast_date, " not available"))
  }
  na_conformer(read_in) %>%
  column_conformer() %>%
  verify_cast(verbose)
}

#' @rdname read_cast 
#'
#' @export
#'
read_casts <- function(main = ".", cast_type = "forecast", 
                       cast_dates = NULL, verbose = FALSE, 
                       arg_checks = TRUE){
  if (is.null(cast_dates)){
    pfolderpath <- sub_paths(main, "predictions")
    pfiles <- list.files(pfolderpath)
    of_interest1 <- grepl(cast_type, pfiles)
    of_interest2 <- grepl("aic", pfiles)
    cast_text <- paste0(cast_type, ".csv")
    cast_dates <- gsub(cast_text, "", pfiles[of_interest1 & !of_interest2])
    cast_dates <- as.Date(cast_dates)
  }
  ndates <- length(cast_dates)
  all_casts <- data.frame()
  for(i in 1:ndates){  
    all_casts <- read_cast(main, cast_type, cast_dates[i], verbose) %>%
                 bind_rows(all_casts, .)
  }
  all_casts
}

#' @title Verify that a read-in cast file is formatted appropriately
#'
#' @description Ensure that a -cast file that has been read in is formatted
#'  according to \href{https://bit.ly/2H2z3Jb}{specifications.}
#'
#' @param cast \code{data.frame} -cast file read in.
#'
#' @param cast_to_check \code{data.frame} -cast file being checked within 
#'  \code{cast_is_valid}.
#'
#' @param verbose \code{logical} indicator if all validation errors should
#'  be reported.
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
#' @return \code{verify_cast}: \code{cast} as read in (as long as it is 
#'  valid). \cr \cr
#'  \code{cast_is_valid}: \code{logical} of if the -cast is formatted 
#'  properly.
#' 
#' @export
#'
verify_cast <- function(cast, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  if(!cast_is_valid(cast)){
    stop("`cast` is not valid")
  }
  cast
}

#' @rdname verify_cast
#'
#' @export
#'
cast_is_valid <- function(cast_to_check, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  is_valid <- TRUE
  violations <- c()
  valid_columns1 <- c("date", "forecastmonth", "forecastyear", 
                      "newmoonnumber", "model", "currency", "level", 
                      "species", "estimate", "LowerPI", "UpperPI", 
                      "fit_start_newmoon", "fit_end_newmoon",
                      "initial_newmoon")
  valid_columns2 <- valid_columns1
  valid_columns2[2:3] <- c("castmonth", "castyear")
  valid_currencies <- c("abundance", "richness", "biomass", "energy")
  valid_levels <- c("All", "Controls", "FullExclosure", "KratExclosure",
                     paste("Plot", 1:24, " ", sep = ""))
  valid_species <- all_species(total = TRUE)

  cols_valid1 <- all(colnames(cast_to_check) %in% valid_columns1) & 
                 all(valid_columns1 %in% colnames(cast_to_check))
  cols_valid2 <- all(colnames(cast_to_check) %in% valid_columns2) & 
                 all(valid_columns2 %in% colnames(cast_to_check))

  if(!cols_valid1 & !cols_valid2){
    messageq("file column names invalid", !verbose)
    return(FALSE)
  }

  cast_to_check$date <- as.Date(cast_to_check$date, "%Y-%m-%d")
  if(any(is.na(cast_to_check$date))){
    is_valid <- FALSE
    violations <- c("date", violations) 
  }
  if(!all(unique(cast_to_check$currency) %in% valid_currencies)){
    is_valid <- FALSE
    violations <- c("currency", violations) 
  }
  if(!all(unique(cast_to_check$level) %in% valid_levels)){ 
    is_valid <- FALSE
    violations <- c("level", violations) 
  }
  if(!all(unique(cast_to_check$species) %in% valid_species)){ 
    is_valid <- FALSE
    violations <- c("species", violations) 
  }
  if(any(is.na(cast_to_check$estimate))) { 
    is_valid <- FALSE
    violations <- c("NA esimates", violations) 
  }
  if(any(is.na(cast_to_check$LowerPI))) { 
    is_valid <- FALSE
    violations <- c("NA LowerPI", violations) 
  }
  if(any(is.na(cast_to_check$UpperPI))) { 
    is_valid <- FALSE
    violations <- c("NA UpperPI", violations) 
  }

  if(!is.integer(cast_to_check$fit_start_newmoon)) {
    is_valid <- FALSE
    violations <- c("fit_start_newmoon not int", violations)
  }
  if(!is.integer(cast_to_check$fit_end_newmoon)) { 
    is_valid <- FALSE
    violations <- c("fit_end_newmoon not int", violations)
  }
  if(!is.integer(cast_to_check$initial_newmoon)) {
    is_valid <- FALSE
    violations <- c("initial_newmoon not int", violations)
  }
  if(any(is.na(cast_to_check$fit_start_newmoon))) {
    is_valid <- FALSE
    violations <- c("fit_start_newmoon contains NA", violations)
  }
  if(any(is.na(cast_to_check$fit_end_newmoon))) {
    is_valid <- FALSE
    violations <- c("fit_end_newmoon contains NA", violations)
  }
  if(any(is.na(cast_to_check$initial_newmoon))) {
    is_valid <- FALSE
    violations <- c("initial_newmoon contains NA", violations)
  }
  
  if(length(violations) > 0){
    violationses <- paste(violations, collapse = ", ")
    messageq(paste0("Cast validation failed: ", violationses), !verbose)
  }
  is_valid
}

#' @title Conform column names for use internally
#'
#' @description Existing cast files may have column names using "forecast"
#'  rather than the current generalized "cast". This function replaces the
#'  old with the new for internal use.
#'
#' @param df \code{data.frame} to have column names converted if needed.
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
#' @return \code{df} with column names converted if needed.
#'
#' @examples
#'  df <- data.frame(xforecastx = 1:10, fore = 2:11)
#'  column_conformer(df)
#'
#' @export
#'
column_conformer <- function(df = NULL, arg_checks = TRUE){
  return_if_null(df)
  check_args(arg_checks)
  cnames <- colnames(df)
  names(df) <- gsub("forecast", "cast", cnames)
  df
}

#' @title Determine the most recent forecast or hindcast
#'
#' @description Determine the date of the most recently produced forecast or 
#'   hindcast in a predictions folder.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'  
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. 
#'
#' @param with_census \code{logical} toggle if the plot should include the
#'   observed data collected during the predicted census.
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
#' @return \code{Date} of the most recent cast.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   most_recent_cast()
#'  }
#'
#' @export
#'
most_recent_cast <- function(main = ".", cast_type = "forecast",
                             with_census = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  pfolderpath <- sub_paths(main, "predictions")
  pfiles <- list.files(pfolderpath)
  of_interest1 <- grepl(cast_type, pfiles)
  of_interest2 <- grepl("aic", pfiles)
  if (sum(of_interest1 & !of_interest2) < 1){
    stop(paste0("no valid ", cast_type, " files in the predictions folder"))
  }
  cast_text <- paste0(cast_type, ".csv")
  cast_dates <- gsub(cast_text, "", pfiles[of_interest1 & !of_interest2])
  cast_dates <- as.Date(cast_dates)
  prior_to <- Sys.Date() + 1
  if (with_census){
    prior_to <- most_recent_census(main)
  }
  max(cast_dates[cast_dates < prior_to])
}





#' @title Add ensemble model to forecasts
#' 
#' @description Add the predictions of an ensemble model to the forecasting 
#'   files.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample.  
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
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
#' @return Forecast abundance table for the ensemble model.
#' 
#' @export
#'
add_ensemble <- function(main = ".", moons = NULL, end_moon = NULL, 
                         cast_date = Sys.Date(), confidence_level = 0.9, 
                         quiet = FALSE, arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  messageq("Creating ensemble model", quiet)
  temp_dir <- sub_paths(main, "tmp")
  pred_dir <- sub_paths(main, "predictions")
  last_moon <- last_newmoon(main = main, moons = moons, cast_date = cast_date,
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  filename_suffix <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  cclass <- c("Date", "integer", "integer", "integer", "character", 
              "character", "character", "character", "numeric",
              "numeric", "numeric", "integer", "integer", "integer")
  all_casts <- do.call(rbind, 
               lapply(files, read.csv, na.strings = "", colClasses  = cclass))
  ensemble <- make_ensemble(all_casts = all_casts, main = main,
                            confidence_level = confidence_level,
                            arg_checks = arg_checks) 
  ensemble <- select(ensemble, colnames(all_casts))
  cast_date <- as.character(cast_date)
  cast_fname <- paste0(cast_date, filename_suffix, ".csv")
  cast_filename <- file.path(pred_dir, cast_fname)
  append_csv(ensemble, cast_filename)
  ensemble
}

#' @title Create the ensemble model from all other forecasts
#' 
#' @description Combine the fitted models to make an ensemble prediction. 
#'
#' @details Uses the weighted mean and weighted sample variance to combine
#'   the models. The mean is the weighted mean of all model means and the
#'   variance is the weighted mean of all model variances + the variances of 
#'   the weighted mean using the unbiased estimate of sample variance. See
#'   https://github.com/weecology/portalPredictions/pull/65
#'   We only store the prediction interval for models, so we backcalculate 
#'   individual model variance assuming the same \code{confidence_level} 
#'   throughout. Assert that the summed weight of all the model ensembles is 
#'   1, as that's what the variance estimates assume. The weight values are 
#'   rounded to account for precision errors. Summed weights can also be 
#'   \code{NA} if there are not weights available for that ensemble. 
#' 
#' @param all_casts \code{data.frame} of all of the forecasts to be 
#'   combined. 
#' 
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
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
#' @return Forecast abundance table for the ensemble model.
#' 
#' @export
#'
make_ensemble <- function(all_casts, main = ".", confidence_level = 0.9, 
                          arg_checks = TRUE){
  check_args(arg_checks)
  if (length(unique(all_casts$model)) == 1){
    ensemble <- all_casts
    ensemble$model <- "Ensemble"
    ensemble$LowerPI <- ifelse(ensemble$LowerPI < 0, 0, ensemble$LowerPI)
    return(ensemble)
  }
  weights <- compile_aic_weights(main)
  weights$date <- as.Date(weights$date)
  CI_quantile <- qnorm((1 - confidence_level) / 2, lower.tail = FALSE)

  leftj <- c("date", "model", "currency", "level", "species", 
             "fit_start_newmoon", "fit_end_newmoon", "initial_newmoon")
  grp <- quos(date, newmoonnumber, castmonth, castyear, level, 
           currency, species, fit_start_newmoon, fit_end_newmoon, 
           initial_newmoon)
  mod_var <- quo(((UpperPI - estimate)/CI_quantile) ^ 2)
  est <- quo(sum(estimate * weight))
  wtss <- quo(sum(weight * (estimate - ensemble_estimate)^2))
  ens_var <- quo(sum(model_var * weight) + 
                 weighted_ss / (n() * sum(weight) - 1))
  weighted_estimates <- all_casts %>%
                        mutate(model_var = !!mod_var) %>%
                        left_join(weights, by = leftj) %>%
                        group_by(!!!grp) %>%
                        summarize(ensemble_estimate = !!est, 
                                  weighted_ss = !!wtss ,
                                  ensemble_var = !!ens_var,
                                  sum_weight = sum(weight)) %>% 
                        ungroup() 

  check_sum <- round(weighted_estimates$sum_weight, 10) == 1 
  check_na <- is.na(weighted_estimates$sum_weight)       
  if(!all(check_sum | check_na)){ 
    stop("Summed weights do not equal 1")
  }

  PI_l <- quo(ensemble_estimate - (sqrt(ensemble_var) * CI_quantile))
  PI_U <- quo(ensemble_estimate + (sqrt(ensemble_var) * CI_quantile))
  ensemble <- weighted_estimates %>%
              mutate(LowerPI = !!PI_l, UpperPI = !!PI_U) %>%
              mutate(LowerPI = ifelse(LowerPI<0, 0, LowerPI)) %>%
              rename(estimate = ensemble_estimate) %>%
              select(-ensemble_var, -weighted_ss, -sum_weight)
  
  ensemble$model <- "Ensemble"
  ensemble
}

#' @title Calculate model weights
#' 
#' @description Calculate AIC-based weights across the models for the full
#'   time series.
#' 
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
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
#' @return \code{data.frame} of model weights.
#' 
#' @export
#'
compile_aic_weights <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  pred_dir <- sub_paths(main, "predictions")

  aic_files <- list.files(pred_dir, full.names = TRUE, recursive = TRUE)
  aic_files <- aic_files[grepl("model_aic", aic_files)]

  aics <- do.call(rbind, 
      lapply(aic_files, read.csv, na.strings = "", stringsAsFactors = FALSE))
 
  grps <- quos(date, currency, level, species, fit_start_newmoon, 
            fit_end_newmoon, initial_newmoon)
 
  aics %>%
  group_by(!!!grps) %>%
  mutate(delta_aic = aic - min(aic), 
         weight = exp(-0.5 * delta_aic) / sum(exp(-0.5*delta_aic))) %>%
  ungroup()
}



#' @title Combine casts and append them to existing files
#' 
#' @description Combine all new casts (from the tmp subdirectory) into
#'  and append the results to the existing files.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
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
#' @return \code{list} of [1] \code{"casts"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @export
#'
combine_casts <- function(main = ".", moons = NULL,
                          end_moon = NULL, 
                          cast_date = Sys.Date(), quiet = FALSE, 
                          arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))

  check_args(arg_checks)
  messageq("Compiling casts", quiet)
  temp_dir <- sub_paths(main, "tmp")
  pred_dir <- sub_paths(main, "predictions")
  last_moon <- last_newmoon(main = main, moons = moons, cast_date = cast_date,
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  filename_suffix <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  casts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))
  file_ptn <- paste(filename_suffix, "_model_aic.csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)

  aics <- do.call(rbind, lapply(files, read.csv, na.strings = ""))
  
  cast_date <- as.character(cast_date)
  cast_fname <- paste(cast_date, filename_suffix, ".csv", sep = "")
  cast_filename <- file.path(pred_dir, cast_fname)
  aic_fname <- paste(cast_date, filename_suffix, "_model_aic.csv", sep = "")
  model_aic_filename <- file.path(pred_dir, aic_fname)
  append_csv(casts, cast_filename)
  append_csv(aics, model_aic_filename)
  
  list(casts = casts, all_model_aic = aics)
}


#' @title Save cast output to files
#'
#' @description Save out the cast abundances and AIC tables from a given 
#'   set of models.
#'
#' @param all Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "all" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param controls Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "controls" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param model \code{character} value of the name of the model.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
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
#' @export
#'
save_cast_output <- function(all, controls, model, main, 
                             arg_checks = TRUE){
  check_args(arg_checks)
  metadata <- read_data(main, "metadata");
  temp_dir <- sub_paths(main, "tmp")
  casts <- rbind(all$cast, controls$cast)
  aics <- rbind(all$aic, controls$aic)

  cast_fname <- paste0(model, metadata$cast_type, ".csv")
  cast_path <- file.path(temp_dir, cast_fname)
  write.csv(casts, cast_path, row.names = FALSE)

  aic_fname <- paste0(model, metadata$cast_type, "_model_aic.csv")
  aic_path <- file.path(temp_dir, aic_fname)
  write.csv(aics, aic_path, row.names = FALSE)

}