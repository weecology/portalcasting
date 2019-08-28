#' @title Check a function's arguments' values for validity
#'
#' @description Provides toggle-able argument checking for nearly all 
#'  functions in the codebase (save a few utilities), that can be turned off
#'  to facilitate sandboxing or kept on to support reliability. \cr \cr
#'  \code{check_args} checks that all of the arguments to a given
#'  function have valid values within the pipeline to avoid naming collision
#'  and improper formatting, by wrapping around \code{check_arg} for each
#'  argument. \cr \cr
#'  \code{check_arg} returns \code{NULL} if the argument is valid or 
#'  \code{character} vector of error messages associated 
#'  with the specific situation (\code{<fun_name>(<arg_name> = arg_value)})
#'  if the argument is invalid. \cr \cr
#'  \code{check_arg_list} produces a list of argument details for 
#'  checking within \code{check_arg}. \cr  \cr 
#'  \strong{\code{check_args} should only be called within a function.}
#'  \cr \cr
#'  See \code{Details} for argument-specific rules.
#'
#' @details Argument details used for checking are produced simply via
#'  \code{check_arg_list}. If you are adding to the codebase and use
#'  novel arguments, add them to that function's production. \cr
#'  Each argument should have its own element that is a list with the 
#'  following elements: \code{class} (\code{character} value of its class), 
#'  \code{null} (\code{logical} indicator if the argument can be \code{NULL}),
#'  \code{na} (\code{logical} indicator if the argument can be \code{NA}),
#'  \code{length} (\code{numeric} value of how many elements the argument can
#'  be or \code{NULL} if unrestricted), and \code{addl} if needed (an
#'  unevaluated \code{expression} that can be evaluated and allows for 
#'  simple evaluation of, e.g., element names of a \code{list}; see the 
#'  source code). \cr 
#'
#'  Usage rules for arguments are as follows: \cr
#'
#'  Must be length-1 \code{logical} values, can be \code{NULL}, but cannot be 
#'  \code{NA}: 
#'   \code{add_error},
#'   \code{add_in_window}, 
#'   \code{add_lead}, 
#'   \code{add_obs}, 
#'   \code{append_cast_csv}, 
#'   \code{arg_checks}, 
#'   \code{cast_covariates}, 
#'   \code{clean},
#'   \code{cleanup},
#'   \code{covariatesTF}, 
#'   \code{effort},
#'   \code{fillweight}, 
#'   \code{hist_covariates}, 
#'   \code{interpolate}, 
#'   \code{na_drop}, 
#'   \code{nadot}, 
#'   \code{NULLname},
#'   \code{overwrite}, 
#'   \code{quiet}, 
#'   \code{retain_target_moons},
#'   \code{return_version},
#'   \code{save}, 
#'   \code{tail}, 
#'   \code{total}, 
#'   \code{unknowns}, 
#'   \code{verbose},
#'   \code{with_census},
#'   \code{zero_drop}. 
#'
#'  Must be \code{logical} values, can be any length or \code{NULL}, but 
#'  cannot be \code{NA}: 
#'   \code{movedTF}, 
#'
#'  Must be length-1 \code{character} values, can be \code{NULL}, but cannot 
#'  be \code{NA}:
#'   \code{colname}, 
#'   \code{data_name}, 
#'   \code{data_set}, 
#'   \code{directory}, 
#'   \code{extension} (if not \code{NULL}, must have a \code{"."} character),
#'   \code{filename}, 
#'   \code{filename_cov}, 
#'   \code{filename_cov_casts}, 
#'   \code{filename_config}, 
#'   \code{filename_meta},
#'   \code{filename_moons}, 
#'   \code{freq},  
#'   \code{level}, 
#'   \code{main}, 
#'   \code{model} (inputted values are checked via 
#'    \code{\link{verify_models}}), 
#'   \code{name}, 
#'   \code{output} (if not \code{NULL}, must be \code{"abundances"}),
#'   \code{path},
#'   \code{plots} (if not \code{NULL}, must be \code{"all"} or 
#'    \code{"longterm"}),
#'   \code{raw_data}, 
#'   \code{sep_char}, 
#'   \code{set},
#'   \code{shape},
#'   \code{source_name},
#'   \code{source_url}, 
#'   \code{species_id} (if not \code{NULL}, must be within the species
#'    produced by \code{rodent_spp("wtotal")}),
#'   \code{sub} (must be one of the named sub directories: \code{"tmp"},
#'    \code{"raw"}, \code{"data"}, \code{"models"}, or \code{"casts"}),
#'   \code{time}, 
#'   \code{type}, 
#'   \code{url}, 
#'   \code{winner} (if not \code{NULL}, must be \code{"hist"} or 
#'    \code{"cast"}), 
#'   \code{zip_destin}.
#'
#'  Must be \code{character} values, can be any length, can be \code{NULL}, 
#'  but cannot be \code{NA}:
#'   \code{concept_rec_id}, 
#'   \code{data}, 
#'   \code{data_sets}, 
#'   \code{downloads_versions},
#'   \code{enquote_args}, 
#'   \code{eval_args}, 
#'   \code{files}, 
#'   \code{models} (inputted values are checked via 
#'    \code{\link{verify_models}}), 
#'   \code{msg}, 
#'   \code{names},
#'   \code{paths}, 
#'   \code{rec_id}, 
#'   \code{rec_version}, 
#'   \code{ref_species},
#'   \code{species} (if not \code{NULL}, must be all within the species
#'    produced by \code{all_secies(total = TRUE)}),
#'   \code{subs} (must all be one of the named sub directories: \code{"tmp"},
#'    \code{"raw"}, \code{"data"}, \code{"models"}, or \code{"casts"}),
#'   \code{target_cols}, 
#'   \code{treatment} (if not \code{NULL}, must be \code{"control"})
#'
#'  Must be \code{data.frame}s, can be any length, can be \code{NULL}, 
#'  but cannot be \code{NA}:
#'   \code{casts}, 
#'   \code{cast_coc}, 
#'   \code{cast_tab}, 
#'   \code{casts}, 
#'   \code{covariates}, 
#'   \code{df}, 
#'   \code{hist_cov}, 
#'   \code{hist_tab}, 
#'   \code{moons}, 
#'   \code{rodents_tab}.
#'
#'  Must be a \code{data.frame} or \code{vector} can be any length, can be 
#'  \code{NULL}, but cannot be \code{NA}:
#'   \code{dfv}.
#'
#'  Must be a \code{data.frame} or \code{list} can be any length, can be 
#'  \code{NULL}, but cannot be \code{NA}:
#'   \code{dfl}.
#'
#'  Must be \code{list}s, can be any length, can be \code{NULL}, but cannot 
#'  be \code{NA}:
#'   \code{control},
#'   \code{control_cdl},
#'   \code{controls_m},
#'   \code{controls_r},
#'   \code{data_set_controls},
#'   \code{downloads},
#'   \code{metadata},
#'   \code{rodents}.
#'
#'  Must be length-1 \code{Date}-conformable values, can be \code{NULL}, 
#'  but cannot be \code{NA}:
#'   \code{cast_date},
#'   \code{date}
#'   \code{end},
#'   \code{from_date},
#'   \code{start}.
#'
#'  Must be \code{Date}-conformable values, can be any length, can be
#'  \code{NULL}, but cannot be \code{NA}:
#'   \code{cast_dates}.
#'   \code{dates}.
#'
#'  Must be length-1 \code{numeric} values, can be \code{NULL}, but
#'  cannot be \code{NA}:
#'   \code{confidence_level} (must be between 0 and 1), 
#'   \code{lat},
#'   \code{lon}.
#'
#'  Must be length-1 \code{integer}-conformable values can be \code{NULL},
#'  but cannot be \code{NA}:
#'   \code{cast_id} (must be non-negative),
#'   \code{end_moon} (must be positive),
#'   \code{lead} (must be positive),
#'   \code{lead_time} (must be non-negative),
#'   \code{lev},
#'   \code{min_observed} (must be positive),
#'   \code{min_plots} (must be positive),
#'   \code{min_traps} (must be positive),
#'   \code{moon} (must be positive),
#'   \code{ndates} (must be positive),
#'   \code{nmoons} (must be non-negative),
#'   \code{start_moon} (must be positive),
#'   \code{topx} (must be non-negative).
#'
#'  Must be length-1 \code{integer}-conformable values can be \code{NULL}, 
#'  and can be \code{NA}:
#'   \code{lag} (must be positive),
#'   \code{min_lag} (must be non-negative).
#'
#'  Must be \code{integer}-conformable values, can be any length, can be 
#'  \code{NULL}, but cannot be \code{NA}:
#'   \code{end_moons} (must be positive),
#'   \code{target_moons} (must be positive).
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'   checked.
#'
#' @param arg_name \code{character} value of the argument name.
#'
#' @param arg_value Input value for the argument.
#'
#' @param fun_name \code{character} value of the function name or \code{NULL}.
#'
#' @return \code{check_args}: throws an error or \code{NULL}. \cr \cr
#'   \code{check_arg}: \code{character} vector of error messages
#'   associated with the specific situation 
#'   (\code{<fun_name>(<arg_name> = arg_value)}).
#'   \code{check_arg_list}: \code{list} of argument checking component 
#'   \code{list}s.
#'
#' @examples
#'  check_arg_list()
#'  check_arg("end_moon", 400)
#'  check_arg("end_moon", 399.5)
#'
#' @name argument_checking
#'
NULL

#' @rdname argument_checking
#'
#' @export
#'
check_args <- function(arg_checks = TRUE){
  if(length(arg_checks) !=1 | !is.logical(arg_checks)){
    stop("`arg_checks` must be a single logical value")
  }
  if(!arg_checks){
    return()
  }

  fun_call <- match.call.defaults(definition = sys.function(-1), 
                                  call = sys.call(-1))
  fun_class <- class(as.list(fun_call)[[1]])
  if(fun_class == "name"){  
    fun_name <- as.list(fun_call)[[1]]
  } else if(fun_class == "function"){

    fun_call2 <- match.call.defaults(definition = sys.function(-2), 
                                     call = sys.call(-2))
    fun_name <- as.list(fun_call2)[[1]]

  } else{
    stop("unrecognized class for function reference in check_args")
  }
  arg_values <- as.list(fun_call)[-1]
  arg_names <- names(arg_values)
  nargs <- length(arg_names)
  out <- NULL
  if(nargs > 0){
    for(i in 1:nargs){
      arg_value <- tryCatch(
                     eval.parent(arg_values[[i]], 2),
                     error = function(x){NA},
                     warning = function(x){x}
                   )
      if(!all(is.null(arg_value)) && all(is.na(arg_value))){
        arg_value <- tryCatch(
                       eval.parent(arg_values[[i]], 1),
                       error = function(x){NULL},
                       warning = function(x){x}
                     )
      }
      out <- c(out, check_arg(arg_names[i], arg_value, fun_name))
    }
  }
  if (!is.null(out)){
    if (length(out) > 1){
      out2 <- paste(out, collapse = "; ")
    } else{
      out2 <- out
    }
    out3 <- paste0("in ", toString(fun_name), ": ", out2)
    stop(out3, call. = FALSE)
  }
}

#' @rdname argument_checking
#'
#' @export
#'
check_arg <- function(arg_name, arg_value, fun_name = NULL){
  out <- NULL
  arg_list <- check_arg_list()
  if(!(arg_name %in% names(arg_list))){
    msg <- paste0(arg_name, " not checked, consider adding to check_arg_list")
    message(msg)
    return(out)
  }
  deets <- arg_list[[arg_name]]
  can_be_null <- deets$null
  can_be_na <- deets$na

  if(is.null(arg_value)){
    if(!can_be_null){
      out <- paste0("`", arg_name, "` cannot be NULL")
    }
    return(out)
  }

  if(all(is.na(arg_value))){
    if(!can_be_na){
      out <- paste0("`", arg_name, "` cannot be NA")
    }
    return(out)
  }

  if(deets$class == "date"){
    cast_date2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
    if (any(is.na(cast_date2))){
      out2 <- paste0("`", arg_name, "` must be a Date or date-conformable")
      out <- c(out, out2)
    }
  } else if (deets$class == "df"){
    if (!("data.frame" %in% class(arg_value))){
      out2 <- paste0("`", arg_name, "` must be a data.frame")
      out <- c(out, out2)
    }
  } else if (deets$class == "cast"){
    if (!("list" %in% class(arg_value))){
      out2 <- paste0("`", arg_name, "` must be a data.frame or list")
      out <- c(out, out2)
    }
  } else if (deets$class == "dfv"){
    if (!("data.frame" %in% class(arg_value)) & !is.vector(arg_value)){
      out2 <- paste0("`", arg_name, "` must be a data.frame or vector")
      out <- c(out, out2)
    }
  } else if (deets$class == "dfl"){
    if (!("data.frame" %in% class(arg_value)) & 
        !("list" %in% class(arg_value))){
      out2 <- paste0("`", arg_name, "` must be a data.frame or list")
      out <- c(out, out2)
    }
  } else if (deets$class == "intnum"){
    if (!is.numeric(arg_value) || !all(arg_value %% 1 == 0)){
      out2 <- paste0("`", arg_name, "` must be an integer-conformable number")
      out <- c(out, out2)
    }
  } else if (deets$class == "nonnegintnum"){
    if (!is.numeric(arg_value) || 
       (!all(arg_value %% 1 == 0 & arg_value >= 0))){
      out2 <- paste0("`", arg_name, 
                     "` must be a non-negative integer-conformable number")
      out <- c(out, out2)
    }
  } else if (deets$class == "posintnum"){
    if (!is.numeric(arg_value) || 
       (!all(arg_value %% 1 == 0 & arg_value > 0))){
      out2 <- paste0("`", arg_name,
                     "` must be a positive integer-conformable number")
      out <- c(out, out2)
    }
  } else if (deets$class == "zeroone"){
    if (!is.numeric(arg_value) || 
       (any(arg_value < 1e-5) | any(arg_value > (1 - 1e-5)))){
      out2 <- paste0("`", arg_name, "` must be a number between 0 and 1")
      out <- c(out, out2)
    }
  } else if (deets$class == "extension"){
    if(("character" %in% class(arg_value))){
      lext <- nchar(arg_value)
      spot <- rep(NA, lext)
      for(i in 1:lext){
        spot[i] <- substr(arg_value, i, i) == "."
      }
      if (sum(spot) != 1){
        out2 <- paste0("`", arg_name, "` must be an extension")
        out <- c(out, out2)
      } 
    } else{
      out2 <- paste0("`", arg_name, "` must be a character")
      out <- c(out, out2)
    }
  } else{
    if(!(deets$class %in% class(arg_value))){
      out2 <- paste0("`", arg_name, "` must be a ", deets$class)
      out <- c(out, out2)
    }
    if(deets$class == "character" & !is.null(deets$vals)){
      if(!all(tolower(arg_value) %in% tolower(deets$vals))){  
        opts <- paste(tolower(deets$vals), collapse = ", ")
        out2 <- paste0("`", arg_name, "` must be one of ", opts)
        out <- c(out, out2)    
      }
    }
  } 
  length_arg <- length(arg_value)
  length_ok <- deets$length
  if(!is.null(length_ok)){
    if(length_arg != length_ok){
      out2 <- paste0("`", arg_name, "` must be of length ", length_ok) 
      if(can_be_null){
        out2 <- paste0(out2, " or NULL")
      }
      out <- c(out, out2)
    }
  }
  out
}



#' @rdname argument_checking
#'
#' @export
#'
check_arg_list <- function(){

  arg_logical <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "logical", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_character <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "character", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_numeric <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "numeric", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_zeroone <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "zeroone", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_list <- function(length = NULL, null = TRUE, na = FALSE, vals = NULL){
    list(class = "list", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_df <- function(length = NULL, null = TRUE, na = FALSE, vals = NULL){
    list(class = "df", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_cast <- function(length = NULL, null = TRUE, na = FALSE, vals = NULL){
    list(class = "cast", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_date <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "date", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_dfv <- function(length = NULL, null = TRUE, na = FALSE, vals = NULL){
    list(class = "dfv", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_dfl <- function(length = NULL, null = TRUE, na = FALSE, vals = NULL){
    list(class = "dfl", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_intnum <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "intnum", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_posintnum <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "posintnum", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_nonnegintnum <- function(length = 1, null = TRUE, na = FALSE, 
                               vals = NULL){
    list(class = "nonnegintnum", null = null, na = na, length = length, 
         vals = vals)
  }
  arg_extension <- function(length = 1, null = TRUE, na = FALSE, vals = NULL){
    list(class = "extension", null = null, na = na, length = length, 
         vals = vals)
  }


  avail_outputs <- c("abundance")
  avail_plots <- c("all", "longterm")
  avail_species <- c("BA", "DM", "DO", "DS", "NA", "NA.", "OL", "OT", "PB", 
                     "PE", "PF", "PH", "PI", "PL", "PM", "PP", "RF", "RM", 
                     "RO", "SF", "SH", "SO", "total")
  avail_subs <- c("casts", "models", "raw", "data", "tmp")
  avail_treatments <- c("control")
  avail_winners <- c("hist", "cast")

  list(
    add_error = arg_logical(),
    add_in_window = arg_logical(), 
    add_lead = arg_logical(),
    add_obs = arg_logical(),
    append_cast_csv = arg_logical(),
    arg_checks = arg_logical(),
    cast = arg_cast(),
    cast_id = arg_nonnegintnum(),
    cast_cov = arg_df(),
    cast_covariates = arg_logical(),
    cast_date = arg_date(),
    cast_dates = arg_date(NULL),
    cast_tab = arg_df(),
    casts = arg_df(),
    clean = arg_logical(),
    cleanup = arg_logical(),
    colname = arg_character(),
    concept_rec_id = arg_character(NULL),
    confidence_level = arg_zeroone(),
    control = arg_list(),
    control_cdl = arg_list(),
    controls_m = arg_list(),
    controls_r = arg_list(),
    covariates = arg_df(),
    covariatesTF = arg_logical(),
    data_name = arg_character(),
    data = arg_character(NULL),
    data_set = arg_character(),
    data_set_controls = arg_list(),
    data_sets = arg_character(NULL),
    date = arg_date(),
    dates = arg_date(NULL),
    df = arg_df(),
    dfl = arg_dfl(),
    dfv = arg_dfv(),
    directory = arg_character(),
    downloads = arg_list(),
    downloads_versions = arg_character(NULL),
    effort = arg_logical(),
    end = arg_date(),
    end_moon = arg_posintnum(),
    end_moons = arg_posintnum(NULL),
    extension = arg_extension(),
    filename = arg_character(),
    filename_config = arg_character(),
    filename_cov = arg_character(),
    filename_cov_casts = arg_character(),
    filename_meta = arg_character(),
    filename_moons = arg_character(),
    files = arg_character(NULL),  
    fillweight = arg_logical(),
    freq = arg_character(),
    from_date = arg_date(),
    hist_cov = arg_df(),
    hist_covariates = arg_logical(),
    hist_tab = arg_df(),
    interpolate = arg_logical(null = TRUE),
    lag = arg_nonnegintnum(null = TRUE, na = TRUE),
    lat = arg_numeric(),
    lead = arg_posintnum(),
    lead_time  = arg_nonnegintnum(),
    lev = arg_intnum(),
    level = arg_character(),
    lon = arg_numeric(),
    main = arg_character(),
    metadata = arg_list(),
    min_lag  = arg_nonnegintnum(na = TRUE),
    min_observed = arg_posintnum(),
    min_plots = arg_posintnum(),
    min_traps = arg_posintnum(),
    model = arg_character(),
    models = arg_character(NULL),
    moon = arg_posintnum(),
    moons = arg_df(),
    movedTF = arg_logical(NULL),
    msg = arg_character(NULL),
    na_drop = arg_logical(),
    nadot = arg_logical(),
    name = arg_character(),
    names = arg_character(NULL),
    ndates = arg_posintnum(),
    nmoons = arg_nonnegintnum(),
    NULLname = arg_logical(),
    output = arg_character(vals = avail_outputs),
    overwrite = arg_logical(),
    path = arg_character(),
    paths = arg_character(NULL),
    plots = arg_character(vals = avail_plots),
    quiet = arg_logical(),
    raw_data = arg_character(),
    rec_id = arg_character(NULL),
    rec_version = arg_character(NULL),
    ref_species = arg_character(NULL),
    retain_target_moons = arg_logical(),
    return_version = arg_logical(),
    rodents = arg_list(),
    rodents_tab = arg_df(),
    save = arg_logical(),
    sep_char = arg_character(),
    set = arg_character(),
    shape = arg_character(),
    source_name = arg_character(),
    source_url = arg_character(),
    species = arg_character(NULL, vals = avail_species),
    species_id = arg_character(vals = avail_species),
    start = arg_date(),
    start_moon = arg_posintnum(),
    sub = arg_character(vals = avail_subs),
    subs = arg_character(NULL, vals = avail_subs),
    tail = arg_logical(),
    target_moons = arg_posintnum(NULL),
    target_cols = arg_character(NULL),
    time = arg_character(),
    total = arg_logical(),
    treatment = arg_character(vals = avail_treatments),
    type = arg_character(),
    unknowns = arg_logical(),
    url = arg_character(),
    verbose = arg_logical(),
    winner = arg_character(vals = avail_winners),
    with_census = arg_logical(),
    zero_drop = arg_logical(),
    zip_destin = arg_character()
  )
}

