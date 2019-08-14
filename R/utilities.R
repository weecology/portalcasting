
#' @title Combine a historical table and a cast table
#'
#' @description A simple utility for combining a table of historical data
#'  and a table of cast data that might need to be assigned to either one
#'  or the other.
#'
#' @param hist_tab,cast_tab A pair of \code{data.frame}s with the same columns
#'  includig a code{date} column of \code{Date}s, which is used to align them.
#'
#' @param winner \code{character} value either {"hist"} or \code{"cast"} to
#'  decide who wins any ties. In the typical portalcasting space, this is 
#'  kept at its default value throughout.
#'
#' @return \code{data.frame} combining \code{hist_tab} and \code{cast_tab}.
#' 
#' @examples
#'  hist_tab <- data.frame(date = Sys.Date(), x = 1:10)
#'  cast_tab <- data.frame(date = Sys.Date(), x = 101:110)
#'  combine_hist_and_cast(hist_tab, cast_tab, "hist") 
#'  combine_hist_and_cast(hist_tab, cast_tab, "cast")  
#'
#' @export
#'
combine_hist_and_cast <- function(hist_tab = NULL, cast_tab = NULL, 
                                  winner = "hist"){
  return_if_null(hist_tab, cast_tab)
  return_if_null(cast_tab, hist_tab)
  
  dupes <- which(cast_tab$date %in% hist_tab$date)
  if(length(dupes) > 0){
    if(winner == "hist"){
      cast_tab <- cast_tab[-dupes, ]
    } else if (winner == "cast"){
      dupes <- which(hist_tab$date %in% cast_tab$date)
      hist_tab <- hist_tab[-dupes, ]
    } else {
      stop("winner must be hist or cast")
    }
  }
  bind_rows(hist_tab, cast_tab)
}

#' @title Add a date to a table that has the year month and day as components 
#' 
#' @description Add a date (as a \code{Date}) column to a table that has the 
#'  year month and day as components.
#' 
#' @param x \code{data.frame} with columns named \code{year}, \code{month},
#'  and \code{day}. 
#'
#' @return \code{data.frame} \code{x} with column of \code{Date}s 
#'  named \code{date} added.
#'
#' @examples
#'  df <- data.frame(year = 2010, month = 2, day = 1:10)
#'  add_date_from_components(df)
#'
#' @export
#'
add_date_from_components <- function(x){
  yrs <- x$year
  mns <- x$month
  dys <- x$day
  x$date <- as.Date(paste(yrs, mns, dys, sep = "-"))
  x
}


#' @title Determine the start and end calendar dates for a cast window
#'
#' @description Based on the cast origin (\code{cast_date}), lead time
#'  (\code{lead_time}), and minimum non-0 lag (\code{min_lag}), determines
#'  the dates bracketing the requested window.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @return Named \code{list} with elements \code{start} and \code{end},
#'  which are both \code{Dates}.
#'
#' @examples
#'  \donttest{
#'  create_dir()
#'  fill_raw()
#'  cast_window()
#'  }
#'
#' @export
#'
cast_window <- function(main = ".",
                            moons = prep_moons(main = main), 
                            cast_date = Sys.Date(),
                            lead_time = 12, min_lag = 6){
  lagged_lead <- lead_time - min_lag
  moons0 <- moons[moons$newmoondate < cast_date, ]
  last_moon <- tail(moons0, 1)
  last_moon$newmoondate <- as.Date(last_moon$newmoondate)
  future_moons <- get_future_moons(moons0, num_future_moons = lead_time)
  start_day <- as.character(as.Date(last_moon$newmoondate) + 1)
  end_day <- as.character(as.Date(future_moons$newmoondate[lead_time]))
  list(start = start_day, end = end_day)
}

#' @title Transpose argument lists for use in do.call
#'
#' @description Arguments coming into functions, when captured using 
#'  \code{\link[DesignLibrary]{match.call.defaults}}, require expansion and
#'  transposition for use within other functions. 
#'
#' @details As an example, see the source code of 
#'  \code{\link{rodents_controls}}.
#'
#' @param eval_args \code{character} vector (or \code{NULL} if not needed)
#'  of any arguments in \code{in_args} need to be wrapped in 
#'  \code{\link{eval}} for replication purposes. \cr \cr
#'  This is generally needed if, for example, a function is used to pass
#'  a list of values (e.g., \code{species} in 
#'  \code{\link{rodents_controls}}.
#'
#' @param enquote_args \code{character} vector (or \code{NULL} if not needed)
#'  of any arguments in \code{in_args} need to be wrapped in 
#'  \code{\link{enquote}} for replication purposes. \cr \cr
#'  This is generally needed if, for example, an argument's input is another
#'  argument (e.g., \code{name} in 
#'  \code{\link{rodents_controls}}.
#'
#' @param in_args \code{list} of arguments that can be of varying lengths,
#'  which need to be combined into a \code{list} of argument values that can 
#'  be easily input into \code{\link{do.call}}.
#'
#' @return \code{list} of argument values that can be easily input into 
#'   \code{\link{do.call}} (each element is a \code{list} of arguments.
#'
#' @examples
#'  in_args <- list(x = 1, y = 1:2, z = NULL)
#'  transpose_args(in_args, NULL)
#'
#' @export
#'
transpose_args <- function(in_args = NULL, eval_args = NULL,
                           enquote_args = NULL){

  return_if_null(in_args)
  if(!is.null(eval_args)){
    neval_args <- length(eval_args)
    for(i in 1:neval_args){
      in_args[[eval_args[i]]] <- eval(in_args[[eval_args[i]]])
    }
  }
  if(!is.null(enquote_args)){
    nenquote_args <- length(enquote_args)
    for(i in 1:nenquote_args){
      in_args[[enquote_args[i]]] <- enquote(in_args[[enquote_args[i]]])
    }
  }


  nargs <- length(in_args)
  arg_names <- names(in_args)

  nlevs <- rep(NA, nargs)
  for(i in 1:nargs){
    nlevs[i] <- length((in_args[[i]]))
  }
  enquoted <- names(in_args) %in% enquote_args
  nlevs[which(enquoted)] <- 0
  tot_levs <- prod(nlevs[nlevs > 0])
  out <- vector("list", length = tot_levs)

  arg_indexes <- vector("list", length = nargs)
  for(i in 1:nargs){
    if(nlevs[i] == 0){
      if(enquoted[i]){
        arg_indexes[[i]] <- 1
      } else {
        arg_indexes[[i]] <- 0
      }
    } else {
      arg_indexes[[i]] <- 1:nlevs[i]
    }
  }
  val_arg <- expand.grid(arg_indexes)
  for(j in 1:tot_levs){
    out[[j]] <- vector("list", length = nargs)
    for(i in 1:nargs){
      if (nlevs[i] == 0){
        if(enquoted[i]){
          evaled <- eval(in_args[[i]])
          spec <- val_arg[j, which(arg_names == evaled)]
          out[[j]][[i]] <- in_args[[evaled]][spec]
        } else{
          out[[j]][[i]] <- NULL
        }
      } else{
        out[[j]][[i]] <- in_args[[i]][ val_arg[j,i] ]
      }
    }
    if(length(out[[j]]) < nargs){
      nmissing <- nargs - length(out[[j]])
      outj <- vector("list", length = nargs)
      for(i in 1:length(out[[j]])){
        outj[[i]] <- out[[j]][[i]]
      }
      out[[j]] <- outj
    }
    names(out[[j]]) <- arg_names
  }
  out
}

#' @title Pass arguments from a parent to a child function and call the
#'  child function
#'
#' @description This set of functions provides utility for passing named
#'  arguments through a function pipeline when the functions do not all
#'  necessarily take the same arguments. \cr \cr
#'  \emph{They should only be called from inside other functions}, which
#'  is checked for by the function \code{\link{error_if_deep}}. \cr \cr
#'  The basic idea is that a \code{child} function is to be called within a
#'  \code{parent} function and should generally be using the arguments as
#'  input to the \code{parent}, although arguments may need to get updated
#'  before passing to the child. \cr \cr
#'  \code{pass_and_call} does the full passing of arguments and calling of 
#'   the \code{child}. \cr \cr
#'  \code{args_to_pass} creates the \code{list} of args to pass to the
#'   \code{child}, which includes removing arguments not in the 
#'   \code{\link{formals}} of \code{child}.
#'
#' @details Arguments are evaluated in the environment they were created to 
#'  so that depdencies which have been removed through passing down into
#'  \code{child} do not cause the evaluation to break. Evaluating the 
#'  arguments in a different (child) environment can be done through 
#'  running the code within a child function and passing the updated 
#'  arguments through \code{...}. \cr \cr
#'  Arguments can be set to \code{NULL}, even if that's not the default,
#'  and they will be retained as part of the named arguments \code{list} to
#'  ensure that they are, in fact, used as \code{NULL}. \cr \cr
#'  Use of \code{\link[DesignLibrary]{match.call.defaults}} ensures that
#'  arguments left as defaults are passed along.
#'
#' @param child The function that is called with the inhereted and potentially
#'  updated arguments.
#' 
#' @param lev The number of frames to go back in the stack. Generally should
#'  be kept as \code{-1}, in reference to inheritance from the parent 
#'  function.
#'
#' @param ... Named arguments to be updated from the parent's definition 
#'  before passing to the child function. 
#'
#' @return 
#'  \code{pass_and_call}: the value returned from \code{child}.
#'  \code{args_to_pass}: \code{list} of args to pass to \code{child}.
#'
#' @examples
#'   yy <- function(n = 1, z = sqrt(w), w = 40){
#'           pass_and_call(rnorm, mean = z, sd  = w)
#'         }
#'
#'   yyy <- function(d = 43){
#'            pass_and_call(yy, n = d)
#'          }
#'   yy()
#'   yy(w = 30)
#'   yy(z = 0, w = 1)
#'   yyy()
#'   yyy(d = 2)
#'
#'   zz <- function(n = 1, sd = 40, other_arg = "xx"){
#'           args_to_pass(rnorm)
#'         }
#'
#' @export
#'
pass_and_call <- function(child = NULL, lev = -1, ...){
  return_if_null(child)
  lev2 <- lev - 1
  error_if_deep(lev2)
  args_for_child <- args_to_pass(child, lev2, ...)
  do.call(child, args_for_child)
}

#' @rdname pass_and_call
#'
#' @export
#'
args_to_pass <- function(child = NULL, lev = -1, ...){
  return_if_null(child)
  lev2 <- lev - 1
  error_if_deep(lev2)
  call <- sys.call(lev)
  def <- sys.function(lev)
  parent_call <- match.call.defaults(definition = def, call = call)
  parent_args <- as.list(parent_call)[-1]

  update_parent_args <- update_list(parent_args, ...)

  names_update_parent_args <- names(update_parent_args)
  needed <- names(formals(child))
  which_needed <- which(names_update_parent_args %in% needed)
  needed_args <- update_parent_args[which_needed]
  nneeded_args <- length(needed_args)
  for(i in 1:nneeded_args){ 
    if(!is.null(needed_args[[i]])){
      needed_args[[i]] <- eval(needed_args[[i]], envir = parent.frame(2L))
    }
  }
  needed_args
}

#' @title Error if a function's requestet is deeper than can be handled
#'
#' @description Produces an informative error message when a function 
#'  that should only be called inside of other functions is called outside
#'  of a function (hence the request to the function is too deep for
#'  what it can handle).
#' 
#' @param lev The number of frames back in the stack where the request needs
#'  to be able to be evaluated.
#'
#' @return Throws an error if the function is called in a place where it 
#'  cannot operate and returns \code{NULL} otherwise.
#'
#' @examples
#'  \dontrun{
#'  # will error:
#'  # error_if_deep(-10)
#'  }
#'  error_if_deep(0)
#'
#' @export
#'
error_if_deep <- function(lev){
  lev2 <- lev - 1
  too_deep <- tryCatch(sys.call(lev2), error = function(x){NA})
  if(!is.null(too_deep) && !is.call(too_deep) && is.na(too_deep)){
    msg <- "too deep; function should only be called inside other functions"
    stop(msg, call. = FALSE)
  } 
}



#' @title Update a list's elements
#'
#' @description Update a list with new values for elements
#'
#' @param orig_list \code{list} to be updated with \code{...}. 
#'
#' @param ... Named elements to update in \code{orig_list}
#'
#' @return Updated \code{list}.
#'
#' @examples
#'  orig_list <- list(a = 1, b = 3, c = 4)
#'  update_list(orig_list)
#'  update_list(orig_list, a = "a")
#'  update_list(orig_list, a = 10, b = NULL)
#'
#' @export
#'
update_list <- function(orig_list = list(), ...){
  if(!is.list(orig_list)){
    stop("orig_list must be a list")
  } 
  update_elems <- list(...)
  nupdate_elems <- length(update_elems)
  norig_elems <- length(orig_list)
  update_list <- vector("list", length = norig_elems)
  names(update_list) <- names(orig_list)
  if(norig_elems > 0){
    for(i in 1:norig_elems){
      if(!is.null(orig_list[[i]])){
        update_list[[i]] <- orig_list[[i]]
      }
    }
  }
  if(nupdate_elems > 0){
    names_update_elems <- names(update_elems)
    for(i in 1:nupdate_elems){
      if(!is.null(update_elems[[i]])){
        update_list[[names_update_elems[i]]] <- update_elems[[i]]
      }
    }
  }
  update_list
}

#' @title Remove any specific incomplete entries as noted by an NA
#'
#' @description Remove any incomplete entries in a table, as determined by
#'   the presence of an \code{NA} entry in a specific column 
#'   (\code{colname}).
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param colname A single \code{character} value of the column to use
#'   to remove incomplete entries. 
#'
#' @return \code{df} without any incomplete entries. 
#'
#' @examples
#'  df <- data.frame(c1 = c(1:9, NA), c2 = 11:20)
#'  remove_incompletes(df, "c1")
#'
#' @export
#'
remove_incompletes <- function(df, colname){
  incompletes <- which(is.na(df[ , colname]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}

#' @title Save data out to a file and return it	
#'
#' @description Save inputted data out to a data file if requested and 
#'  return it to the console.
#'
#' @param x \code{data.frame} table to be written out.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param save \code{logical} indicator controlling if \code{x} should 
#'   be saved out.
#'
#' @param filename \code{character} name of the file for saving \code{x}.
#'
#' @param overwrite \code{logical} indicator of if the file should be
#'  overwritten if it exists.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{x} as input.
#'
#'
#' @export
#'
data_out <- function(x = NULL, main = ".", save = TRUE, filename = "x.csv", 
                     overwrite = TRUE, quiet = FALSE){
  return_if_null(x)
  save_it <- FALSE
  if(save){
    fext <- file_ext(filename)
    local_path <- paste0("data/", filename)
    full_path <- file_paths(main, local_path)
    f_exists <- file.exists(full_path)
    if(f_exists){
      if(overwrite){
        save_it <- TRUE
        msg <- paste0(filename, " exists and overwrite = TRUE; file saved")
      } else {
        msg <- paste0(filename, " exists and overwrite = FALSE; not saved") 
      }
    } else{
      save_it <- TRUE
      msg <- paste0(filename, " saved")
    }
    messageq(msg, quiet)
    if( save_it){
        if(fext == "csv"){
          write.csv(x, full_path, row.names = FALSE)
      } else if (fext == "yaml"){
          yams <- as.yaml(x)
          writeLines(yams, con = full_path)
      } else{
        stop("file type not supported")
      }
    }
   
  }
  x
}

#' @title If a value is NULL, trigger the parent function's return
#'
#' @description If the focal input is \code{NULL}, return \code{value} from
#'  the parent function. Should only be used within a function.
#'
#' @param x Focal input.
#'
#' @param value If \code{x} is \code{NULL}, \code{\link{return}} this input
#'  from the parent function. 
#'
#' @return If \code{x} is not \code{NULL}, \code{NULL} is returned. If 
#'  \code{x} is \code{NULL}, the result of \code{\link{return}} with 
#'  \code{value} as its input evaluated within the parent function's 
#'  environment is returned.
#' 
#' @examples
#'  ff <- function(x = 1, null_return = "hello"){
#'    return_if_null(x, null_return)
#'    x
#'  }
#'  ff()
#'  ff(NULL)
#'
#' @export 
#'
return_if_null <- function(x, value = NULL){
  if(is.null(x)){
    do.call(return, list(value), envir = sys.frame(-1))
  } 
}

#' @title Determine the depth of a list
#'
#' @description Evaluate an input for the depth of its nesting. 
#'
#' @details If \code{x = list()}, then technically the input value is a list, 
#'  but is empty (of length \code{0}), so depth is returned as \code{0}.
#'
#' @param x Focal input.
#'
#' @return \code{integer} value of the depth of the list.
#' 
#' @examples
#'  list_depth("a")
#'  list_depth(list())
#'  list_depth(list("a"))
#'  list_depth(list(list("a")))
#'
#' @export 
#'
list_depth <- function(x){
  xx <- match.call()
  xxx <- deparse(xx[[2]])
  if(xxx == "list()"){
    0L
  } else if (is.list(x)){
    1L + max(sapply(x, list_depth))
  } else {
    0L
  }
}

#' @title Replace if NULL
#'
#' @description If the focal input is \code{NULL}, replace it with 
#'   alternative. 
#'
#' @param x Focal input.
#'
#' @param alt Alternative value.
#'
#' @return \code{x} if not \code{NULL}, \code{alt} otherwise.
#' 
#' @examples
#'  ifnull(NULL, 123)
#'  ifnull(TRUE, 123)
#'  ifnull(FALSE, 123)
#'
#' @export 
#'
ifnull <- function(x = NULL, alt = NULL){
  if(is.null(x)){
    x <- alt
  }
  x
}

#' @title Optionally generate a message based on a logical input
#'
#' @description Given the input to \code{quiet}, generate the message(s) 
#'   in \code{msg} or not.
#'
#' @param msg \code{character} vector of the message(s) to generate or 
#'   \code{NULL}. If more than one element is contained in \code{msg}, they
#'   are concatenated with a newline between.
#'
#' @param quiet \code{logical} indicator controlling if the message is
#'   generated.
#'
#' @examples
#'  messageq("Hello world", FALSE)
#'  messageq("Hello world", TRUE)
#'
#' @export
#'
messageq <- function(msg = NULL, quiet = FALSE){
  if (!quiet){
    msg2 <- paste(msg, collapse = "\n")
    message(msg2)
  }
}