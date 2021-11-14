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
#' @return \code{list} of prepared \code{datasets}.
#'   
#'
#' @name prepare_rodents
#'
NULL


#' @rdname prepare_rodents
#'
#' @export
#'
prepare_rodents <- function (main     = ".",
                             datasets = prefab_rodent_datasets(),
                             quiet    = FALSE,
                             verbose  = FALSE) {


  return_if_null(datasets)

  envr <- environment()
  data(rodent_datasets, envir = envr)
  

  datasets_list <- rodent_datasets[datasets]



  messageq(" - rodents datasets", quiet = quiet)

  ndatasets <- length(datasets_list)
  out <- vector("list", ndatasets)
  for (i in 1:ndatasets) {

    args <- update_list(orig_list = datasets_list[[i]]$args, 
                        main      = main, 
                        quiet     = quiet, 
                        verbose   = verbose)

    out[[i]] <- do.call(datasets_list[[i]]$fun, args)
  
  }
  names(out) <- datasets
  invisible(out)

}













