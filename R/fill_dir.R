#' @title Fill a forecasting directory with basic components.
#'
#' @description Fill the forecasting directory with basic components.
#'
#' @details Arguments input directly here take precedence over those in the 
#'  \code{downloads} \code{list}.
#'
#' @param downloads \code{list} or \code{list} of \code{list}s containing
#'  inputs to \code{\link{download}} for each download to be put into the 
#'  raw subdirectory. 
#'
#' @param tree Directory tree \code{list}. Consisting of \code{base}, 
#'  \code{main}, and \code{subs} elements. See \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param base \code{character} value of the name of the base component of
#'  the directory tree. See \code{Details}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. See \code{Details}.
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @return All \code{fill_} functions return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   data(PortalPredictions)
#'   create_dir()
#'   fill_raw(PortalPredictions$downloads)
#'  }
#'
#' @export
#'
fill_dir <- function(downloads = NULL, tree = dirtree(), base = NULL,
                     main = NULL, quiet = FALSE, cleanup = TRUE){
  fill_raw(downloads, tree, base, main, quiet, cleanup)
}

#' @rdname fill_dir
#'
#' @export
#'
fill_raw <- function(downloads = NULL, tree = dirtree(), base = NULL,
                     main = NULL, quiet = FALSE, cleanup = TRUE){
  if(!is.null(downloads)){
    if(list_depth(downloads) == 1){
      downloads <- list(downloads)
    }
    messageq("Downloading raw files", quiet)
    ndl <- length(downloads)
    for(i in 1:ndl){
      downloads[[i]]$cleanup <- ifnull(downloads[[i]]$cleanup, cleanup)
      downloads[[i]]$tree <- ifnull(downloads[[i]]$tree, tree)
      downloads[[i]]$base <- ifnull(downloads[[i]]$base, base)
      downloads[[i]]$main <- ifnull(downloads[[i]]$main, main)
      downloads[[i]]$quiet <- ifnull(downloads[[i]]$quiet, quiet)
      do.call(download, downloads[[i]])
    }
  }
}