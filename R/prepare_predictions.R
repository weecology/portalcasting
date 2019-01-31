
#' @title Download the predictions files from the portalPredictions repo
#'
#' @description Download the files into the predictions subdirectory in the
#'   the portalcasting directory tree from the 
#'   \href{https://github.com/weecology/portalPredictions/tree/master/predictions}{main repository}.
#'   Due to the volume of files, \code{\link{download.file}} is forced 
#'   \code{quiet = TRUE}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param download \code{logical} indicator of whether the download should 
#'   actually happen. Should be \code{TRUE} except for testing purposes.
#'
#' @export
#'
download_predictions <- function(tree = dirtree(), download = TRUE){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!("logical" %in% class(download))){
    stop("`download` is not logical")
  }
  from1 <- "https://api.github.com/repos/weecology/portalPredictions/"
  from2 <- "contents/predictions"
  from_path <- paste0(from1, from2)
  from3 <- "https://raw.github.com/weecology/portalPredictions/master/"
  to_path_choices <- sub_paths(tree)
  to_path <- to_path_choices[grepl("predictions", to_path_choices)]
  req <- GET(from_path)
  stop_for_status(req)
  filelist <- unlist(lapply(content(req), "[", "path"), use.names = FALSE)
  to_fpaths <- file_path(tree, filelist)
  from_fpaths <- paste0(from3, filelist)

  if (download){
    x <- mapply(download.file, url = from_fpaths, destfile = to_fpaths, 
                quiet = TRUE)
  }
}

