
#' @title Download the predictions files from the Portal Predictions repo
#'
#' @description Download the files into the predictions subdirectory in the
#'   the portalcasting directory tree from the 
#'   \href{https://bit.ly/2IFuPd7}{main repository}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param download \code{logical} indicator of whether the download should 
#'   actually happen. Should be \code{TRUE} except for testing purposes.
#'
#' @param quiet \code{logical} indicator of whether the download messages 
#'   are returned to the console.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' download_predictions()
#' }
#'
#' @export
#'
download_predictions <- function(tree = dirtree(), download = TRUE, 
                                 quiet = FALSE){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!("logical" %in% class(download))){
    stop("`download` is not logical")
  }
  if (!("logical" %in% class(quiet))){
    stop("`quiet` is not logical")
  }
  from1 <- "https://api.github.com/repos/weecology/portalPredictions/"
  from2 <- "contents/predictions"
  from_path <- paste0(from1, from2)
  from3 <- "https://raw.github.com/weecology/portalPredictions/master/"
  to_path_choices <- sub_paths(tree)
  to_path <- to_path_choices[grepl("predictions", to_path_choices)]
  if (download){
    if (!quiet){
      message("Downloading predictions files")
    }
    req <- GET(from_path)
    stop_for_status(req)
    filelist <- unlist(lapply(content(req), "[", "path"), use.names = FALSE)
    to_fpaths <- file_path(tree, filelist)
    from_fpaths <- paste0(from3, filelist)
    x <- mapply(download.file, url = from_fpaths, destfile = to_fpaths, 
                quiet = quiet)
  }
}

