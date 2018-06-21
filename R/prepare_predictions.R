
#' @title Download the predictions files from the portalPredictions repo
#'
#' @description Download the files into the predictions subdirectory in the
#'   the tree. Due to the volume of files, the function \code{download.file}
#'   is forced quiet.
#'
#' @param tree the name tree of the forecasting directory
#'
#' @return nothing
#'
#' @export
#'
download_predictions <- function(tree = dirtree()){

  from1 <- "https://api.github.com/repos/weecology/portalPredictions/"
  from2 <- "contents/predictions"
  from_path <- paste0(from1, from2)
  req <- GET(from_path)
  stop_for_status(req)
  filelist <- unlist(lapply(content(req), "[", "path"), use.names = FALSE)
  from1 <- "https://raw.github.com/weecology/portalPredictions/master/"
  from_fpaths <- paste0(from1, filelist)

  to_path_choices <- sub_paths(tree)
  to_path <- to_path_choices[grepl("predictions", to_path_choices)]
  to_fpaths <- file_path(tree, filelist)
  
  x <- mapply(download.file, url = from_fpaths, destfile = to_fpaths, 
              quiet = TRUE)

}

