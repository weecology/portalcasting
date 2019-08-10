#' @title Create the structure of a forecasting directory
#'
#' @description This suite of functions creates the necessary folder structure
#'  for a forecasting directory. At each level of the hierarchy, the more
#'  basal folders are required to be present (verified using 
#'  \code{\link{verify}}) and the relevant folders at that level are created 
#'  if not present (using \code{\link{create}}). \cr \cr
#'  \code{create_dir} creates a full directory or any missing parts. \cr \cr
#'  \code{create_main} creates the main folder of the directory.\cr \cr
#'  \code{create_subs} creates the sub folders of the directory.
#'
#' @details Folder paths are created internally using  
#'  \code{\link{main_path}} and \code{\link{sub_paths}}, such that the user 
#'  only needs to input folder locations as names within the respective level 
#'  of the directory tree structure, not as pre-formatted file paths. See 
#'  \code{Examples}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. Default value (\code{"."}) puts the forecasting
#'  directory in the present locations. Nesting the forecasting directory
#'  in a folder can be done by simply adding to the \code{main} input (see
#'  \code{Examples}).
#'
#' @param subs \code{character} vector of the names of the sub components of
#'  the directory tree. Generally shouldn't be changed.
#'
#' @return All \code{create_} functions return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   create_dir(main = "./main_folder")
#'   create_dir(main = ".\\main_folder")
#'   create_main()
#'   create_subs()
#'  }
#'
#' @export
#'
create_dir <- function(main = ".", subs = subdirs(), quiet = FALSE){
  create_main(main, quiet)
  create_subs(main, subs, quiet)
}



#' @rdname create_dir
#'
#' @export
#'
create_main <- function(main = ".", quiet = FALSE){
  mainp <- main_path(main)
  create(mainp, "main", quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_subs <- function(main = ".", subs = subdirs(), quiet = FALSE){
  mainp <- main_path(main)
  subsp <- sub_paths(main, subs)
  verify(mainp, "main")
  create(subsp, basename(subsp), quiet)
}

#' @title Verify that a required folder exists
#'
#' @description If any of the folders (specified by \code{path} and named by
#'  \code{level}) do not exist, the operation throws an error.
#'
#' @param path \code{character} vector of the folder paths.
#'
#' @param level \code{character} vector of the names of the levels in the
#'  hierarchy at which the folders will exist or the folder names. For the
#'  purposes of messaging and not inherently required to be non-\code{NULL}.
#'
#' @return Throws an error if any of the folders do not exist, otherwise
#'  \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   main <- main_path()
#'   verify(main, "main")
#'  }
#'
#' @export
#'
verify <- function(path = NULL, level = NULL){
  if(!is.null(path)){
    for(i in 1:length(path)){
      if (!dir.exists(path[i])){
        msg <- paste0(level[i], " folder does not exist at ", path[i])
        stop(msg)
      }
    }
  }
}

#' @title Create a specific folder
#'
#' @description If folders (specified by \code{path} and named by
#'  \code{level}) do not exist, they are created, with a message or not as
#'  indicated by \code{quiet}.
#'
#' @param path \code{character} vector of the folder paths.
#'
#' @param level \code{character} vector of the names of the levels in the
#'  hierarchy at which the folders will exist or the folder names. For the
#'  purposes of messaging and not inherently required to be non-\code{NULL}.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   main <- main_path()
#'   create(main, "main")
#'  }
#'
#' @export
#'
create <- function(path = NULL, level = NULL, quiet = FALSE){
  if(!is.null(path)){
    for(i in 1:length(path)){
      if (!dir.exists(path[i])){
        msg <- paste0("Creating ", level[i], " directory at ", path[i])
        messageq(msg, quiet)
        dir.create(path[i])
      }
    }
  }
}
