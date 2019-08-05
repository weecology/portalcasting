#' @title Create the structure of a forecasting directory
#'
#' @description This suite of functions creates the necessary folder structure
#'  for a forecasting directory. At each level of the hierarchy, the more
#'  basal folders are required to be present (verified using 
#'  \code{\link{verify}}) and the relevant folders at that level are created 
#'  if not present (using \code{\link{create}}). \cr \cr
#'  \code{create_dir} creates a full directory or any missing parts. \cr \cr
#'  \code{create_base} creates the base folder of the directory.\cr \cr
#'  \code{create_main} creates the main folder of the directory.\cr \cr
#'  \code{create_subs} creates the sub folders of the directory.
#'
#' @details Alternatively or in addition to the \code{tree} argument, users 
#'  can input the folder locations directly via \code{base}, \code{main}, and 
#'  \code{subs} arguments. In this case, any non-\code{NULL} input for 
#'  \code{base}, \code{main}, or \code{subs} overrides the respective 
#'  element in \code{tree}. \cr \cr
#'  Folder paths are created internally using \code{\link{base_path}}, 
#'  \code{\link{main_path}}, and \code{\link{sub_paths}}, such that the user 
#'  only needs to input folder locations as names within the respective level 
#'  of the directory tree structure, not as pre-formatted file paths. See 
#'  \code{Examples}.
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
#' @param subs \code{character} vector of the names of the sub components of
#'  the directory tree. See \code{Details}.
#'
#' @return All \code{create_} functions return \code{NULL}.
#'
#' @examples
#'   create_dir()
#'   create_dir(base = "base_folder", main = "main_folder")
#'   create_base()
#'   create_main()
#'   create_subs()
#'
#' @export
#'
create_dir <- function(tree = dirtree(), base = NULL, main = NULL, 
                       subs = NULL, quiet = FALSE){
  create_base(tree, base, quiet)
  create_main(tree, base, main, quiet)
  create_subs(tree, base, main, subs, quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_base <- function(tree = dirtree(), base = NULL, quiet = FALSE){
  tree <- update_tree(tree, base)
  base <- base_path(tree)
  create(base, "base", quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_main <- function(tree = dirtree(), base = NULL, main = NULL,
                        quiet = FALSE){
  tree <- update_tree(tree, base, main)
  base <- base_path(tree)
  main <- main_path(tree)
  verify(base, "base")
  create(main, "main", quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_subs <- function(tree = dirtree(), base = NULL, main = NULL, 
                        subs = NULL, quiet = FALSE){
  tree <- update_tree(tree, base, main, subs)
  base <- base_path(tree)
  main <- main_path(tree)
  subs <- sub_paths(tree)
  verify(c(base, main), c("base", "main"))
  create(subs, basename(subs), quiet)
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
#'  tree <- dirtree()
#'  base <- base_path(tree)
#'  verify(base, "base")
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
#'  tree <- dirtree()
#'  base <- base_path(tree)
#'  create(base, "base")
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
