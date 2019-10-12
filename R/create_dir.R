#' @title Create the structure of a forecasting directory
#'
#' @description This suite of functions creates the necessary folder structure
#'  for a forecasting directory as well as a YAML file that tracks the
#'  setup configurations. At each level of the hierarchy, the more
#'  basal folders are required to be present (verified using 
#'  \code{\link{verify}}) and the relevant folders at that level are created 
#'  if not present (using \code{\link{create}}). \cr \cr
#'  \code{create_dir} creates a full directory or any missing parts
#'  and writes the configuration file at \code{filename_config} using
#'  \code{\link{write_directory_config}}. \cr \cr
#'  \code{create_main} creates the main folder of the directory.\cr \cr
#'  \code{create_subs} creates the sub folders of the directory 
#'  (\code{tmp}, \code{raw}, \code{data}, \code{models}, and \code{casts}).
#'
#' @details Folder paths are created internally using  
#'  \code{\link{main_path}} and \code{\link{sub_path}}, such that the user 
#'  only needs to input the main folder's standard \code{main} name input. \cr
#'  The subdirectories are presently hardcoded. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param bline \code{logical} indicator if a horizontal break lines should be
#'  included in messages.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. Default value (\code{"."}) puts the forecasting
#'  directory in the present locations. Nesting the forecasting directory
#'  in a folder can be done by simply adding to the \code{main} input (see
#'  \code{Examples}).
#'
#' @param filename_config \code{character} value of the path to the directory
#'  config YAML.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
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
#' @name directory_creation
#'
NULL

#' @rdname directory_creation
#'
#' @export
#'
create_dir <- function(main = ".", filename_config = "dir_config.yaml", 
                       bline = FALSE, quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  creation_message(main = main, bline = bline, quiet = quiet, 
                   arg_checks = arg_checks)
  create_main(main = main, arg_checks = arg_checks)
  create_subs(main = main, arg_checks = arg_checks)
  write_directory_config(main = main, filename_config = filename_config, 
                         quiet = quiet, arg_checks = arg_checks)
}

#' @rdname directory_creation
#'
#' @export
#'
create_main <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  mainp <- main_path(main = main, arg_checks = arg_checks)
  create(paths = mainp, arg_checks = arg_checks)
}

#' @rdname directory_creation
#'
#' @export
#'
create_subs <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  subs <- c("casts", "models", "raw", "data", "tmp")
  mainp <- main_path(main = main, arg_checks = arg_checks)
  subsp <- sub_path(main = main, subs = subs, arg_checks = arg_checks)
  verify(paths = mainp, arg_checks = arg_checks)
  create(paths = subsp, arg_checks = arg_checks)
}

#' @title Verify that folders exist and create folders 
#'
#' @description 
#'  \code{verify} throws an error if any of the folders (specified by 
#'  \code{path} and named by \code{level}) do not exist. \cr \cr
#'  \code{create} creates a requested folder if it does not already exist.
#'
#' @param paths \code{character} vector of the folder paths.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return 
#'  \code{verify}: throws an error if any of the folders do not exist, 
#'  otherwise \code{NULL}.
#'  \code{create}: \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   mainp <- main_path()
#'   create(mainp, "main")
#'   verify(mainp, "main")
#'  }
#'
#' @name verify_and_create
#'
NULL

#' @rdname verify_and_create
#'
#' @export
#'
verify <- function(paths = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(paths)
  misses <- NULL
  for(i in 1:length(paths)){
    if (!dir.exists(paths[i])){
      misses <- c(misses, paths[i])
    }
  }
  nmisses <- length(misses)
  if (nmisses == 0){
    return(invisible(NULL))
  } 
  if (nmisses == 1){
    msg <- paste0("\n Folder does not exist at ", misses)
  } else{
    msg_m <- paste(misses, collapse = ", ")
    msg <- paste0("\n Folders do not exist at ", msg_m)
  }
  msg2 <- c("Missing directory components", msg, 
             "\n Run `create_dir` to create directory")
  stop(msg2, call. = FALSE)
}

#' @rdname verify_and_create
#'
#' @export
#'
create <- function(paths = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(paths)
  for(i in 1:length(paths)){
    if(!dir.exists(paths[i])){
      dir.create(paths[i])
    }
  }
}