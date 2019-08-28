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
#'  \code{\link{main_path}} and \code{\link{sub_paths}}, such that the user 
#'  only needs to input the main folder's standard \code{main} name input. \cr
#'  The subdirectories are presently hardcoded. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or just tidy messages. 
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
#' @name create
#'
NULL

#' @rdname create
#'
#' @export
#'
create_dir <- function(main = ".", filename_config = "dir_config.yaml", 
                       quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  main_path <- main_path(main = main, arg_checks = arg_checks)
  msg <- paste0("Establishing portalcasting directory at ", main_path)
  messageq(msg = msg, quiet = quiet)
  create_main(main = main, quiet = quiet, verbose = verbose, 
              arg_checks = arg_checks)
  create_subs(main = main, quiet = quiet, verbose = verbose, 
              arg_checks = arg_checks)
  write_directory_config(main = main, filename_config = filename_config, 
                         quiet = quiet, arg_checks = arg_checks)
}

#' @rdname create
#'
#' @export
#'
create_main <- function(main = ".", quiet = FALSE, verbose = FALSE,
                        arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  mainp <- main_path(main = main, arg_checks = arg_checks)
  create(paths = mainp, names = "main", quiet = quiet, verbose = verbose, 
         arg_checks = arg_checks)
}

#' @rdname create
#'
#' @export
#'
create_subs <- function(main = ".", quiet = FALSE, 
                        verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  subs <- c("casts", "models", "raw", "data", "tmp")
  mainp <- main_path(main = main, arg_checks = arg_checks)
  subsp <- sub_path(main = main, subs = subs, arg_checks = arg_checks)
  verify(paths = mainp, names = "main", arg_checks = arg_checks)
  create(paths = subsp, names = basename(subsp), quiet = quiet,
         verbose = verbose, arg_checks = arg_checks)
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
#' @param names \code{character} vector of the names of the levels in the
#'  hierarchy at which the folders will exist or the folder names. For the
#'  purposes of messaging and not inherently required to be non-\code{NULL}.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages). 
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
#'   main <- main_path()
#'   create(main, "main")
#'   verify(main, "main")
#'  }
#'
#' @name verify_and_create
#'
NULL

#' @rdname verify_and_create
#'
#' @export
#'
verify <- function(paths = NULL, names = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(paths)
  for(i in 1:length(paths)){
    if (!dir.exists(paths[i])){
      msg <- paste0(names[i], " folder does not exist at ", paths[i])
      stop(msg)
    }
  }
}

#' @rdname verify_and_create
#'
#' @export
#'
create <- function(paths = NULL, names = NULL, quiet = FALSE, verbose = FALSE, 
                   arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(paths)
  for(i in 1:length(paths)){
    if (!dir.exists(paths[i])){
      if(names[i] == "main"){ 
        msg <- paste0(" -", names[i])
      } else{
        msg <- paste0("  -", names[i])
      }
      if(verbose){
        msg <- paste0(msg, " at ", paths[i])
      }
      messageq(msg, quiet)
      dir.create(paths[i])
    }
  }
}