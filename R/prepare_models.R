#' @title Update models based on user input controls
#'
#' @description Update model scripts based on the user-defined model control
#'  inputs. This allows users to define their own models or re-define prefab
#'  models within the \code{\link{portalcast}} pipeline.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to update.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param update_prefab_models \code{logical} indicator if all of the models'
#'  scripts should be updated, even if they do not have an explicit change
#'  to their model options via \code{controls_model}. Default is
#'  \code{FALSE}, which leads to only the models in \code{controls_model}
#'  having their scripts re-written. Switching to \code{TRUE} results in the
#'  models listed in \code{models} having their scripts re-written. \cr \cr
#'  This is particularly helpful when one is changing the global (with respect
#'  to the models) options \code{main}, \code{quiet}, \code{verbose}, or
#'  \code{control_files}.
#'
#' @return \code{NULL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   cm <- model_control(name = "AutoArima", data_sets = "all")
#'   update_models(controls_model = cm)
#'  }
#'
#' @export
#'
update_models <- function(main = ".", models = NULL,
                          update_prefab_models = FALSE, 
                          control_files = files_control(),
                          quiet = FALSE, verbose = FALSE){

  if(update_prefab_models){
    models <- unique(c(models, prefab_models()))
  } 
  return_if_null(models)

  messageq("Updating model scripts", quiet = quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, 
                model = models[i])
  }
  messageq(message_break(), quiet = quiet) 
  invisible(NULL)
}



#' @title Verify that models requested to cast with exist
#'
#' @description Verify that models requested have scripts in the models 
#'   subdirectory. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param models \code{character} vector of the names of models to verify.
#'
#' @return \code{NULL} with messaging. 
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   verify_models()
#'  }
#'
#' @export
#'
verify_models <- function(main = ".", models = prefab_models(), 
                          quiet = FALSE){
  messageq("Checking model availability", quiet = quiet)
  model_dir <- sub_path(main = main, subs = "models")
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist", call. = FALSE)
  }
  available <- list.files(model_dir)
  if (models[1] != "all"){
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      msg <- paste0("Requested model(s) ", missmod, " not in directory \n")
      stop(msg, call. = FALSE)
    }
  }
  messageq(" *All requested models available*", quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)
}








#' @title Provide the Names of the Prefabricated Models
#'
#' @description Create a \code{character} vector of the names of the models to be included including the pre-fabricated (prefab) models. \cr \cr
#'              The currently prefabricated models include \code{"AutoArima"}, \code{"ESSS"}, \code{"NaiveArima"}, \code{"nbGARCH"}, \code{"nbsGARCH"}, \code{"pevGARCH"}, and \code{"jags_RW"}. 
#'
#' @return \code{character} vector of model names.
#'
#' @examples
#'  prefab_models()
#'
#' @export
#'
prefab_models <- function () {

  c("AutoArima", "ESSS", "NaiveArima", "nbGARCH", "nbsGARCH", "pevGARCH", "jags_RW")

}


#' @title Write Model Function Script into Directory
#'
#' @description Writes a model's function as a script into the defined directory. \cr \cr
#'              \code{model} can be input as a \code{character} string, symbol (backquoted name), or \code{function}, as \code{\link{match.fun}}
#'
#' @param main \code{character} value defining the main component of the portalcasting directory tree. 
#'
#' @param model \code{character} name of a model function, the \code{function} itself, or its symbol (backquoted name).
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
write_model <- function (main  = ".", 
                         model = NULL, 
                         quiet = FALSE){

  return_if_null(model)

  messageq("   -", model, quiet = quiet)

  FUN <- match.fun(model)
  FUN_char <- format(FUN)
  out <- c(paste0(model, " <- "), FUN_char)

  model_file <- paste0(model, ".R")
  dest <- file.path(main, "models", model_file)

  write(out, dest)

  invisible()

}

