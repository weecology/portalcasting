#' @title Set up a full forecasting directory
#'
#' @description Create (via \code{\link{create_dir}}) and populate (via 
#'  \code{\link{fill_dir}}) a full portalcasting directory, with 
#'  specifications controlled via list inputs. Default behavior is
#'  to setup the directory for using all of the "prefab" models
#'  (AutoArima, ESSS, nbGARCH, nbsGARCH, and pevGARCH; see the
#'  \href{https://bit.ly/2xP9jKI}{model description site}). \cr \cr
#'  \code{setup_sandbox} creates a portalcasting directory with defaults set
#'  for optimal sandboxing experience (see Details).
#'
#' @details The default behavior is to over-write existing files, which can
#'  be prevented for specific components by setting the control argument
#'  \code{update} to \code{FALSE}. \cr \cr
#'  Sandboxing: \code{setup_dir}'s defaults are designed for working on the
#'  forecasting server, namely it doesn't download the most recent set of 
#'  forecasting files because it has them already. However, sandbox users
#'  will want all files (including previous covariate forecasts and previous
#'  rodent forecasts) that the forecasting server has. Thus, default 
#'  \code{setup_sandbox} is functionally the same as \code{setup_dir} with the
#'  two \code{download} arguments (in \code{predictions_control} and
#'  \code{data_control}) set to \code{TRUE}.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param PortalData_control A \code{list} of arguments to control the filling
#'  of the PortalData subdirectory. See \code{\link{PortalData_control}}. 
#'  Values not set assume defaults.
#'
#' @param data_control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'
#' @param predictions_control A \code{list} of arguments to control the 
#'  filling of the predictions subdirectory. See 
#'  \code{\link{predictions_control}}. Values not set assume defaults.
#'
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{NULL}, which retrieves the model names 
#'  from \code{models_control}, so the models returned by the default
#'  settings of \code{\link{model_names}}.
#'
#' @param models_control A \code{list} of arguments to control the 
#'  filling of the models subdirectory. See \code{\link{models_control}}.
#'  Arguments not specified assume default values.
#'
#' @examples
#'\dontrun{
#'
#'  setup_dir()
#'  setup_sandbox()
#'}
#'
#' @export
#'
setup_dir <- function(models = NULL, tree = dirtree(), quiet = FALSE, 
                      PortalData_control = list(), data_control = list(), 
                      predictions_control = list(),
                      models_control = list()){
  version_number <- packageDescription("portalcasting", fields = "Version")
  msg <- paste0("This is portalcasting v", version_number)
  messageq(msg, quiet)
  models_control <- do.call("models_control", models_control)
  models <- ifnull(models, names(models_control))
  create_dir(tree = tree, quiet = quiet)
  fill_dir(models, tree, quiet, PortalData_control, data_control,
           predictions_control, models_control)
}

#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(models = NULL, 
                          tree = dirtree(), quiet = FALSE, 
                          PortalData_control = list(), 
                          data_control = list(download = TRUE), 
                          predictions_control = list(download = TRUE),
                          models_control = list()){

  setup_dir(models, tree, quiet, PortalData_control, data_control,
            predictions_control, models_control)
  sandbox_welcome(tree, quiet)
}


#' @title Create the structure of a portalcasting directory
#'
#' @description This suite of functions creates the necessary folder structure
#'  for a portalcasting directory. At each level of the hierarchy, the more
#'  basal folders are required to be present and the relevant folders at that
#'  level are created if not present. \cr \cr
#'  \code{create_dir}: Create a full portalcasting directory or 
#'  any missing components. \cr \cr
#'  \code{create_main_dir}: Create the main level of the 
#'  portalcasting directory. \cr \cr
#'  \code{create_sub_dirs}: Create the sub level folders of the 
#'  portalcasting directory. \cr \cr
#'  \code{create_sub_dir}: Create a specific subdirectory 
#'  folder if it does not already exist.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @examples
#'   \dontrun{
#'
#'   create_dir()
#'   create_base_dir()
#'   create_main_dir()
#'   create_sub_dirs()
#'   create_sub_dir()
#'   }
#'
#' @export
#'
create_dir <- function(tree = dirtree(), quiet = FALSE){
  create_base_dir(tree, quiet)
  create_main_dir(tree, quiet)
  create_sub_dirs(tree, quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_base_dir <- function(tree = dirtree(), quiet = FALSE){
  tree <- ifnull(tree, dirtree(base))
  base <- base_path(tree)
  create(base, "base", quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_main_dir <- function(tree = dirtree(), quiet = FALSE){
  base <- base_path(tree)
  main <- main_path(tree)
  verify(base, "base")
  create(main, "main", quiet)
}

#' @rdname create_dir
#'
#' @export
#'
create_sub_dirs <- function(tree = dirtree(), quiet = FALSE){
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
#' @param level \code{character} vector of the folder names.
#'
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
#' @param level \code{character} vector of the folder names.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
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

#' @title Populate the components of a portalcasting directory
#'
#' @description Fill each of the components of the directory. \cr \cr
#'   \code{fill_dir}: populate the files of an existing 
#'   portalcasting directory structure in full. (Future flexibility will 
#'   allow for selective population of components.) \cr \cr
#'   \code{fill_portalData}: populate the components of the PortalData 
#'   folder. \cr \cr
#'  \code{fill_data}: populate the for-use data folder in the 
#'  portalcasting directory, including the historical covariates, trapping 
#'  table, moons, rodents, covariates, and metadata files. \cr \cr
#'  \code{fill_predictions}: populate the predictions folder with 
#'  existing predictions housed on the main portalPredictions repo site.
#'  \cr \cr
#'  \code{fill_models}: Populate the model folder with specified 
#'  model scripts to be run.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param PortalData_control A \code{list} of arguments to control the filling
#'  of the PortalData subdirectory. See \code{\link{PortalData_control}}. 
#'   Arguments not specified assume default values.
#' 
#' @param data_control A \code{list} of arguments to control the filling of
#'   the data subdirectory. See \code{\link{data_control}}. 
#'   Arguments not specified assume default values.
#'
#' @param models_control A \code{list} of arguments to control the 
#'   filling of the models subdirectory. See \code{\link{models_control}}.
#'   Arguments not specified assume default values.
#'
#' @param predictions_control A \code{list} of arguments to control the 
#'   filling of the predictions subdirectory. See 
#'   \code{\link{predictions_control}}. Arguments not specified assume default
#'   values.
#' 
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{NULL}, which retrieves the model names 
#'  from \code{models_control}, so the models returned by the default
#'  settings of \code{\link{model_names}}.
#'
#' @param control A \code{list} of controls specific to the function.
#'   
#' @examples
#' \dontrun{
#'
#' create_dir()
#' fill_dir()
#' fill_PortalData()
#' fill_data()
#' fill_predictions()
#' fill_models()
#' }
#' 
#' @export
#'
fill_dir <- function(models = NULL, 
                     tree = dirtree(), quiet = FALSE, 
                     PortalData_control = list(), data_control = list(),
                     predictions_control = list(),
                     models_control = list()){
  models_control <- do.call("models_control", models_control)
  models <- ifnull(models, names(models_control))
  fill_PortalData(tree, quiet, PortalData_control)
  fill_data(tree, quiet, data_control)
  fill_predictions(tree, quiet, predictions_control)
  fill_models(models, tree, quiet, models_control)
}

#' @rdname fill_dir
#'
#' @export
#'
fill_PortalData <- function(tree = dirtree(), quiet = FALSE, 
                            control = list()){
  control <- do.call("PortalData_control", control)
  if(control$update){
    messageq("Downloading raw data into PortalData subdirectory", quiet)
    main_folder <- main_path(tree)
    version <- control$version 
    from_zenodo <- control$from_zenodo
    PD <- download_observations(main_folder, version, from_zenodo)
  }
}

#' @rdname fill_dir
#'
#' @export
#'
fill_data <- function(tree = dirtree(), quiet = FALSE, control = list()){
  verify_PortalData(tree = tree, quiet = quiet)
  control <- do.call("data_control", control)
  if(control$update){
    messageq("Loading forecasting data files into data subdirectory", quiet)
    download_hist_covariate_forecasts(tree, quiet, control$covariates)
    transfer_trapping_table(tree, quiet)
    moons <- prep_moons(tree, quiet, control$moons)
    rodents_list <- prep_rodents_list(moons, tree, quiet, control$rodents)
    covariates <- prep_covariates(moons, tree, quiet, control$covariates)
    met <- prep_metadata(moons, rodents_list, covariates, tree, quiet, 
                         control$metadata)
  }
}

#' @rdname fill_dir
#'
#' @export
#'
fill_predictions <- function(tree = dirtree(), quiet = FALSE, 
                             control = list()){
  control <- do.call("predictions_control", control)
  if (control$download){
    messageq("Downloading predictions into predictions subdirectory", quiet)
    download_predictions(tree, quiet, control)
  }
}

#' @rdname fill_dir
#'
#' @export
#'
fill_models <- function(models = NULL, 
                        tree = dirtree(), quiet = FALSE,
                        control = list()){

  control <- do.call("models_control", control)
  models <- ifnull(models, names(control))
  nmods <- length(models)
  if(nmods > 0){
    messageq("Adding models to models subdirectory:", quiet)
    for (i in 1:nmods){
      model <- models[i]
      needs <- control[[model]]
      covar <- needs$covariates
      lag <- needs$lag
      update <- needs$update
      mod <- write_model(model, covar, lag, update, tree, quiet)
    }
  }
}

#' @title Verify that the PortalData subdirectory is present and has required 
#'   data
#'
#' @description Check that the PortalData subdirectory exists and has the
#'   needed specified file(s) within the "Rodents" subdirectory. If any of
#'    the file(s) is(/are) not present, the (entire) directory is filled.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param foldername \code{character}-valued vector of name(s) of the 
#'   folder(s) to use for specific checking. Default (\code{Rodents}) 
#'   settings use the moon dates table in the Rodents folder.
#'
#' @param filename \code{character}-valued vector of name(s) of the file(s) to
#'   use for specific checking. Default (\code{moon_dates.csv})
#'   settings use the moon dates table in the Rodents folder.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#' 
#' @param control A \code{list} of arguments to control the filling
#'   of the PortalData subdirectory. See \code{\link{PortalData_control}}. 
#'   Arguments not specified assume default values.
#'
#' @examples
#' \dontrun{
#'
#' setup_dir()
#' verify_PortalData()
#' }
#'
#' @export
#' 
verify_PortalData <- function(tree = dirtree(), foldername = "Rodents",
                              filename = "moon_dates.csv", quiet = FALSE,
                              control = list()){
  lpath <- paste0("PortalData/", foldername, "/", filename)
  path <- file_paths(tree, lpath) 
  if (!all(file.exists(path))){
    messageq("PortalData missing, downloading now", quiet)
    create_dir(tree, quiet)
    control <- do.call("PortalData_control", control)
    fill_PortalData(tree, quiet, control)
  } else{
    messageq("PortalData present", quiet)
  }
}

#' @title Remove the temporary files in PortalData and tmp
#'
#' @description Remove content in specified (by \code{options_dir$to_cleanup})
#'   subdirectories and the subdirectories themselves.
#'
#' @param to_cleanup \code{character} vector of subdirectory names to
#'   cleanup after completion of casting. Defaults to the temp and PortalData
#'   subdirectories: \code{c("tmp", "PortalData")}.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'   \code{base}, \code{main}, and \code{subs} elements. See
#'   \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'   quieted.
#' 
#' @examples
#' \dontrun{
#'
#' setup_dir()
#' portalcast()
#' cleanup_dir()
#' }
#'
#' @export
#' 
cleanup_dir <- function(to_cleanup = c("tmp", "PortalData"),
                        tree = dirtree(), quiet = FALSE){
  if (is.null(to_cleanup)){
    subs <- NULL
    msg <- "no subdirectories requested to be removed"
  } else{
    subs <- sub_paths(tree, to_cleanup)
    to_cleanup2 <- paste(to_cleanup, collapse = " ")
    msg <- paste("Removing", to_cleanup, "subdirectories", sep = " ")
    mm <- sapply(msg, messageq, quiet = quiet)
  } 
  cleaned <- sapply(subs, unlink, recursive = TRUE, force = TRUE)
}

#' @title Clean a tmp subdirectory
#'
#' @description Clean (\code{clean_tmp}) the tmp subdirectory specifically.
#'
#' @details If the tmp subdirectory does not exist, it will be created if
#'   possible (if the basal directory exists).
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'   quieted.
#'
#' @export
#'
clear_tmp <- function(tree = dirtree(), quiet = FALSE){
  check_args()
  temp_dir <- sub_paths(tree, "tmp")
  tree_tmp <- dirtree(tree$base, tree$main, "tmp")
  if (!dir.exists(temp_dir)){
    create_sub_dirs(tree_tmp, quiet)
  }
  if (length(list.files(temp_dir)) > 0){
    messageq("cleaning tmp directory", quiet)
    file.remove(file_paths(tree, paste0("tmp/", list.files(temp_dir))))
  }
}


#' @title Sandbox welcome
#'
#' @description Create a welcome message for the sandbox
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'   \code{base}, \code{main}, and \code{subs} elements. See
#'   \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress message should be
#'   quieted.
#' 
#' @export
#' 
sandbox_welcome <-function(tree = dirtree(), quiet = FALSE){

main <- main_path(tree)
castle <- "
                                         ____
             /\\                         / -- )   
            /  \\                       (____/
           /|  |\\                       / /  
          /_|__|_\\                     / / 
          |      |                    / /
 __    __ |      | __    __          / / 
[  ]__[  ].      .[  ]__[  ]        / /  
|__         ____         __|  ____ / /__ 
   |      .|    |.      |    / .------  )
   |      |      |      |   / /      / / 
   |      |      |      |  / /      / /  
~~~~~~~~~~~~~~~~~~~~~~~~~~------------~~~~~~~~~~~~~~
"
succ <- "sanbox directory successfully set up at \n"
happy <- "\nhappy portalcasting!"
msg <- paste0(castle, succ, main, happy)

messageq(msg, quiet)
}
