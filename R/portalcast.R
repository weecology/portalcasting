#' @title Cast Portal Rodents Models
#'
#' @description Cast the Portal rodent population data using the (updated if needed) data and models in a portalcasting directory. \cr \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of multiple models, with data preparation as needed between runs occurring via \code{prep_data}. \cr \cr
#'  \code{cast} runs a single cast of multiple models across data sets.
#'
#' @details Multiple models can be run together on the multiple, varying data sets for a single new moon using \code{cast}. \code{portalcast} wraps around \code{cast}, providing updating, verification, and resetting utilities as well as facilitating multiple runs of \code{cast} across new moons (e.g., when using a rolling origin).
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param end_moons,end_moon \code{integer} (or integer \code{numeric}) newmoon number(s) of the last sample(s) to be included. Default value is \code{NULL}, which equates to the most recently included sample. \cr
#'  \strong{\code{end_moons} allows for multiple moons, \code{end_moon} can only be one value}.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of the cast). In the recurring forecasting, is set to today's date using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number of the first sample to be included. Default value is \code{217}, corresponding to \code{1995-01-01}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param datasets \code{character} vector of dataset names to be created. 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @return Results are saved to files, \code{NULL} is returned \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = "ESSS")
#'   cast()
#'  }
#'
#' @export
#'
portalcast <- function (main         = ".", 
                        models       = prefab_models(), 
                        datasets     = prefab_datasets(),
                        end_moons    = NULL, 
                        start_moon   = 217, 
                        cast_date    = Sys.Date(),
                        settings     = directory_settings(),
                        multiprocess = FALSE,
                        quiet        = FALSE,
                        verbose      = FALSE){

#
# the datasets here should come from the models selected
#  and not as an argument ... or? maybe not? idk. think this out.
# 

  return_if_null(models)

  messageq(message_break(), "\nPreparing directory for casting\n", message_break(), "\nThis is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", message_break(), quiet = quiet)

  systemType <- .Platform$OS.type

  if (multiprocess != TRUE) {
    systemType <- multiprocess
  }
  if (multiprocess != FALSE) {
    messageq(message_break(), "Running on ", systemType, " System", message_break())
    if(systemType == "unix") {
      messageq("Will be using Forking approach for parallel processing.", message_break())
    } else {
      messageq("Will be using Socket approach for parallel processing.", message_break())
    }
  }

  moons <- read_moons(main     = main,
                      settings = settings)

  which_last_moon <- max(which(moons$newmoondate < cast_date))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moons       <- ifnull(end_moons, last_moon)
  nend_moons      <- length(end_moons)

  cast_f <- function(i) {

    cast(main       = main, 
         datasets   = datasets,
         models     = models, 
         end_moon   = end_moons[i], 
         start_moon = start_moon, 
         cast_date  = cast_date, 
         settings   = settings,
         multiprocess = systemType,
         quiet      = quiet, 
         verbose    = verbose)

  }

  if(FALSE) {
    #This is breaking as of now since it just overwrites preparatory data.
    mclapply(1:nend_moons, cast_f, mc.cores = detectCores())
  } else {
    lapply(1:nend_moons, cast_f)
  }

  if (end_moons[nend_moons] != last_moon) {
 # this maybe should happen within cast?
    messageq(message_break(), "\nResetting data to most up-to-date versions\n", message_break(), quiet = quiet)

    fill_data(main         = main, 
              datasets     = datasets,
              models       = models,
              settings     = settings,
              multiprocess = systemType,
              quiet        = quiet, 
              verbose      = verbose)

  }

  messageq(message_break(), "\nCasting complete\n", message_break(), quiet = quiet)
  invisible() 

} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function (main       = ".", 
                  models     = prefab_models(), 
                  datasets   = prefab_datasets(),
                  end_moon   = NULL, 
                  start_moon = 217, 
                  cast_date  = Sys.Date(), 
                  settings   = directory_settings(), 
                  multiprocess = FALSE,
                  quiet      = FALSE, 
                  verbose    = FALSE) {

  moons <- read_moons(main     = main,
                      settings = settings)

  which_last_moon <- max(which(moons$newmoondate < cast_date))
  last_moon       <- moons$newmoonnumber[which_last_moon]
  end_moon        <- ifnull(end_moon, last_moon)

  messageq(message_break(), "\nReadying data for forecast origin newmoon ", end_moon, "\n", message_break(), quiet = quiet)

  if (end_moon != last_moon) {

    fill_data(main     = main, 
              datasets = datasets,
              models   = models,
              settings = settings,
              multiprocess = multiprocess,
              quiet    = quiet, 
              verbose  = verbose)

  }

  messageq(message_break(), "\nRunning models for forecast origin newmoon ", end_moon, "\n", message_break(), quiet = quiet)

  models_scripts <- models_to_cast(main     = main, 
                                   models   = models,
                                   settings = settings)

  nmodels <- length(models)

  model_f <- function(i) {

    model <- models_scripts[i]

    messageq(message_break(), "\n -Running ", path_no_ext(basename(model)), "\n", message_break(), quiet = quiet)

    run_status <- tryCatch(expr  = source(model, local=TRUE),
                           error = function(x){NA})

    if (all(is.na(run_status))) {

      messageq("  |----| ", path_no_ext(basename(model)), " failed |----|", quiet = quiet)

    } else {

      messageq("  |++++| ", path_no_ext(basename(model)), " successful |++++|", quiet = quiet)

    }

  }

  if(multiprocess == 'unix') {

    models_list <- lapply(1:nmodels, function(i) {
      mcparallel(model_f(i))
    })

    mccollect(models_list)


  } else if (multiprocess == 'windows') {

    clusters <- makeCluster(detectCores() - 1, outfile = "")

    clusterExport(cl=clusters, varlist=c('models_scripts'), envir=environment())

    parLapply(clusters, 1:nmodels, function(i) {
      model_f(i)
    })

    stopCluster(clusters)

  } else {

    for (i in 1:nmodels) {

      model_f(i)

    }

  }
  

  invisible()

}




#' @title Determine the Paths to the Scripts for Models to Cast with
#'
#' @description Translate a \code{character} vector of model name(s) into a \code{character} vector of file path(s) corresponding to the model scripts. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{character} vector of the path(s) of the R script file(s) to be run.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   models_to_cast()
#'  }
#'
#' @export
#'
models_to_cast <- function (main     = ".", 
                            models   = prefab_models(),
                            settings = directory_settings()) {
  
  models_path <- file.path(main, settings$subs$models)
  file_names  <- paste0(models, ".R")
  torun       <- list.files(models_path) %in% file_names
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 

  normalizePath(torun_paths)

}