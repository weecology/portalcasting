
#' @title Evaluate Portalcasting Forecasts
#'
#' @description Evaluate forecasts.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast group to combine with an ensemble. If \code{NULL} (default), the most recent cast group is ensembled. 
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting ensembling, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param dataset \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to the first data set encountered.
#'
#' @param include_interp \code{logical} indicator of if the basic data set names should also be inclusive of the associated interpolated data sets.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @return Nothing yet
#'
#' @export
#'
evaluate_casts <- function (main           = ".", 
                            settings       = directory_settings(),
                            cast_groups    = NULL, 
                            cast_ids       = NULL, 
                            cast_tab       = NULL, 
                            end_moon       = NULL, 
                            models         = NULL, 
                            dataset        = NULL, 
                            include_interp = TRUE,
                            quiet          = FALSE,
                            verbose        = FALSE) {

# not convinced we want to keep this many arguments here

  if (is.null(cast_tab)) {

    cast_choices <- select_casts(main           = main, 
                                 settings       = settings,
                                 cast_ids       = cast_ids, 
                                 models         = models, 
                                 end_moons      = end_moon, 
                                 datasets       = dataset, 
                                 include_interp = include_interp)

    if (NROW(cast_choices) == 0) {
 
      stop("no casts available for request")

    } 


    cast_tab <- read_cast_tabs(main     = main, 
                               settings = settings,
                               cast_ids = cast_choices$cast_ids)

  } else {

    cast_ids  <- ifnull(cast_ids, unique(cast_tab$cast_id))
    models    <- ifnull(models, unique(cast_tab$model))
    dataset   <- ifnull(dataset, unique(cast_tab$dataset))
    end_moon  <- ifnull(end_moon, unique(cast_tab$end_moon))

    cast_id_in  <- cast_tab$cast_id %in% cast_ids
    model_in    <- cast_tab$model %in% models
    dataset_in  <- cast_tab$dataset %in% dataset
    end_moon_in <- cast_tab$end_moon %in% end_moon
    all_in      <- cast_id_in & model_in & dataset_in & end_moon_in

    if (sum(all_in) == 0) {

      stop("no casts available for requested plot")

    }

    cast_tab <- cast_tab[all_in, ]

  }
  
  cast_tab


  # working here! now need to set up evaluate_cast (singular) and then loop

  


}


