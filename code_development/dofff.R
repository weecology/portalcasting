#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "new_dataset_controls")
#'    setup_dir(main = main1)
#'
#'    dataset_controls_template( )
#'
#'    new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
#'                                         args     = new_dataset_args(name     = "newdata", 
#'                                                                     filename = "rodents_newdata.csv"))
#'
#'    added <- add_new_dataset(main                 = main1, 
#'                             new_dataset_controls = new_controls)
#'
#'    portalcast(main     = main, 
#'               models   = "AutoArima", 
#'               datasets = "newdata", 
#'               species  = c("DM", "PP", "total"))
#'
#'
#'    unlink(main1, recursive = TRUE)
#'  }
#'

new_controls <- new__controls(metadata = new__metadata(name = ""),
                                     )

added <- add_new_dataset(main = main, new__controls = new_controls)