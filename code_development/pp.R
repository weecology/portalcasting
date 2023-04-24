

added <- add_new_model(main = main, new_model_controls = new_controls)



#'
#' @examples
#' \donttest{ 
#' \dontrun{
#'    main1 <- file.path(tempdir(), "new_model_controls")
#'    setup_dir(main = main1)
#'
#'    new_controls <- new_model_controls(metadata = new_model_metadata(name       = "newmod", 
#'                                                                     print_name = "New Model"),
#'                                       fit      = new_model_fit(fun  = "arima", 
#'                                                                args = list(x = "abundance")),
#'                                       response = new_model_response(link           = "normal", 
#'                                                                     type           = "distribution", 
#'                                                                     scoring_family = "normal"))
#'   added <- add_new_model(main               = main1, 
#'                          new_model_controls = new_controls
#'
#'    unlink(main1, recursive = TRUE)
#'  }
#'