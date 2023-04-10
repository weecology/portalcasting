#'
#' @examples
#' \donttest{ 
#' \dontrun{
#'    main1 <- file.path(tempdir(), "standard")
#'    setup_dir(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#'  }
#'

#'
#' @examples
#' \donttest{ 
#' \dontrun{
#'    main1 <- file.path(tempdir(), "production")
#'    setup_sandbox(main = main1)
#'
#'    global <- global_list(main = main1)
#'    available_newmoonnumbers(global, event_name = "initial_evaluation_tab")
#'
#'    unlink(main1, recursive = TRUE)
#'  }}
#'

devtools::document()


