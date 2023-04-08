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
#'    portalcast(main = main1, models = "AutoArima")
#'    plot_cast_ts(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#'  }}
#'


