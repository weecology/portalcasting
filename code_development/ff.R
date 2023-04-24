
#' @rdname portalcast
#'
#' @export
#'
xcast <- function (main       = ".", 
                  settings   = directory_settings( ), 
                  quiet      = FALSE, 
                  verbose    = FALSE) {

    model <- models_scripts[i]

    messageq(message_break( ), "\n -Running ", path_no_ext(basename(model)), "\n", message_break( ), quiet = quiet)

    run_status <- tryCatch(expr  = source(model),
                           error = function(x){NA})

    if (all(is.na(run_status))) {

      messageq("  |----| ", path_no_ext(basename(model)), " failed |----|", quiet = quiet)

    } else {

      messageq("  |++++| ", path_no_ext(basename(model)), " successful |++++|", quiet = quiet)

    }

  }

  invisible( )

}

