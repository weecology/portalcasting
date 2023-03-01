setup_start_message <- function (main     = ".",
                                 settings = production_settings(), 
                                 quiet    = FALSE) {

  messageq(message_break(), "\nThis is ", core_package_version[["package"]], " v", core_package_version[["version"]], "  ", format(Sys.time(), "%x %T %Z"), "\n", message_break(), quiet = quiet)
  messageq(message_break(), "\nEstablishing directory at\n ", normalizePath(file.path(main = main), mustWork = FALSE), "\n", message_break(), quiet = quiet)

}

setup_end_message <- function (main     = ".",
                               settings = production_settings(), 
                               quiet    = FALSE) {

  messageq(message_break(), "\nDirectory successfully instantiated\n", message_break(), quiet = quiet)

}