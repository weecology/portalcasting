#'
create_dir <- function(main     = ".", 
                       settings = directory_settings(), 
                       quiet    = FALSE){

  # check for existence of directory and halt creation unless settings$overwrite
  # then read the directory settings

  directory_present <- file.exists(file.path(main))
  if (directory_present) {

    if (settings$overwrite) {
    
      messageq("Directory already exists at ", file.path(main), " and `overwrite` is FALSE, directory not created.", quiet = quiet)
      return()

    }

  }

  core_package_version <- package_version_finder("setup_dir")



  mapply(FUN          = dir.create, 
         path         = file.path(main, settings$subdirectories),
         recursive    = TRUE,
         showWarnings = FALSE)

  write_directory_configuration(main     = main, 
                                settings = settings, 
                                quiet    = quiet)


}
