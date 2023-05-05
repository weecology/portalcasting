# This file launches the shiny app. It *should* be called from within portalcasting::run_app, but is also (hopefully) operational regardless of calling function.

# Check to see if there are objects named main and global that exist outside the current call and save each as a new object called env_<>. 
# If either doesn't exist, the value of env_<> is NULL.

if (exists("main")) {
  env_main <- main
} else {
  env_main <- NULL
}

if (exists("global")) {
  env_global <- global
} else {
  env_global <- NULL
}

# Determine the value of the argument main, which should be used in the process of calling this script (it is an argument with default "." in portalcasting::run_app). 
# If it's not found, the main that is supposed to come from the argument is given "."
# The argument main is then "scoped up" to the parent environment to allow broad access across the app.

arg_main <-  dynGet("main", minframe = 0, ifnotfound = ".")
main     <<- arg_main

# When the app is stopped, either intentionally or not, we need to reset the main and global variables to what they were before the app was called.
# If the env_main is not NULL, it means that we need to swap that value back into main, otherwise (if it is NULL) we just wipe it. 
# Similarly, if env_global is not NULL, we swap that value back into global, otherwise (if it is NULL) we just wipe it. 

onStop( function( ) {

  if (!is.null(env_main)) {

    main <<- env_main

  } else {

    rm("main", inherits = TRUE)

  }

  if (!is.null(env_global)) {

    global <<- env_global

  } else {

    rm("env_global", inherits = TRUE)

  }

})

# Define the value for global list and scope it up for access across the app.

global <<- global_list(main = main)

# Run the app.

shinyApp(ui     = app_ui(global = global),
         server = app_server)