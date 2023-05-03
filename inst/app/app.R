main_exists <- any(ls() == "main")

old_main <- dynGet("main")

main   <- "."

onStop(function() {
  rm(main, inherits = TRUE)
  if (main_exists) main <<- old_main
})

global <- global_list(main = main)

shiny::shinyApp(ui = app_ui(global = global),
                server = app_server)