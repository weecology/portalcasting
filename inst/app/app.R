
main_exists   <- any(ls() == "main")

onStop(function() {
  if (!main_exists) {
    rm(main, inherits = TRUE)
  }
  rm(global, inherits = TRUE)

})

main   <<- dynGet("main")
global <<- global_list(main = main)

shinyApp(ui     = app_ui(global = global),
         server = app_server)