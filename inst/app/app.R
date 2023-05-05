library(portalcasting)

main_exists   <- any(ls() == "main")

onStop(function() {
  if (exists('main_exists')) {
    rm(main, inherits = TRUE)
  }
  if (exists('global')) {
    rm(global, inherits = TRUE)
  }
})

if (!main_exists) {
  main   <<- "."
} else {
  main   <<- dynGet("main")
}

global <<- global_list(main = main)

shinyApp(ui     = app_ui(global = global),
         server = app_server)