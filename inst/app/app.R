main   <<- dynGet("main")
global <<- global_list(main = main)

shinyApp(ui     = app_ui(global = global),
         server = app_server)