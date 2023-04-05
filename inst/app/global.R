old_main <- main

main   <- ".."

global <- global_list(main = main)

onStop(function() {main <<- old_main})