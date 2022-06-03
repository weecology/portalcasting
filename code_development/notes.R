

devtools::document()
devtools::load_all()

main <- "~/portalcasting"


setup_production(main = main)

# working in "model shell"

# trying to establish how best to write out model function arguments

# the big sticking point right now is how to manage the different response
# variable names for interpolation,

update_models(main)

model_controls(main     = main, 
                             models   = model,
                             settings = settings)