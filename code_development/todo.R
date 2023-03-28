new model and dataset controls and templates
update directory function
document patches and needs to remove them

update NEWS and PR
figures --- incl testing
remotes::install_github("weecology/portalr#288")



full test



rm(list=ls())
devtools::load_all()

main <- "~/portalcasting"
setup_production(main)
portalcast(main)
evaluate_casts(main)
todo



something like this, but needs easier updates

add_model_controls <- function (main               = ".",
                                new_model_controls = NULL,
                                settings           = directory_settings( ),
                                quiet              = FALSE, 
                                ...) {

  template_path <- system.file(...     = "extdata", 
                               ...     = "model_controls_template.yaml", 
                               package = "portalcasting")

  template      <- read_yaml(file = template_path)

  update_list(template, ... = ...)

}