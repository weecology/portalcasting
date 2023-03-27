new model and dataset controls and templates
update directory function
document patches and needs to remove them



remotes::install_github("weecology/portalr#288")










rm(list=ls())
devtools::load_all()

main <- "~/ptc"
setup_production(main)


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