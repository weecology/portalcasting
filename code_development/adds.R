add_model <- function (main               = ".",
                       new_model_controls = NULL,
                       models             = prefab_models( ),
                       settings           = directory_settings( ),
                       quiet              = FALSE) {


  template_controls_file <- system.file(...     = "extdata", 
                                        ...     = "model_controls_template.yaml", 
                                        package = "portalcasting")

  template_controls      <- read_yaml(file = template_controls_file)



}