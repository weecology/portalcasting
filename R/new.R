new_model_controls <- function( ) {

  model_controls_template_file <- system.file(...     = "extdata", 
                                              ...     = "model_controls_template.yaml", 
                                              package = "portalcasting")

  read_yaml(file = model_controls_template_file)

}