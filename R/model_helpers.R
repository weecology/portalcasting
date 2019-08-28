update_metadata <- function(metadata = NULL, model = NULL, data_set = NULL,
                            data_set_controls = NULL, 
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(metadata)
  metadata$controls_r <- data_set_controls
  metadata$models <- "AutoArima"
  metadata$data_sets <- data_set
  metadata
}

species_from_table <- function(rodents_tab = NULL, total = NULL, nadot = NULL,
                               arg_checks = arg_checks){
  sp_col <- is_sp_col(rodents_tab = rodents_tab, total = total, nadot = nadot,
                      arg_checks = arg_checks)
  colnames(rodents_tab)[sp_col]
}