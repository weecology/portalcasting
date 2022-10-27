
update_models <- function (main     = ".",
                           models   = prefab_models(), 
                           datasets = prefab_datasets(),
                           settings = directory_settings(), 
                           quiet    = FALSE, 
                           verbose  = FALSE) {

  write_model_controls(main     = main, 
                       settings = settings, 
                       models   = models, 
                       quiet    = quiet)

  fill_models(main     = main, 
              settings = settings,
              models   = models, 
              quiet    = quiet, 
              verbose  = verbose)

}