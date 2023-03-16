devtools::load_all()
main = "~/pct"
dataset = "controls"
species = "DM"
settings = directory_settings( )
quiet = FALSE
verbose = FALSE
model = "AutoArima"


  abundance  <- prepare_rodent_abundance(main     = main,
                                         dataset  = dataset,
                                         species  = species,
                                         model    = model,
                                         settings = settings,
                                         quiet    = quiet,
                                         verbose  = verbose)

  covariates <- read_covariates(main     = main,
                                settings = settings)
  metadata   <- read_metadata(main       = main,
                              settings   = settings)
  newmoons   <- read_newmoons(main       = main,
                              settings   = settings)
