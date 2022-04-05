context(desc = "ensembling functions")

main <- "./testing"

test_that(desc = "ensemble_casts ensembles accordingly", {

  # download is held back on cran

    skip_on_cran() 

end_moons <- NULL
  cast_ids <- NULL
  models <- NULL
rodent_dataset <- NULL
include_interp <- TRUE
species <- NULL

    cast_choices <- select_casts(main           = main, 
                                 cast_ids       = cast_ids, 
                                 models         = models, 
                                 end_moons      = end_moons, 
                                 rodent_datasets       = rodent_dataset, 
                                 include_interp = include_interp)



      cast_tab <- read_cast_tabs(main     = main, 
                                 cast_ids = cast_choices$cast_id)
      cast_tab <- add_obs_to_cast_tab(main     = main,  
                                      cast_tab = cast_tab)
      cast_tab <- add_err_to_cast_tab(main     = main,  
                                      cast_tab = cast_tab)
      cast_tab <- add_lead_to_cast_tab(main     = main,  
                                       cast_tab = cast_tab)
      cast_tab <- add_covered_to_cast_tab(main     = main,  
                                          cast_tab = cast_tab)



  cast_tab$data_set <- gsub("_interp", "", cast_tab$data_set)
  cast_ids          <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models            <- ifnull(models, unique(cast_tab$model))
  dataset          <- ifnull(dataset, unique(cast_tab$data_set)[1])
  species           <- ifnull(species, evalplot_species()) 
  end_moons         <- ifnull(end_moons, unique(cast_tab$end_moon)) 
  cast_id_in        <- cast_tab$cast_id %in% cast_ids
  model_in          <- cast_tab$model %in% models
  rodent_dataset_in       <- cast_tab$rodent_dataset == dataset
  species_in        <- cast_tab$species %in% species
  end_moon_in       <- cast_tab$end_moon %in% end_moons
  all_in            <- cast_id_in & model_in & rodent_dataset_in & species_in & end_moon_in

  cast_tab <- cast_tab[all_in, ]



  expect_is(ensemble_casts(main     = main, 
                           settings = settings,
                           cast_tab = cast_tab,
                           end_moon = end_moons[1],
                           models   = models, 
                           rodent_dataset = rodent_dataset,
                           species  = species), "data.frame")


  expect_error(ensemble_casts(main = main, cast_id = 1e10))

  expect_error(ensemble_casts(main = main, end_moon = 1e10))

})