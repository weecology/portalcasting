context(desc = "webapp functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "global_list makes a list of globals and component functions operate on it", {

  skip_on_cran() 

  expect_silent(gl <- global_list(main = main3))
  expect_is(gl, "list")

  rv <- list(forecast_tab_species                       = gl$initial_forecast_tab_selected_species,
             forecast_tab_dataset                       = gl$initial_forecast_tab_selected_dataset,
             forecast_tab_model                         = gl$initial_forecast_tab_selected_model,
             forecast_tab_historic_end_newmoonnumber    = gl$initial_forecast_tab_selected_historic_end_newmoonnumber,
             evaluation_tab_species                     = gl$initial_evaluation_tab_selected_species,
             evaluation_tab_dataset                     = gl$initial_evaluation_tab_selected_dataset,
             evaluation_tab_model                       = gl$initial_evaluation_tab_selected_model,
             evaluation_tab_historic_end_newmoonnumber  = gl$initial_evaluation_tab_selected_historic_end_newmoonnumber,
             evaluation_tab_newmoonnumber               = gl$initial_evaluation_tab_selected_newmoonnumber)

  expect_is(available_models(global = gl, event = "initial_forecast_tab"), "character")
  expect_is(available_models(global = gl, event = "initial_evaluation_tab"), "character")
  expect_is(available_models(global = gl, event = "null_test"), "NULL")

  expect_is(available_datasets(global = gl, event = "initial_forecast_tab"), "character")
  expect_is(available_datasets(global = gl, event = "initial_evaluation_tab"), "character")
  expect_is(available_datasets(global = gl, event = "null_test"), "NULL")

  expect_is(available_species(global = gl, event = "initial_forecast_tab"), "character")
  expect_is(available_species(global = gl, event = "initial_evaluation_tab"), "character")
  expect_is(available_species(global = gl, event = "null_test"), "NULL")

  expect_is(available_historic_end_newmoonnumbers(global = gl, event = "initial_forecast_tab"), "integer")
  expect_is(available_historic_end_newmoonnumbers(global = gl, event = "initial_evaluation_tab"), "integer")
  expect_is(available_historic_end_newmoonnumbers(global = gl, event = "null_test"), "NULL")

  expect_is(available_newmoonnumbers(global = gl, event = "initial_evaluation_tab"), "integer")
  expect_is(available_newmoonnumbers(global = gl, event = "null_test"), "NULL")

  expect_is(available_models(global = gl, event = "forecast_tab_model", rv = rv), "character")
  expect_is(available_models(global = gl, event = "evaluation_tab_model", rv = rv), "character")

  expect_is(available_datasets(global = gl, event = "forecast_tab_dataset", rv = rv), "character")
  expect_is(available_datasets(global = gl, event = "evaluation_tab_dataset", rv = rv), "character")

  expect_is(available_species(global = gl, event = "forecast_tab_species", rv = rv), "character")
  expect_is(available_species(global = gl, event = "evaluation_tab_species", rv = rv), "character")

  expect_is(available_historic_end_newmoonnumbers(global = gl, event = "forecast_tab_historic_end_newmoonnumber", rv = rv), "integer")
  expect_is(available_historic_end_newmoonnumbers(global = gl, event = "evaluation_tab_historic_end_newmoonnumber", rv = rv), "integer")

  expect_is(available_newmoonnumbers(global = gl, event = "evaluation_tab_newmoonnumber", rv = rv), "integer")


  rv1 <- update_list(rv, forecast_tab_model = "xx")
  expect_is(selected_model(global = gl, event = "forecast_tab_model", rv = rv1), "character")
  rv1 <- update_list(rv, evaluation_tab_model = "xx")
  expect_is(selected_model(global = gl, event = "evaluation_tab_model", rv = rv1), "character")
  expect_is(selected_model(global = gl, event = "null_test", rv = rv1), "NULL")

  rv1 <- update_list(rv, forecast_tab_dataset = "xx")
  expect_is(selected_dataset(global = gl, event = "forecast_tab_dataset", rv = rv1), "character")
  rv1 <- update_list(rv, evaluation_tab_dataset = "xx")
  expect_is(selected_dataset(global = gl, event = "evaluation_tab_dataset", rv = rv1), "character")
  expect_is(selected_dataset(global = gl, event = "null_test", rv = rv1), "NULL")

  rv1 <- update_list(rv, forecast_tab_species = "xx")
  expect_is(selected_species(global = gl, event = "forecast_tab_species", rv = rv1), "character")
  rv1 <- update_list(rv, evaluation_tab_species = "xx")
  expect_is(selected_species(global = gl, event = "evaluation_tab_species", rv = rv1), "character")
  expect_is(selected_species(global = gl, event = "null_test", rv = rv1), "NULL")

  rv1 <- update_list(rv, forecast_tab_historic_end_newmoonnumber = 1e10)
  expect_is(selected_historic_end_newmoonnumber(global = gl, event = "forecast_tab_historic_end_newmoonnumber", rv = rv1), "integer")
  rv1 <- update_list(rv, evaluation_tab_historic_end_newmoonnumber = 1e10)
  expect_is(selected_historic_end_newmoonnumber(global = gl, event = "evaluation_tab_historic_end_newmoonnumber", rv = rv1), "integer")
  expect_is(selected_historic_end_newmoonnumber(global = gl, event = "null_test", rv = rv1), "NULL")

  expect_is(selected_species(global = gl, event = "evaluation_tab_species", rv = rv1), "character")
  expect_is(selected_species(global = gl, event = "null_test", rv = rv1), "NULL")

  rv1 <- update_list(rv, evaluation_tab_newmoonnumber = 1e10)
  expect_is(selected_newmoonnumber(global = gl, event = "evaluation_tab_newmoonnumber", rv = rv1), "integer")
  expect_is(selected_newmoonnumber(global = gl, event = "null_test", rv = rv1), "NULL")




})




