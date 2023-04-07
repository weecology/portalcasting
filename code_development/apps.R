rm(list=ls())
main <- "~/sandbox"
devtools::document()
setup_sandbox(main)
fill_app(main)
run_app(main)

shiny::testServer

shiny::testServer(portal_forecast_server, {

  session$setInputs(forecast_tab_species                      = global$initial_forecast_tab_selected_species, 
                    forecast_tab_dataset                      = global$initial_forecast_tab_selected_dataset,
                    forecast_tab_model                        = global$initial_forecast_tab_selected_model,
                    forecast_tab_historic_end_newmoonnumber   = global$initial_forecast_tab_selected_historic_end_newmoonnumber,
                    evaluation_tab_species                    = global$initial_evaluation_tab_selected_species,
                    evaluation_tab_dataset                    = global$initial_evaluation_tab_selected_dataset,
                    evaluation_tab_model                      = global$initial_evaluation_tab_selected_model,
                    evaluation_tab_historic_end_newmoonnumber = global$initial_evaluation_tab_selected_historic_end_newmoonnumber,
                    evaluation_tab_newmoonnumber              = global$initial_evaluation_tab_selected_newmoonnumber)

  expect_equal(output$forecast_tab_species, as.vector(global$initial_forecast_tab_selected_species))
  expect_equal(output$forecast_tab_dataset, as.vector(global$initial_forecast_tab_selected_dataset))
  expect_equal(output$forecast_tab_model, as.vector(global$initial_forecast_tab_selected_model))
  expect_equal(output$forecast_tab_historic_end_newmoonnumber, as.character(global$initial_forecast_tab_selected_historic_end_newmoonnumber))

  expect_equal(output$evaluation_tab_species, as.vector(global$initial_evaluation_tab_selected_species))
  expect_equal(output$evaluation_tab_dataset, as.vector(global$initial_evaluation_tab_selected_dataset))
  expect_equal(output$evaluation_tab_model, as.vector(global$initial_evaluation_tab_selected_model))
  expect_equal(output$evaluation_tab_historic_end_newmoonnumber, as.character(global$initial_evaluation_tab_selected_historic_end_newmoonnumber))
  expect_equal(output$evaluation_tab_newmoonnumber, as.character(global$initial_evaluation_tab_selected_newmoonnumber))


})



covariates <- read_covariates(main)


head(covariates)

windows(12, 6)
plot_cast_ts(main, species = "DM", dataset = "controls")
plot_cast_ts(main, species = "total", dataset = "all")

casts_evaluations <- read_casts_evaluations(main)

plot_casts_cov_RMSE(main, species = "DM", datasets = "controls", models = "AutoArima")
plot_cast_point(main, species = "DM", dataset = "controls", model = "AutoArima", historic_end_newmoonnumber = 565, with_census = T, newmoonnumber = 565)

plot_cast_point(main, highlight_sp = "DM", dataset = "controls", model = "AutoArima", historic_end_newmoonnumber = 558)

plot_cast_point(main)
fill_app(main)
run_web_app(main)

models = "AutoArima"
species = "DM"

arrow::write_csv_arrow

dataset = "controls"
settings <- read_directory_settings(main)

path <- (file.path(main, settings$subdirectories$forecasts, settings$files$forecast_evaluations))
system.time(x1 <- read.csv(path))

system.time(x2 <-as.data.frame( arrow::read_csv_arrow(path)))

dim(x2)

plot_cast_point(main)