rm(list=ls())
main <- "~/production_testing"
devtools::document()
run_web_app(main)


covariates <- read_covariates(main)


head(covariates)

windows(12, 6)
plot_cast_ts(main, species = "DM", dataset = "controls")
plot_cast_ts(main, species = "total", dataset = "all")

casts_evaluations <- read_casts_evaluations(main)

plot_casts_cov_RMSE(main, species = "DM", datasets = "controls", models = "AutoArima")

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