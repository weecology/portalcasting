devtools::document()
devtools::spell_check()
urlchecker::url_check()


main <- "~/pd"
setup_production(main = main)

fm <- read_forecasts_metadata(main)

ids <- fm$forecast_id

ef <- evaluate_forecasts(main = main, forecasts_ids = ids)


