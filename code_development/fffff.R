devtools::load_all()
main<-"~/pt"
setup_production(main)
run_app(main)




    ids <- select_forecasts(main = main)$forecast_id
         
    evaluate_forecasts(main        = main, 
                      forecasts_ids = ids[1:10])

x<-read_forecasts_evaluations(main = main)
head(x)

  plot_forecast_ts(main = main)
    plot_forecast_point(main = main)
    plot_forecasts_error_lead(main = main, datasets = "all")
    plot_forecasts_cov_RMSE(main    = main, 
                            models  = "AutoArima", datasets = "all", 
                            species = "DM")
