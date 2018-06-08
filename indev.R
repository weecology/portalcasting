
# in progress on run back through data prep functions

prep_hist_covariate_date produces the historical data

need to now make prep_covariate_date with a forecasts boolean input
it will also need the moons and the metadata, but later gives a circularity rn


i think the issue is that we'll just need to tag the covariates as to whether
they're a forecast or historical



append_covariate_fcast <- function(covariates, moons, metadata){

  forecast_covariates <- fcast_covariates(covariates, moons, metadata)
  
  return(out)
}
#append_covariate_fcast_csv(forecast_covariates, "covariate_forecasts.csv")



#working here to create helper functions to keep pevgarch tidy

# scraps right now 
# stuff is still very much in progress

  newmoons_fcast <- metadata$covariate_forecast_newmoons
  fcast_match <- which(covariates_all_lag$newmoonnumber %in% newmoons_fcast)
  covariates_lag <- covariates_all_lag[-fcast_match, ]
  covariates_fcast_lag <- covariates_all_lag[fcast_match, ]

  out <- list("hist_lag" = covariates_lag, "fcast_lag" = covariates_fcast_lag)
  return(out)





covariate_models <- function(type = "pevgarch"){

  out <- NULL

  if (type == "pevgarch"){
    out <- list(c("maxtemp", "meantemp", "precipitation", "ndvi"),
                c("maxtemp", "mintemp", "precipitation", "ndvi"),
                c("mintemp", "maxtemp", "meantemp", "precipitation"),
                c("precipitation", "ndvi"),
                c("mintemp", "ndvi"),
                c("mintemp"),
                c("maxtemp"),
                c("meantemp"),
                c("precipitation"),
                c("ndvi"),
                c(NULL))
  }
  return(out)
}


lag_covariates <- function(covariates, covariates_fcast, metadata, lag){

  covariates_fcast <- select(covariates_fcast, -"forecast_newmoon")
  covariates_all <- bind_rows(covariates, covariates_fcast)
  covariates_all_lag <- lag_data(covariates_all, lag, tail = TRUE)

}

