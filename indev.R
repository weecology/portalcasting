






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

