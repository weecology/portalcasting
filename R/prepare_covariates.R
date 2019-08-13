prep_covariates <- function(moons = prep_moons(main = main),
                            main = ".", quiet = FALSE, end_moon = NULL,
                            cov_hist = TRUE, cov_fcast = TRUE,
                            lead_time = 12, 
                            cast_date = Sys.Date(), cast_type = "forecast",
                            save = TRUE, filename = "covariates.csv",
                            overwrite = TRUE){
  messageq("Loading covariate data files into data subdirectory", quiet)
  hist_cov <- pass_and_call(prep_hist_covariates)
  if (cov_fcast){


    fcast_cov <- pass_and_call(prep_fcast_covariates, hist_cov = hist_cov)
  }
  out <- hist_cov[-(1:nrow(hist_cov)), ]
  if (cov_hist){
    out <- bind_rows(out, hist_cov)
  }
  if (cov_fcast){
    out <- bind_rows(out, fcast_cov)
  }
  data_out(out, main, save, filename, overwrite, quiet)
}


# this will now work for a covariate forcast or not
target_newmoons <- function(moons = prep_moons(main = main),
                              lead_time = 12, main =  ".",
                                   cast_date = Sys.Date(),
                                   cast_type = "forecast", end_moon = NULL,
                                   hist_cov = NULL){

  if (cast_type == "forecast"){

    which_prev_newmoon <- max(which(moons$newmoondate < cast_date))
    prev_newmoon <- moons$newmoonnumber[which_prev_newmoon]
  } else if (cast_type == "hindcast"){
    prev_newmoon <- end_moon    
  } else {
    stop("cast_type can only be forecast or hindcast")
  } 
  if(is.null(hist_cov)){
    first_fcast_newmoon <- prev_newmoon + 1
  } else {
    prev_covar_newmoon <- tail(hist_cov, 1)$newmoonnumber
    first_fcast_newmoon <- prev_covar_newmoon + 1
  }
  last_fcast_newmoon <- prev_newmoon + lead_time
  first_fcast_newmoon:last_fcast_newmoon
}




prep_hist_covariates <- function(main = ".", quiet = FALSE, end_moon = NULL
                                 ){
  weather_data <- prep_weather_data(main)
  ndvi_data <- ndvi("newmoon", TRUE, sub_paths(main, "raw"))
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  out$source <- "hist"
  if (!is.null(end_moon)){
    out <- out[which(out$newmoonnumber <= end_moon), ]
  }
  data.frame(out)
}


prep_weather_data <- function(main = "."){
  cols <- c("mintemp", "maxtemp", "meantemp", "precipitation", 
            "newmoonnumber")
  weather("newmoon", TRUE, sub_paths(main, "raw")) %>% 
  ungroup() %>%
  select(cols) %>%
  remove_incompletes("newmoonnumber")
}