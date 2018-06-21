right now: working to update the model generation.
as of now, the .R files are generated, but they're not right (out of date)


# consider (i.e. do) organizing the documentation for the options lists so 
# they're all together





# next step is to update model_template
#
# need to update the function flexibibility, to flexibly write model scripts
# leverage the tree set up to basically just pass the tree to the
# read and write functions (wrap them if needed)
# it'll likely be easier if we just give all the model functions covariates
#  but we could also make some indicator for them






# not sure if we still need this?

#' @title Append a covariate forecast to historical covariate table
#' 
#' @description combining weather and ndvi forecasts to the existing 
#'   covariates
#' 
#' @param covariates output from \code{get_covariate_data}
#'
#' @param metadata model metadata
#'
#' @param moons moon data table
#' 
#' @return no value
#'
#' @export
#'
append_covariate_fcast <- function(covariates, metadata, 
                                   moons = prep_moons){

  covariates_fcast <- forecast_covariates(covariates, moons, metadata)
  covariates_fcast <- select(covariates_fcast, -"forecast_newmoon")
  covariates_all <- bind_rows(covariates, covariates_fcast)
  return(covariates_all)
}


