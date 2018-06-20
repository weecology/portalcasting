notes:

working right now on the covariate forecasting

the current situation with the gap might be a weird nuance of the frequency of
data updates. should build checks in to handle it, but may not need to worry
too much about it right now


write a small wrapper for appending of the csv
document all the dang functions wow


hmmm getting a note at the outset about the default data path
likely coming from portalr
should figure out what's causing it and quiet it




specifically working within the prep_fcast_covariates function

here's what was cut out and needs to be put back in
might need its own functions
definitely needs options added (carry them all the way back up!)

  if (append_fcast_csv){
    append_covariate_fcast_csv(fcast_data, covariate_file, source_name)
  }


  fcast_data <- select(fcast_data, -forecast_newmoon)
  fcast_data$source <- "fcast"


and then get to handling the existing historical covariate forecasts

  covdat <- full_path("portalcasting/data/covariate_forecasts.csv", base_path)
  if (!file.exists(covdat)){
    to <- covdat
    fname <- "extdata/covariate_forecasts.csv"
    from <- system.file(fname, package = "portalcasting")
    if (!file.exists(from)){
      stop("Covariate data not found.")
    }
    temp <- read.csv(from, stringsAsFactors = FALSE)    

  }



# next step is to update model_template
#
# need to update the function flexibibility, to flexibly write model scripts
# leverage the tree set up to basically just pass the tree to the
# read and write functions (wrap them if needed)
# it'll likely be easier if we just give all the model functions covariates
#  but we could also make some indicator for them


#' @title Create the text to be written out to a script for a model
#'
#' @description Create the text for a model script to be housed in the model 
#'   directory
#'
#' @param model names of model script text to write
#'
#' @return Nothing
#'
#' @export
#'
model_template <- function(model = "autoarima"){

  if (length(model) != 1){
    stop("1 and only 1 model at a time, please and thank you!")
  }

  if (model == "autoarima"){
    out <- template_autoarima()
  }
  if (model == "esss"){
    out <- template_esss()
  }
  if (model == "nbgarch"){
    out <- template_nbgarch()
  }
  if (model == "pevgarch"){
    out <- template_pevgarch()
  }
  return(out)
}

#' @title Create template autoarima text
#'
#' @description Creates the text for a template autoarima script
#'
#' @return text of the template autoarima script
#'
#' @export
#'
template_autoarima <- function(){

'all <- read.csv(full_path("all.csv", pc_path("data", "~")));
controls <- read.csv(full_path("controls.csv", pc_path("data", "~")));
metadata <- yaml::yaml.load_file(
              full_path("metadata.yaml", pc_path("data", "~")));
aa_a <- autoarima(all, metadata);
aa_c <- autoarima(controls, metadata, level = "Controls");
save_forecast_output(aa_a, aa_c, "AutoArima", metadata)'

}

#' @title Create template esss text
#'
#' @description Creates the text for a template esss script
#'
#' @return text of the template esss script
#'
#' @export
#'
template_esss <- function(){

'all <- read.csv(full_path("all.csv", pc_path("data", "~")));
controls <- read.csv(full_path("controls.csv", pc_path("data", "~")));
metadata <- yaml::yaml.load_file(
              full_path("metadata.yaml", pc_path("data", "~")));
esss_a <- esss(all, metadata);
esss_c <- esss(controls, metadata, level = "Controls");
save_forecast_output(esss_a, esss_c, "ESSS", metadata)'

}

#' @title Create template nbGARCH text
#'
#' @description Creates the text for a template nbGARCH script
#'
#' @return text of the template nbGARCH script
#'
#' @export
#'
template_nbgarch <- function(){

'all <- read.csv(full_path("all.csv", pc_path("data", "~")));
controls <- read.csv(full_path("controls.csv", pc_path("data", "~")));
metadata <- yaml::yaml.load_file(
              full_path("metadata.yaml", pc_path("data", "~")));
nbgarch_a <- nbgarch(all, metadata);
nbgarch_c <- nbgarch(controls, metadata, level = "Controls");
save_forecast_output(nbgarch_a, nbgarch_c, "nbGARCH", metadata)'

}

#' @title Create template pevGARCH text
#'
#' @description Creates the text for a template pevGARCH script
#'
#' @return text of the template pevGARCH script
#'
#' @export
#'
template_pevgarch <- function(){

'all <- read.csv(full_path("all.csv", pc_path("data", "~")));
controls <- read.csv(full_path("controls.csv", pc_path("data", "~")));
covariates <- read.csv(full_path("covariates.csv", pc_path("data", "~")));
metadata <- yaml::yaml.load_file(
              full_path("metadata.yaml", pc_path("data", "~")));
pevg_a <- pevgarch(all, covariates, metadata);
pevg_c <- pevgarch(controls, covariates, metadata, level = "Controls");
save_forecast_output(pevg_a, pevg_c, "pevGARCH", metadata)'

}


