#' @title Create a portalcasting directory
#'
#' @description Create and populate a full portalcasting directory or any 
#'   missing components.
#'
#' @param main_dir directory name for the location of the portalcasting folder
#'
#' @param models names of model scripts to include
#'
#' @return Nothing
#'
#' @export
#'
setup_portalcast_dir <- function(main_path = pc_path("", "~"), 
                                 models = c("autoarima", "esss", "nbgarch", 
                                            "pevgarch")
                                 ){
  
  if (!dir.exists(main_path)){
    dir.create(main_path)
  }

  cat("Downloading Portal Data. \n")
  download_observations()

  if (!dir.exists(full_path("predictions", main_path))){
    dir.create(full_path("predictions", main_path))
  }

  if (!dir.exists(full_path("data", main_path))){
    dir.create(full_path("data", main_path))
  }

  if (!dir.exists(full_path("models", main_path))){
    dir.create(full_path("models", main_path))
    cat(paste0("populating models directory with ", models, "\n"))
    data_dir <- full_path("data", main_path)
    populate_models(full_path("models", main_path), models, data_dir)
  }
}

#' @title Populate a portalcasting models subdirectory
#'
#' @description Create and populate a portalcasting models subdirectory or add
#'   models to an existing one
#'
#' @param model_dir directory name where the model files will reside
#'
#' @param models names of model scripts to include
#' 
#' @param data_dir directory name where the data files will reside
#'
#' @return Nothing
#'
#' @export
#'
populate_models <- function(model_dir = pc_path("models", "~"), 
                            models = c("autoarima", "esss", "nbgarch", 
                                       "pevgarch"),
                             data_dir = pc_path("data", "~")){
  if ("autoarima" %in% models){
    write(model_template("autoarima"), full_path("autoarima.R", model_dir))
  }
  if ("esss" %in% models){
    write(model_template("esss"), full_path("esss.R", model_dir))
  }
  if ("nbgarch" %in% models){
    write(model_template("nbgarch"), full_path("nbgarch.R", model_dir))
  }
  if ("pevgarch" %in% models){
    write(model_template("pevgarch"), full_path("pevgarch.R", model_dir))
  }
}

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
