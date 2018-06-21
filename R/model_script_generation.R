#' @title Provide the names of models
#'
#' @description Based on a \code{type}, returns a character vectory of model
#'   names to be included. Currently only support for 
#'   \code{type = "portalcasting"}.
#'
#' @param type type of model options (currently only support for 
#'   "portalcasting")
#'
#' @return vector of model names
#'
#' @export
#'
models <- function(type = "portalcasting"){
  out <- NULL
  if (type == "portalcasting"){
    out <- c("autoarima", "esss", "nbgarch", "pevgarch")
  }
  return(out)
}

#' @title Write the template for a model into the model sub directory
#'
#' @description Create template script for a given model
#'
#' @param model the name of the specific model
#'
#' @param options_models model option list
#'
#' @return Nothing
#'
#' @export
#'
write_model <- function(model = NULL, options_models = models_options()){
  if (is.null(model)){
    return()
  }
  if (!options_models$quiet){
    cat("", paste0("adding ", model, " model to models sub-directory \n"))
  }
  mod_path <- model_path(options_models$tree, model = model)
  mod_template <- model_template(options_models$tree, model = model)
  write(mod_template, mod_path)
}

#' @title Create the text to be written out to a script for a model
#'
#' @description Create the text for a model script to be housed in the model 
#'   directory
#'
#' @param tree directory tree
#'
#' @param model names of model script text to write
#'
#' @return Nothing
#'
#' @export
#'
model_template <- function(tree = dirtree(), model = "autoarima"){

  if (length(model) != 1){
    stop("1 and only 1 model at a time, please and thank you!")
  }

  if (model == "autoarima"){
    out <- template_autoarima(tree)
  }
  if (model == "esss"){
    out <- template_esss(tree)
  }
  if (model == "nbgarch"){
    out <- template_nbgarch(tree)
  }
  if (model == "pevgarch"){
    out <- template_pevgarch(tree)
  }
  return(out)
}

#' @title Create template autoarima text
#'
#' @description Creates the text for a template autoarima script
#'
#' @param tree directory tree
#'
#' @return text of the template autoarima script
#'
#' @export
#'
template_autoarima <- function(tree = dirtree()){

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
#' @param tree directory tree
#'
#' @return text of the template esss script
#'
#' @export
#'
template_esss <- function(tree = dirtree()){

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
#' @param tree directory tree
#'
#' @return text of the template nbGARCH script
#'
#' @export
#'
template_nbgarch <- function(tree = dirtree()){

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
#' @param tree directory tree
#'
#' @return text of the template pevGARCH script
#'
#' @export
#'
template_pevgarch <- function(tree = dirtree()){

'all <- read.csv(full_path("all.csv", pc_path("data", "~")));
controls <- read.csv(full_path("controls.csv", pc_path("data", "~")));
covariates <- read.csv(full_path("covariates.csv", pc_path("data", "~")));
metadata <- yaml::yaml.load_file(
              full_path("metadata.yaml", pc_path("data", "~")));
pevg_a <- pevgarch(all, covariates, metadata);
pevg_c <- pevgarch(controls, covariates, metadata, level = "Controls");
save_forecast_output(pevg_a, pevg_c, "pevGARCH", metadata)'

}


