right now: working to update the model generation.
as of now, the .R files are generated, but they're not right (out of date)


# consider (i.e. do) organizing the documentation for the options lists so 
# they're all together





#
# need to update the function flexibibility, to flexibly write model scripts
# leverage the tree set up to basically just pass the tree to the
# read and write functions (wrap them if needed)
# it'll likely be easier if we just give all the model functions covariates
#  but we could also make some indicator for them

# i think we'll want each model to have its own options list
# this will allow us to toggle the inclusion of covariates and the covariate 
# lag, as well as other things


options_model <- list(tree = dirtree(), )













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

