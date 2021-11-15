#' @title Model Controls List
#'
#' @description \code{list} of the models available for use via \code{\link{prepare_models}}. 
#'
#' @details The currently prefabricated models include \code{"AutoArima"}
#'
#' @format \code{list} of control \code{list}s, each corresponding to a model. 
#'
"model_controls"

#' @title Provide the Names of the Prefabricated Models
#'
#' @description Create a \code{character} vector of the prefabricated models. \cr \cr
#'
#' @return \code{character} vector of model names.
#'
#' @examples
#'  prefab_models()
#'
#' @export
#'
prefabricated_models <- function () {

  envr <- environment()
  data(model_controls, envir = envr)
  names(model_controls)  


}




AutoArima <- function (main     = ".", 
                       dataset  = "all", 
                       settings = directory_settings(),
                       quiet    = FALSE, 
                       verbose  = FALSE) {

  newmoon_species_model(main     = main,
                        model    = "AutoArima",
                        fun      = auto.arima,
                        dataset  = "all", 
                        settings = directory_settings(),
                        quiet    = FALSE, 
                        verbose  = FALSE)
                        
                        
                        
}

newmoon_species_model <- function  (main     = ".", 
                                    model    = "AutoArima",
                                    fun      = auto.arima,
                                    dataset  = "all", 
                                    settings = directory_settings(),
                                    quiet    = FALSE, 
                                    verbose  = FALSE) {

  messageq("  - ", model, " for ", dataset, quiet = quiet)

  rodents_table <- read_rodents_table(main = main, dataset = dataset)


  species <- species_from_table(rodents_tab = rodents_table, total = TRUE, nadot = TRUE)


  moons <- read_moons(main = main, settings = settings)

  metadata <- read_metadata(main = main, settings = settings)

  t1_newmoonnumber     <- max(moons$newmoonnumber[moons$newmoondate <= metadata$t1], na.rm = TRUE)
  origin_newmoonnumber <- max(moons$newmoonnumber[moons$newmoondate <= metadata$origin], na.rm = TRUE)
  final_newmoonnumber  <- max(moons$newmoonnumber[moons$newmoondate <= (as.Date(metadata$origin) + metadata$lead)], na.rm = TRUE)
 
  fit_newmoonnumbers       <- t1_newmoonnumber : origin_newmoonnumber
  forecast_newmoonnumbers  <- (origin_newmoonnumber + 1) : final_newmoonnumber
  nforecast_newmoonnumbers <- length(forecast_newmoonnumbers)

  rodents_table <- rodents_table[order(rodents_table$newmoonnumber), , drop = FALSE]
  fit_rodents   <- rodents_table[rodents_table$newmoonnumber %in% fit_newmoonnumbers, species, drop = FALSE]

  fits <- apply(fit_rodents, 2, fun)

  forecasts <- mapply(forecast, fits, h = nforecast_newmoonnumbers, SIMPLIFY = FALSE)
  forecasts <- lapply(forecasts, data.frame, newmoonnumber = forecast_newmoonnumbers)
  forecasts <- mapply(data.frame, forecasts, species = names(forecasts), SIMPLIFY = FALSE)

  forecast_table         <- do.call(rbind, update_list(forecasts, make.row.names = FALSE))

  forecast_table$model   <- model
  forecast_table$dataset <- dataset

  forecast_table$t1_newmoonnumber     <- t1_newmoonnumber
  forecast_table$origin_newmoonnumber <- origin_newmoonnumber


  model_metadata <- update_list(list     = metadata, 
                                models   = model,
                                datasets = dataset)

  list(metadata  = model_metadata, 
       fits      = fits,
       forecasts = forecast_table)


}


