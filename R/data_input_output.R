
#' @title Save Data Out to a File and Return It	Invisibly
#'
#' @description Save inputted data out to a data file if requested and return it to the console, \code{\link[base]{invisible}}-ly..
#'
#' @param x \code{data.frame} or YAML \code{list} to be written out.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param subdirectory \code{character} value defining the data subdirectory of the portalcasting directory tree. 
#'
#' @param save \code{logical} indicator controlling if \code{x} should be saved out.
#'
#' @param filename \code{character} name of the file for saving \code{x}.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{x} as input, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
write_data <- function (x            = NULL, 
                        main         = ".", 
                        subdirectory = "data",
                        save         = TRUE, 
                        filename     = NULL, 
                        quiet        = FALSE) {
  
  return_if_null(x)

  return_if_null(filename)


  if (save) {

    full_path <- file.path(main, subdirectory, filename)

    if (file.exists(full_path)) {


    } else {

      messageq("    **", filename, " saved**", quiet = quiet)

    }


      if (file_ext(filename) == "csv") {

        write.csv(x, full_path, row.names = FALSE)

      } else if (file_ext(filename) == "yaml"){

        write_yaml(x, file = full_path)

      } else {

        stop("file type not supported")

      }

    }
   

  invisible(x)

}

#' @title Read in and Format a Portalcasting Data File 
#'
#' @description Read in a specified data file.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'  
#' @param data_name \code{character} representation of the data needed. Current options include \code{"rodents"}, \code{"rodents_table"}, \code{"covariates"}, \code{"forecast_covariates"},  \code{"historical_covariates"}, \code{"covariate_forecasts"}, \code{"newmoons"}, and \code{"metadata"}.
#'
#' @param dataset,datasets \code{character} representation of the grouping name(s) used to define the rodents. Standard options are \code{"all"}, \code{"controls"}, and \code{"exclosures"}. \code{dataset} can only be length 1, \code{datasets} is not restricted in length.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return Data requested.
#' 
#' @export
#'
read_data <- function (main      = ".", 
                       data_name = NULL, 
                       dataset   = "all", 
                       datasets  = prefab_datasets( ), 
                       settings  = directory_settings( )) {
  
  return_if_null(data_name)

  data_name <- tolower(data_name)

  if (data_name == "rodents") {

    out <- read_rodents(main     = main, 
                        datasets = datasets, 
                        settings = settings)

  }

  if (data_name == "rodents_table") { 

    out <- read_rodents_table(main     = main, 
                              dataset  = dataset, 
                              settings = settings)

  }

  if (data_name == "covariates") {

    out <- read_covariates(main     = main, 
                           settings = settings)

  }

  if (data_name == "climate_forecasts") {

    out <- read_climate_forecasts(main     = main, 
                                  settings = settings)

  }

  if (data_name == "newmoons") {

    out <- read_newmoons(main     = main, 
                      settings = settings)

  }

  if (data_name == "metadata") {

    out <- read_metadata(main     = main, 
                         settings = settings)

  }

  out
}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function (main     = ".", 
                                dataset  = "all", 
                                settings = directory_settings( )) {

  return_if_null(dataset)
  read.csv(file.path(main, settings$subdirectories$data, paste0("rodents_", tolower(dataset), ".csv"))) 

}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function (main     = ".", 
                          datasets = prefab_datasets( ), 
                          settings = directory_settings( )) {
  
  return_if_null(datasets)
  mapply(FUN = read_rodents_table, dataset = datasets, main = main, SIMPLIFY = FALSE)

}

#' @rdname read_data
#'
#' @export
#'
read_newmoons <- function(main     = ".", 
                       settings = directory_settings( )){
  
  read.csv(file.path(main, settings$subdirectories$data, settings$files$newmoons))

}

#' @rdname read_data
#'
#' @export
#'
read_covariates <- function (main     = ".",
                             settings = directory_settings( )) {

  read.csv(file.path(main, settings$subdirectories$data, settings$files$covariates))

}


#' @rdname read_data
#'
#' @export
#'
read_climate_forecasts <- function (main     = ".",
                                    settings = directory_settings( )) {

  datas <- c(mintemp = "tasmin", meantemp = "tasmean", maxtemp = "tasmax", precipitation = "pr")
  ndatas <- length(datas)
  dat_list <- mapply(FUN = read.csv, file.path(main, settings$subdirectories$resources, files = paste0("/NMME/",  datas, ".csv")), SIMPLIFY = FALSE)

  dat_tab <- dat_list[[1]]
  dat_tab <- dat_tab[ , c(1, ncol(dat_tab))]
  colnames(dat_tab)[ncol(dat_tab)] <- names(datas)[1]
  
  if (ndatas > 1) {

    for (i in 2:ndatas) {

      dat_tab_i <- dat_list[[i]]
      x         <- dat_tab_i[ , ncol(dat_tab_i)]
      dat_tab   <- data.frame(dat_tab, x)
      colnames(dat_tab)[ncol(dat_tab)] <- names(datas)[i]

    }

  }

  colnames(dat_tab)[1] <- "date"
  dat_tab[ , 1]        <- as.Date(dat_tab[,1])

  for (i in 2:ncol(dat_tab)) {

    if(grepl("temp", colnames(dat_tab)[i])) {

      x             <- dat_tab[ , i]
      x[x == -9999] <- NA
      dat_tab[ , i] <- (x - 32) * 5 / 9  
    
    } else if (grepl("precip", colnames(dat_tab)[i])) {

      x             <- dat_tab[ , i]
      x[x == -9999] <- NA
      x[x < 0]      <- 0
      dat_tab[ , i] <- x * 25.4

    }

  }

  dat_tab

}




#' @rdname read_data
#'
#' @export
#'
read_metadata <- function(main     = ".", 
                          settings = directory_settings( )){
  
  read_yaml(file.path(main, settings$subdirectories$data, settings$files$metadata), eval.expr = TRUE)

}


  
#' @title Read in the Casts Metadata File
#'
#' @description Read in the casts metadata file. If the data file does not exist, an effort is made to create the file.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return Data requested.
#'
#' @export
#'
read_casts_metadata <- function (main     = ".",
                                 settings = directory_settings( ), 
                                 quiet    = FALSE){
  
  meta_path <- file.path(main, settings$subdirectories$forecasts, settings$files$forecast_metadata)

  if (!file.exists(meta_path)) {

    messageq("  **creating forecast metadata file**", quiet = quiet)

    casts_meta <- data.frame(cast_id               = 0, 
                             cast_group            = 0, 
                             origin             = NA, 
                             start_moon            = NA, 
                             end_moon              = NA,
                             lead_time             = NA, 
                             model                 = NA, 
                             dataset               = NA,
                             portalcasting_version = NA,
                             QAQC                  = FALSE, 
                             notes                 = NA)

    write.csv(casts_meta, meta_path, row.names = FALSE)

  }

  out <- read.csv(meta_path)


  out[out$cast_group != 0, ]

}
