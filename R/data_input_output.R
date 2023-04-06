#' @title Save Data Out to a File and Return It	(Invisibly)
#'
#' @description Save inputted data out to a data file if requested and return it to the console, [`invisible`][base::invisible]-ly. Currently available for yaml, csv, and json file extensions.
#'
#' @param x `data.frame` or `list` to be written out.
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param subdirectory `character` value defining the data subdirectory of the portalcasting directory tree. 
#'
#' @param save `logic` indicator controlling if `x` should be saved out.
#'
#' @param filename `character` name of the file for saving `x`.
#'
#' @param overwrite `logic` indicator of whether or not file writing sould occur even if a local copy already exists.
#'
#' @param quiet `logic` indicator if messages should be quieted.
#'
#' @return `x` as input, [`invisible`][base::invisible]-ly.
#'
#' @export
#'
write_data <- function (x            = NULL, 
                        main         = ".", 
                        subdirectory = "data",
                        save         = TRUE, 
                        overwrite    = TRUE, 
                        filename     = NULL, 
                        quiet        = FALSE) {
  
  return_if_null(x = x)
  return_if_null(x = filename)


  if (save) {

    full_path <- file.path(main, subdirectory, filename)

    if (file.exists(full_path)) {

      if (overwrite) {

        messageq("    **", filename, " updated**", quiet = quiet)

      } else {

        messageq("  `overwrite` is FALSE, **", filename, " not updated**", quiet = quiet)
        return(invisible(x))

      }

    } else {

      messageq("    **", filename, " saved**", quiet = quiet)

    }

    if (file_ext(filename) == "csv") {

      row_names(x) <- NULL
      write_csv_arrow(x = x, file = full_path)

    } else if (file_ext(filename) == "yaml"){

      write_yaml(x, file = full_path)

    } else if (file_ext(filename) == "json"){

      x2 <- serializeJSON(x = x)
      write_json(x2, path = full_path)

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
#' @param main `character` value of the name of the main component of the directory tree.
#'  
#' @param data_name `character` representation of the data needed. Current options include `"rodents"`, `"rodents_table"`, `"covariates"`, `"climate_forecasts"`, `"newmoons"`, and `"metadata"`.
#'
#' @param dataset,datasets `character` representation of the grouping name(s) used to define the rodents. Standard options are `"all"`, `"controls"`, and `"exclosures"`. `dataset` can only be length 1, `datasets` is not restricted in length.
#'
#' @return Data requested.
#' 
#' @export
#'
read_data <- function (main      = ".", 
                       data_name = NULL, 
                       dataset   = "all", 
                       datasets  = prefab_datasets( )) {
  
  return_if_null(x = data_name)

  data_name <- tolower(data_name)

  if (data_name == "rodents") {

    out <- read_rodents(main     = main, 
                        datasets = datasets)

  }

  if (data_name == "rodents_table") { 

    out <- read_rodents_table(main    = main, 
                              dataset = dataset)

  }

  if (data_name == "covariates") {

    out <- read_covariates(main = main)

  }

  if (data_name == "climate_forecasts") {

    out <- read_climate_forecasts(main = main)

  }

  if (data_name == "newmoons") {

    out <- read_newmoons(main = main)

  }

  if (data_name == "metadata") {

    out <- read_metadata(main = main)

  }

  out
}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function (main    = ".", 
                                dataset = "all") {

  return_if_null(x = dataset)
  settings <- read_directory_settings(main = main)
  as.data.frame(read_csv_arrow(file = file.path(main, settings$subdirectories$data, paste0("rodents_", tolower(dataset), ".csv"))) )

}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function (main     = ".", 
                          datasets = prefab_datasets( )) {
  
  return_if_null(x = datasets)
  settings <- read_directory_settings(main = main)
  mapply(FUN = read_rodents_table, dataset = datasets, main = main, SIMPLIFY = FALSE)

}

#' @rdname read_data
#'
#' @export
#'
read_newmoons <- function(main = "."){
  
  settings <- read_directory_settings(main = main)
  as.data.frame(read_csv_arrow(file = file.path(main, settings$subdirectories$data, settings$files$newmoons)))

}

#' @rdname read_data
#'
#' @export
#'
read_covariates <- function (main = ".") {

  settings <- read_directory_settings(main = main)
  as.data.frame(read_csv_arrow(file = file.path(main, settings$subdirectories$data, settings$files$covariates)))

}


#' @rdname read_data
#'
#' @export
#'
read_climate_forecasts <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  datas <- c(mintemp = "tasmin", meantemp = "tasmean", maxtemp = "tasmax", precipitation = "pr")
  ndatas <- length(datas)
  dat_list <- mapply(FUN = read_csv_arrow, file.path(main, settings$subdirectories$resources, files = paste0("/NMME/",  datas, ".csv")), SIMPLIFY = FALSE)

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
read_metadata <- function(main = "."){
  
  settings <- read_directory_settings(main = main)
  read_yaml(file.path(main, settings$subdirectories$data, settings$files$metadata), eval.expr = TRUE)

}

