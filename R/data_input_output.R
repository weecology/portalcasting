#' @title Read from and Write to a Data File 
#'
#' @description Generalized data input-output functionality with specifics for common files. \cr \cr 
#'              `write_data` saves inputted data out to a data file if requested and returns it to the console, [`invisible`][base::invisible]-ly. Currently available for `yaml`, `csv`, and `json` file extensions. \cr \cr
#'              `read_data` reads in a specified data file. Specific functions available include `read_rodents`, `read_rodents_dataset`, `read_covariates`, `read_climate_forecasts`, `read_newmoons`, and `read_metadata`. 
#'
#' @param x Data, such as a `data.frame` or `list`, to be written out.
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
#' @param data_name `character` representation of the data needed. Current options include `"rodents"`, `"rodents_table"`, `"covariates"`, `"climate_forecasts"`, `"newmoons"`, and `"metadata"`.
#'
#' @param dataset,datasets `character` representation of the grouping name(s) used to define the rodents. Standard options are `"all"`, `"controls"`, and `"exclosures"`. `dataset` can only be length 1, `datasets` is not restricted in length.
#'
#' @return `write_data` `x` as input, [`invisible`][base::invisible]-ly.
#'
#' @name read write data
#'
#' @examples  
#'  \donttest{
#'    main1 <- file.path(tempdir(), "standard")
#'    setup_dir(main = main1)
#'    write_data(main = main1, x = data.frame(rnorm(10)), filename = "xx.csv")
#'    read_data(main = main1)
#'    read_rodents(main = main1)
#'    read_rodents_dataset(main = main1)
#'    read_covariates(main = main1)
#'    read_climate_forecasts(main = main1)
#'    read_newmoons(main = main1)
#'    read_metadata(main = main1)
#'    unlink(main1, force = TRUE, recursive = TRUE)
#'  }
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

      row.names(x) <- NULL
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

#' @rdname read-write-data
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

  if (data_name == "rodents_dataset") { 

    out <- read_rodents_dataset(main    = main, 
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

#' @rdname read-write-data
#'
#' @export
#'
read_rodents_dataset <- function (main    = ".", 
                                  dataset = "all") {

  return_if_null(x = dataset)

  as.data.frame(read_csv_arrow(file = rodents_dataset_path(main    = main, 
                                                           dataset = dataset)))

}

#' @rdname read-write-data
#'
#' @export
#'
read_rodents <- function (main     = ".", 
                          datasets = prefab_datasets( )) {
  
  return_if_null(x = datasets)

  mapply(FUN      = read_rodents_dataset, 
         dataset  = datasets, 
         main     = main, 
         SIMPLIFY = FALSE)

}

#' @rdname read-write-data
#'
#' @export
#'
read_newmoons <- function(main = "."){
  
  as.data.frame(read_csv_arrow(file = newmoons_path(main = main)))

}

#' @rdname read-write-data
#'
#' @export
#'
read_covariates <- function (main = ".") {

  as.data.frame(read_csv_arrow(file = covariates_path(main = main)))

}

#' @rdname read-write-data
#'
#' @export
#'
read_metadata <- function(main = "."){
  
  read_yaml(file = metadata_path(main = main), eval.expr = TRUE)

}

#' @rdname read-write-data
#'
#' @export
#'
read_climate_forecasts <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  dat_list <- mapply(FUN = read_csv_arrow, climate_forecasts_paths(main = main), SIMPLIFY = FALSE)
  ndatas <- length(dat_list)
  dat_list <- lapply(dat_list, FUN = as.data.frame)
  dat_tab <- dat_list[[1]]
  dat_tab <- dat_tab[ , c(1, ncol(dat_tab))]
  colnames(dat_tab)[ncol(dat_tab)] <- names(dat_list)[1]
  
  if (ndatas > 1) {

    for (i in 2:ndatas) {

      dat_tab_i <- dat_list[[i]]
      x         <- dat_tab_i[ , ncol(dat_tab_i)]
      dat_tab   <- data.frame(dat_tab, x)
      colnames(dat_tab)[ncol(dat_tab)] <- names(dat_list)[i]

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



