
#' @title Save data out to a file and return it	
#'
#' @description Save inputted data out to a data file if requested and 
#'  return it to the console.
#'
#' @param dfl \code{data.frame} or YAML \code{list} to be written out.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param save \code{logical} indicator controlling if \code{x} should 
#'   be saved out.
#'
#' @param filename \code{character} name of the file for saving \code{x}.
#'
#' @param overwrite \code{logical} indicator of if the file should be
#'  overwritten if it exists.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#'
#' @return \code{dfl} as input.
#'
#' @export
#'
write_data <- function (dfl       = NULL, 
                        main      = ".", 
                        save      = TRUE, 
                        filename  = NULL, 
                        overwrite = TRUE, 
                        quiet     = FALSE) {
  
  return_if_null(dfl)

  return_if_null(filename)

  save_it <- FALSE

  if (save) {

    full_path <- file.path(main, "data", filename)

    if (file.exists(full_path)) {

      if (overwrite) {

        save_it <- TRUE

        messageq("    **", filename, " exists and overwrite = TRUE; file saved**", quiet = quiet)

      } else {

        messageq("    **", filename, " exists and overwrite = FALSE; not saved***", quiet = quiet) 
      }

    } else {

      save_it <- TRUE

      messageq("    **", filename, " saved**", quiet = quiet)

    }

    if (save_it) {

      if (file_ext(filename) == "csv") {

        write.csv(dfl, full_path, row.names = FALSE)

      } else if (file_ext(filename) == "yaml"){

        write_yaml(dfl, file = full_path)

      } else {

        stop("file type not supported", call. = FALSE)

      }

    }
   
  }

  dfl

}

#' @title Read in and Format a Portalcasting Data File 
#'
#' @description Read in a specified data file. \cr \cr
#'              Current options include \code{"rodents"} (produces a list), \code{"rodents_table"} (produces a rodents table), \code{"covariates"}, \code{"covariate_casts"}, \code{"moons"}, and \code{"metadata"}, which  are available as calls to \code{read_data} with a specified  \code{data_name} or as calls to the specific \code{read_<data_name>}  functions (like \code{read_moons}). \cr \cr
#'              \code{read_cov_casts} reads in the current or (if no current version) local archived version of the covariate casts.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'  
#' @param data_name \code{character} representation of the data needed. Current options include \code{"rodents"}, \code{"rodents_table"}, \code{"covariates"}, \code{"covariate_forecasts"}, \code{"moons"}, and \code{"metadata"}.
#'
#' @param dataset,datasets \code{character} representation of the grouping name(s) used to define the rodents. Standard options are \code{"all"} and \code{"controls"}. \code{dataset} can only be length 1, \code{datasets} is not restricted in length.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or just tidy messages. 
#'  
#' @return Data requested.
#' 
#' @examples
#' \donttest{
#'  setup_dir()
#'  read_data(data_name = "rodents")
#'  read_data(data_name = "rodents_table")
#'  read_data(data_name = "rodents_table", dataset = "controls")
#'  read_data(data_name = "covariates")
#'  read_data(data_name = "covariate_casts")
#'  read_data(data_name = "moons")
#'  read_data(data_name = "metadata")
#'
#'  read_rodents()
#'  read_rodents_table()
#'  read_covariates()
#'  read_climate_forecasts()
#'  read_covariate_casts()
#'  read_moons()
#'  read_metadata()
#' }
#'
#' @export
#'
read_data <- function (main      = ".", 
                       data_name = NULL, 
                       dataset   = "all", 
                       datasets  = c("all", "controls"), 
                       settings  = directory_settings()){
  
  return_if_null(data_name)

  data_name <- tolower(data_name)

  if (data_name == "rodents") {

    read_rodents(main = main, datasets = datasets)

  }

  if (data_name == "rodents_table") { 

    read_rodents_table(main = main, dataset)

  }

  if (data_name == "covariates") {

    read_covariates(main = main, settings = settings)

  }

  if (data_name == "covariate_casts") {

    read_covariate_casts(main = main, settings = settings)

  }

  if (data_name == "moons") {

    read_moons(main = main, settings = settings)

  }
  if (data_name == "metadata") {

    read_metadata(main = main, settings = settings)

  }

}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function (main    = ".", 
                                dataset = "all") {

  return_if_null(dataset)
  read.csv(file.path(main, "data", paste0("rodents_", tolower(dataset), ".csv"))) 

}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function (main     = ".", 
                          datasets = c("all", "controls")) {
  
  return_if_null(datasets)

  mapply(FUN = read_rodents_table, dataset = datasets, main = main, SIMPLIFY = FALSE)

}

#' @rdname read_data
#'
#' @export
#'
read_moons <- function(main     = ".", 
                       settings = directory_settings()){
  
  read.csv(file.path(main, "data", settings$files$moons))

}

#' @rdname read_data
#'
#' @export
#'
read_covariates <- function (main     = ".",
                             settings = directory_settings()) {

  read.csv(file.path(main, "data", settings$files$covariates))

}



#' @rdname read_data
#'
#' @export
#'
read_climate_forecasts <- function (main = ".") {

  datas <- c(mintemp = "tasmin", meantemp = "tasmean", maxtemp = "tasmax", precipitation = "pr")
  ndatas <- length(datas)
  dat_list <- mapply(FUN = read.csv, file.path(main, "raw", files = paste0("/NMME/",  datas, ".csv")), SIMPLIFY = FALSE)

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
read_covariate_casts <- function(main = ".", settings = directory_settings(),
                           quiet = FALSE, verbose = FALSE){
  
  curr_path <- file.path(main, "data", control_files$filename_cov_casts)
  curr_path2 <- gsub("covariate_casts", "covariate_forecasts", curr_path)

  arch_path <- paste0(control_files$directory, "/data/", 
                      control_files$filename_cov_casts)
  arch_path <- file.path(main, "raw", arch_path)
  arch_path2 <- gsub("covariate_casts", "covariate_forecasts", arch_path)

  if(file.exists(curr_path)){
    cov_cast <- read.csv(curr_path, stringsAsFactors = FALSE)
  } else if (file.exists(curr_path2)){
    cov_cast <- read.csv(curr_path2, stringsAsFactors = FALSE)
  } else {
    if(file.exists(arch_path)){
      cov_cast <- read.csv(arch_path, stringsAsFactors = FALSE)
    } else if (file.exists(arch_path2)){
      cov_cast <- read.csv(arch_path2, stringsAsFactors = FALSE)
    } else {
      msg <- "current and archive versions missing, run `fill_raw`"
      stop(msg, call. = FALSE)
    }
  }
  if(any(grepl("forecast_newmoon", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("forecast_newmoon", "cast_moon", 
                                colnames(cov_cast))
  }
  if(any(grepl("newmoonnumber", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("newmoonnumber", "moon", colnames(cov_cast))
  } 
  cov_cast
}



#' @rdname read_data
#'
#' @export
#'
read_metadata <- function(main = ".", settings = directory_settings()){
  
  fpath <- file.path(main, "data", control_files$filename_meta)
  if(!file.exists(fpath)){
    md <- prep_metadata(main = main)
    return(md)
  }
  yaml.load_file(fpath, eval.exp = TRUE) 
}


  
