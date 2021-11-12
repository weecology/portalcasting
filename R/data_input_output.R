#' @title Create, update, and read the directory configuration file
#' 
#' @description The directory configuration file is a special file within
#'  the portalcasting directory setup and has its own set of functions. 
#'  \cr \cr
#'  \code{write_directory_config} creates the \code{dir_config.yaml} file.
#'  It is (and should only be) called from within \code{\link{create_dir}}, 
#'  it captures information about the compute environment used to create
#'  the directory. \cr \cr
#'  \code{update_directory_config} adds key components to the 
#'  \code{dir_config.yaml} file; presently only adding the versions of the
#'  downloaded data from within \code{\link{fill_raw}} (the only place it is
#'  and should be called from. \cr \cr
#'  \code{read_directory_config} reads     - s of the \code{dir_config.yaml} 
#'  file into the R session.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param filename_config \code{character} value of the path to the directory
#'  config YAML.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param downloads_versions \code{character} vector returned from 
#'  \code{fill_raw} of the successfully downloaded versions of
#'  \code{downloads}.
#'
#' @return \code{write_directory_config} and \code{update_directory_config}
#'  both write out the \code{dir_config.yaml} file and return \code{NULL}. 
#'  \cr \cr
#'  \code{read_directory_config}: \code{list} of directory configurations. 
#'
#' @name directory_config
#'
NULL

#' @rdname directory_config
#'
#' @export
#'
write_directory_config <- function(main = ".", 
                                   filename_config = "dir_config.yaml",
                                   quiet = FALSE){
  
  subs <- c("casts", "models", "raw", "data", "tmp")
  config_path <- file_path(main = main, files = filename_config)
  pc_version <- packageDescription("portalcasting", fields = "Version")
  R_version <- sessionInfo()$R.version
  downloads_versions <- NULL
  directory_tree <- list(main = main, subs = subs)
  config <- list(setup_date = as.character(Sys.Date()),
              setup_R_version = R_version,
              setup_portalcasting_version = pc_version,
              directory_tree = directory_tree,
              downloads_versions = downloads_versions) 
  yams <- as.yaml(config)
  writeLines(yams, con = config_path)
  invisible(NULL)
}

#' @rdname directory_config
#'
#' @export
#'
update_directory_config <- function(main = ".", 
                                   filename_config = "dir_config.yaml",
                                    downloads_versions = NULL, 
                                    quiet = FALSE){
  
  config_path <- file_path(main = main, files = filename_config)
  config <- read_directory_config(main = main, 
                                 filename_config = filename_config,
                                 quiet = quiet)
  if(!is.null(downloads_versions)){
    config$downloads_versions <- downloads_versions
  }
  yams <- as.yaml(config)
  writeLines(yams, con = config_path)  
  invisible(NULL)
}



#' @rdname directory_config
#'
#' @export
#'
read_directory_config <- function(main = ".", 
                                  filename_config = "dir_config.yaml",
                                  quiet = FALSE){
  
  config_path <- file_path(main = main, files = filename_config)
  if(!file.exists(config_path)){
    stop("dir_config.yaml file is missing, recreate directory", call. = FALSE)
  }
  yaml.load_file(config_path)  
}

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
write_data <- function(dfl = NULL, main = ".", save = TRUE, filename = NULL, 
                       overwrite = TRUE, quiet = FALSE){
  
  return_if_null(dfl)
  return_if_null(filename)
  save_it <- FALSE
  if(save){
    fext <- file_ext(filename)
    full_path <- file_path(main = main, sub = "data", files = filename)
    f_exists <- file.exists(full_path)
    if(f_exists){
      if(overwrite){
        save_it <- TRUE
        msg <- paste0("    **", filename, 
                      " exists and overwrite = TRUE; file saved**")
      } else {
        msg <- paste0("    **", filename, 
                      " exists and overwrite = FALSE; not saved***") 
      }
    } else{
      save_it <- TRUE
      msg <- paste0("    **", filename, " saved**")
    }
    messageq(msg, quiet = quiet)
    if( save_it){
        if(fext == "csv"){
          write.csv(dfl, full_path, row.names = FALSE)
      } else if (fext == "yaml"){
          yams <- as.yaml(dfl)
          writeLines(yams, con = full_path)
      } else{
        stop("file type not supported", call. = FALSE)
      }
    }
   
  }
  dfl
}

#' @title Read in a data file and format it for specific class
#'
#' @description Read in a specified data file. \cr \cr
#'  Current options include \code{"rodents"} (produces a list), 
#'  \code{"rodents_table"} (produces a rodents table), \code{"covariates"},
#'  \code{"covariate_casts"}, \code{"moons"}, and \code{"metadata"}, which
#'  are available as calls to \code{read_data} with a specified 
#'  \code{data_name} or as calls to the specific \code{read_<data_name>} 
#'  functions (like \code{read_moons}). \cr \cr
#'  If the requested data do not exist, an effort is made to prepare them
#'  using the associated \code{prep_<data_name>} functions (like
#'  \code{prep_moons}).\cr \cr
#'  \code{read_cov_casts} reads in the current or (if no current version)
#'  local archived version of the covariate casts.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'  
#' @param data_name \code{character} representation of the data needed.
#'  Current options include \code{"rodents"}, \code{"rodents_table"}, 
#'  \code{"covariates"}, \code{"covariate_forecasts"}, \code{"moons"}, and 
#'  \code{"metadata"}.
#'
#' @param dataset,datasets \code{character} representation of the grouping
#'  name(s) used to define the rodents. Standard options are \code{"all"} and 
#'  \code{"controls"}. \code{dataset} can only be length 1, 
#'  \code{datasets} is not restricted in length.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
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
#'  read_covariate_casts()
#'  read_moons()
#'  read_metadata()
#' }
#'
#' @export
#'
read_data <- function(main = ".", data_name = NULL, dataset = "all", 
                      datasets = c("all", "controls"), 
                      control_files = files_control()){
  
  return_if_null(data_name)
  data_name <- tolower(data_name)
  dataset <- tolower(dataset)
  datasets <- tolower(datasets)
  if (data_name == "rodents"){
    data <- read_rodents(main = main, datasets = datasets)
  }
  if (data_name == "rodents_table"){
    data <- read_rodents_table(main = main, dataset)
  }
  if (data_name == "covariates"){
    data <- read_covariates(main = main, control_files = control_files)
  }
  if (data_name == "covariate_casts"){
    data <- read_covariate_casts(main = main, control_files = control_files)
  }
  if (data_name == "moons"){
    data <- read_moons(main = main, control_files = control_files)

  }
  if (data_name == "metadata"){
    data <- read_metadata(main = main, control_files = control_files)

  }
  data
}

#' @rdname read_data
#'
#' @export
#'
read_rodents_table <- function(main = ".", dataset = "all"){


  dataset <- tolower(dataset)
  lpath <- paste0("rodents_", dataset, ".csv") 
  fpath <- file_path(main = main, sub = "data", files = lpath)
  if(!file.exists(fpath)){
    rodents <- prepare_rodent_datasets(main = main, datasets = dataset)
    rodents_tab <- rodents[[1]]
    return(rodents_tab)
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_rodents <- function(main = ".", datasets = c("all", "controls")){
  
  return_if_null(datasets)
  datasets <- tolower(datasets)
  ndatasets <- length(datasets)
  rodents <- vector("list", length = ndatasets)
  for(i in 1:ndatasets){
    rodents[[i]] <- read_rodents_table(main = main, dataset = datasets[i])
  }
  names(rodents) <- datasets
  rodents
}


#' @rdname read_data
#'
#' @export
#'
read_covariates <- function(main = ".", control_files = files_control()){
  
  fpath <- file_path(main = main, sub = "data", 
                     files = control_files$filename_cov)
  if(!file.exists(fpath)){
    return(prep_covariates(main = main))
  }
  read.csv(fpath, stringsAsFactors = FALSE) 
}

#' @rdname read_data
#'
#' @export
#'
read_covariate_casts <- function(main = ".", control_files = files_control(),
                           quiet = FALSE, verbose = FALSE){
  
  curr_path <- file_path(main = main, sub = "data", 
                         files = control_files$filename_cov_casts)
  curr_path2 <- gsub("covariate_casts", "covariate_forecasts", curr_path)

  arch_path <- paste0(control_files$directory, "/data/", 
                      control_files$filename_cov_casts)
  arch_path <- file_path(main = main, sub = "raw", files = arch_path)
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
read_moons <- function(main = ".", control_files = files_control()){
  
  fpath <- file_path(main = main, sub = "data", 
                     files = control_files$filename_moons)
  if(!file.exists(fpath)){
    return(prep_moons(main = main))
  }
  read.csv(fpath, stringsAsFactors = FALSE)
}

#' @rdname read_data
#'
#' @export
#'
read_metadata <- function(main = ".", control_files = files_control()){
  
  fpath <- file_path(main = main, sub = "data", 
                     files = control_files$filename_meta, 
)
  if(!file.exists(fpath)){
    md <- prep_metadata(main = main)
    return(md)
  }
  yaml.load_file(fpath) 
}


  
