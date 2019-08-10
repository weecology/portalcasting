#' @title Download raw components of a forecasting directory
#'
#' @description This suite of functions manages the downloading and (if 
#'  needed) unzipping of raw files associated with the forecasting directory.
#'  \code{download} downloads a file from a from a website into the 
#'  directory, unzipping and cleaning up as needed. \cr \cr
#'  \code{download_message} creates a customized download message. \cr \cr
#'  \code{download_url} prepares the URL from the inputs, depending on the
#'  download type (see \code{Details}). \cr \cr
#'  \code{download_destin} determines the download destination. \cr \cr
#'  \code{unzip_download} unzips any compressed downloads. \cr \cr
#'  \code{unzip_destins} determins the unzipping destinations for any 
#'   compressed downloads. 
#'
#' @param name \code{character} value of the component's name, used to create
#'  the folder within the raw subdirectory. If left as \code{NULL},
#'  \code{\link{record_name_from_url}} tries to obtain a name from the URL,
#'  unless \code{NULLname} is set to \code{TRUE}.
#'
#' @param type \code{character} value representing the type of input. 
#'  Allowable types currently include a raw URL (\code{type = "url"}), which
#'  requires a non-\code{NULL} input for \code{url}, and from Zenodo
#'  (\code{type = "zenodo"}), which requires non-\code{NULL} input for either 
#'  the concept record identifier (\code{concept_rec_id}) and 
#'  version (\code{rec_version}) or record identifier (\code{rec_id})  
#'  (\strong{if \code{rec_id} is used, it overrides 
#'  \code{concept_rec_id}.}) \cr \cr
#'
#' @param url \code{character} value of the URL to be used if 
#'  \code{type = "url"}.
#'  
#' @param concept_rec_id Concept record identifier, a \code{character} value
#'  corresponding to the Zenodo concept, used when \code{type = "zenodo"}.
#'
#' @param rec_version \code{character} value of the version number or 
#'  \code{"latest"} (default) for the data to be download. 
#'  Used when \code{type = "zenodo"}.
#'
#' @param rec_id Optional input record identifier, a \code{character} value
#'  corresponding to the Zenodo record. Used when \code{type = "zenodo"}.
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param NULLname \code{logical} indicator if \code{name} should be kept as
#'  \code{NULL}.
#'
#' @param zip_destin \code{character} value of the destination of the 
#'  download of the zip, which is to be unzipped.
#'
#' @param source_url \code{character} value of the URL from which the download
#'  should occur
#'
#' @details If \code{type = NULL}, it is assumed to be a URL (\emph{i.e.}, 
#'  \code{type = "url"}).
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   download("PortalData", "zenodo", concept_rec_id = "1215988")
#'   source_url <- download_url(type = "zenodo", concept_rec_id = "1215988")
#'   destin <- download_destin(name = "PortalData", source_url = source_url)
#'   download_message(type = "zenodo", source_url, rec_version = "latest")
#'   download.file(source_url, destin, mode = "wb")
#'   unzip_destins("PortalData", destin)
#'   unzip_download("PortalData", destin)
#'  }
#'
#' @export
#'
download <- function(name = NULL, type = NULL, url = NULL, 
                     concept_rec_id = NULL, rec_version = "latest", 
                     rec_id = NULL, 
                     main = ".", quiet = FALSE, cleanup = TRUE, 
                     NULLname = FALSE){
  source_url <- download_url(type, url, concept_rec_id, rec_version, rec_id)
  name <- ifnull(name, record_name_from_url(source_url, NULLname))
  destin <- download_destin(name, source_url, main)
  resp <- GET(source_url)
  stop_for_status(resp)
  download_message(name, type, source_url, rec_version, quiet)
  download.file(source_url, destin, quiet = quiet, mode = "wb")
  extension <- file_ext(source_url)
  if(extension == "zip"){
    unzip_download(name, destin, main, cleanup)
  } 
}



#' @rdname download
#'
#' @export
#'
unzip_download <- function(name = NULL, zip_destin, main = ".", 
                           cleanup = TRUE){
  unzip_destins <- unzip_destins(name, zip_destin, main)
  unzip(zip_destin, exdir = unzip_destins$initial)
print(unzip_destins$final)
  file.rename(unzip_destins$with_archive, unzip_destins$final)
  if(cleanup){
    unlink(unzip_destins$initial, recursive = TRUE)
    unlink(zip_destin)
  }
}

#' @rdname download
#'
#' @export
#'
unzip_destins <- function(name = NULL, zip_destin, main = "."){
  folder <- sub_paths(main, "tmp")
  full <- paste0(folder, "\\", name)
  initial <- normalizePath(full, mustWork = FALSE) 
  add_lev <- unzip(zip_destin, list = TRUE)$Name[1]
  full <- paste0(initial, "\\", add_lev)
  with_archive <- normalizePath(full, mustWork = FALSE)
  folder <- sub_paths(main, subs = subdirs(), "raw")
  full <- paste0(folder, "\\", name)
  final <- normalizePath(full, mustWork = FALSE)
  list(initial = initial, with_archive = with_archive, final = final)
}

#' @rdname download
#'
#' @export
#'
download_message <- function(name = NULL, type = NULL, url = NULL, 
                             rec_version = NULL, quiet = FALSE){
  msg1 <- paste0("Downloading ", name)
  if(is.null(type) || type == "url"){
    msg2 <- paste0(" from ", url)
  } else if (type == "zenodo"){
    msg2 <- paste0(" (", rec_version, ") from zenodo (", url, ")")
  } 
  msg <- paste0(msg1, msg2)
  messageq(msg, quiet)
}

#' @rdname download
#'
#' @export
#'
download_url <- function(type = NULL, url = NULL, concept_rec_id = NULL, 
                         rec_version = "latest", rec_id = NULL){
  type <- ifnull(type, "url")
  type <- tolower(type)
  if(type == "url"){
    url
  } else if (type == "zenodo"){
    zenodo_url(concept_rec_id, rec_version, rec_id)
  } else {
    stop("present types allowed are only `url` and `zenodo`")
  }
}

#' @rdname download
#'
#' @export
#'
download_destin <- function(name = NULL, source_url, main = "."){
  extension <- file_ext(source_url)
  if(extension == "zip"){
    folder <- sub_paths(main, subdirs(), "tmp")
  } else{
    folder <- sub_paths(main, subdirs(), "raw")
  }
  extension2 <- NULL
  if(!is.null(extension)){
    extension2 <- paste0(".", extension)
  } 
  full <- paste0(folder, "\\", name, extension2)
  normalizePath(full, mustWork = FALSE)
}

#' @title Obtain the URL for a Zenodo record to be downloaded
#'
#' @description \code{zenodo_url} obtains the URL for a given Zenodo record, 
#'  identified either by the concept record identifier (\code{concept_rec_id}) 
#'  and version (\code{rec_version}) or record identifier (\code{rec_id}). 
#'  (\strong{Note}: if \code{rec_id} is used, it overrides 
#'  \code{concept_rec_id}). \cr \cr
#'  \code{zenodo_versions}: determines the available version numbers and the 
#'  corresponding record identifier for each version available for a given 
#'  Zenodo concept (group of records).
#'  
#' @param concept_rec_id Concept record identifier, a \code{character} value
#'  corresponding to the Zenodo concept. 
#'
#' @param rec_version \code{character} value of the version number or 
#'   \code{"latest"} (default) for the data to be download. 
#'
#' @param rec_id Optional input record identifier, a \code{character} value
#'  corresponding to the Zenodo record. 
#'
#' @return \code{zenodo_url}: \code{character} value of the URL for the zip 
#'  to be downloaded. \cr \cr
#'  \code{zenodo_versions}: a \code{data.frame} of version number and record 
#'  identifier for each version available.
#' 
#' @examples
#'  \donttest{
#'    zenodo_versions("1215988")
#'    zenodo_url("1215988", "latest")
#'    zenodo_url("1215988", "1.71.0")
#'    zenodo_url(rec_id = "1217163")
#'  }
#'
#' @export
#'
zenodo_url <- function(concept_rec_id = NULL, rec_version = "latest",
                            rec_id = NULL){
  if(is.null(rec_id)){
    avail_versions <- zenodo_versions(concept_rec_id)
    if(rec_version == "latest"){
      rec_id <- concept_rec_id
    } else{
      spot <- which(avail_versions$version == rec_version)
      if(length(spot) == 0){
        stop(paste0("version ", rec_version, " not available"))
      }
      rec_id <- avail_versions$rec_id[spot]
    }
  } else if (!is.null(concept_rec_id)){
    warning("both concept_rec_id and rec_id input. rec_id takes precedence")
  }

  url <- paste0("https://zenodo.org/api/records/", rec_id)
  res <- GET(url)
  stop_for_status(res)
  content(res)$files[[1]]$links$download
}

#' @rdname zenodo_url
#'
#' @export
#'
zenodo_versions <- function(concept_rec_id){
  url <- paste0("https://zenodo.org/api/records/?size=9999&",
                "q=conceptrecid:", concept_rec_id, "&all_versions=True")
  res <- GET(url)
  stop_for_status(res)
  cont <- content(res)
  nv <- length(cont)
  vers <- rep(NA, nv) 
  recid <- rep(NA, nv)
  for(i in 1:nv){
    vers[i] <- cont[[i]]$metadata$version
    recid[i] <- cont[[i]]$record_id
  }
  data.frame(version = vers, rec_id = recid)
}

#' @title Attempt to extract the record name from a URL
#'
#' @description The record name is often encoded in a URL, so in the case that
#'  a name is not given for the record, this function attempts to extract it,
#'  unless told to keep the name as NULL (via \code{NULLname}).
#'
#' @param url \code{character} value of the URL.
#' 
#' @param NULLname \code{logical} indicator of if the name should be kept as
#'  \code{NULL}, rather than given a name based on the URL.
#'
#' @return \code{character} value of the name or \code{NULL}.
#'
#' @examples
#'  source_url <- zenodo_url(concept_rec_id = "1215988")
#'  record_name_from_url(source_url)
#'
#' @export
#'
record_name_from_url <- function(url, NULLname = FALSE){
  if(NULLname){
    NULL
  } else{
    fname <- basename(url)
    fname2 <- file_path_sans_ext(fname)
    strsplit(fname2, "-")[[1]][1]
  }
}

#' @title Create a downloads list for zenodo downloads
#'
#' @description Create a downloads \code{list} for downloads from Zenodo.
#'
#' @param concept_rec_id Concept record identifier, a \code{character} value
#'  corresponding to the Zenodo concept. 
#'
#' @param rec_version \code{character} value of the version number or 
#'  \code{"latest"} for the data to be download. 
#'
#' @param rec_id Optional input record identifier, a \code{character} value
#'  corresponding to the Zenodo record. 
#'
#' @return \code{list} of \code{list}s of arguments to \code{\link{download}}.
#'
#' @examples
#'  zenodo_downloads()
#'
#' @export
#'
zenodo_downloads <- function(concept_rec_id = NULL, rec_version = "latest",
                             rec_id = NULL){
  return_if_null(c(concept_rec_id, rec_id))
  ndls <- max(c(length(concept_rec_id), length(rec_id)))
  out <- vector("list", length = ndls)
  if(!is.null(concept_rec_id)){
   if(length(rec_version) == 1){
     rec_version <- rep(rec_version, ndls)
   }
   for(i in 1:ndls){
     out[[i]] <- list(type = "zenodo", concept_rec_id = concept_rec_id[i],
                      rec_version = rec_version[i])
   }
  } else{
   for(i in 1:ndls){
     out[[i]] <- list(type = "zenodo", rec_id = rec_id[i])
   }
  }
  out
}

#' @title Verify that the raw data folder exists 
#'
#' @description Check that the raw data folder exists.
#'
#' @param raw_path_data \code{character} value indicating the folder path
#'  to the data within the \code{raw} subdirectory but above the files where
#'  the raw data \emph{should} exist. For example, a standard portalcasting 
#'  directory downloads the raw data files into \code{"raw\PortalData"}, 
#'  so \code{raw_location_data = "PortalData"} (as \code{"raw/"} is implied). 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @return \code{logical} indicator of whether or not the raw data folder
#'  exists.
#'
#' @examples
#'  verify_raw_data()
#' 
#' @export
#'
verify_raw_data <- function(raw_path_data = "PortalData", main = "."){
  #folder <- sub_paths(main, subs = subdirs(), "raw")
  #full <- paste0(folder, "/", raw_path_data)

  lpath <- paste0("raw/", raw_path_data)
  full <- file_paths(main, lpath) 

  #full <- file_paths(main, lpath) 
  #full <- file.path(folder, raw_path_data)
  norm_full <- normalizePath(full, mustWork = FALSE)
  file.exists(norm_full)
}


