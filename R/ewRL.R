# these functions are to be moved to the ewRL package
# the functionality needs to be split out better again 
# see also the code in portalr

#' @title Download the Portal Predictions Repository Archive
#'
#' @description Downloads a specific \code{version} of the Portal Predictions repository from either GitHub or Zenodo (based on \code{source}) into the \code{<main>/raw} sub.
#'
#' @param main \code{character} value defining the main component of the portalcasting directory tree. 
#'
#' @param resources_sub \code{character} value defining the resources subdirectory of the portalcasting directory tree. 
#'
#' @param version \code{character} version of the data to download. Default \code{"latest"} downloads the most recent (by date published). \code{NULL} means no download. 
#'
#' @param source \code{character} indicator of the source for the download. Either \code{"github"} (default) or \code{"github"}.
#'
#' @param pause Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delay. 
#'
#' @param timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be printed.
#'
#' @note There are two calls to \code{link[base]{Sys.sleep}} for \code{pause} seconds each to allow for the file unzipping, copying, and such to catch up.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'
#'   create_dir(main = "./portalcasting")
#'   download_archive(main = "./portalcasting")
#'  } 
#'
#' @name download archive
#'
#' @export
#'
download_archive <- function(main          = ".",
                             resources_sub = "resources",
                             version       = "latest", 
                             source        = "github",
                             quiet         = FALSE,
                             verbose       = FALSE,
                             pause         = 30,
                             timeout       = getOption("timeout")) {


  return_if_null(version)

  timeout_backup <- getOption("timeout")
  on.exit(options(timeout = timeout_backup))
  options(timeout = timeout) 

  version <- tolower(version)
  source  <- tolower(source)

  if (source == "zenodo") {

    got <- GET(url   = "https://zenodo.org/api/records/", 
               query = list(q            = "conceptrecid:833438",
                            size         = 9999, 
                            all_versions = "true"))

    stop_for_status(x    = got,
                    task = paste0("locate Zenodo concept record"))

    contents <- content(x = got)    

    metadata <- lapply(FUN  = getElement, 
                       X    = contents, 
                       name = "metadata")
    versions <- sapply(FUN  = getElement, 
                       X    = metadata, 
                       name = "version")
    pub_date <- sapply(FUN  = getElement, 
                       X    = metadata, 
                       name = "publication_date")

    selected <- ifelse(test = version == "latest",
                       yes  = which.max(as.Date(pub_date)),
                       no   = which(versions == version))

    if (length(selected) == 0){

      stop("Failed to locate version `", version, "` on Zenodo")
   
    }
    
    zipball_url <- contents[[selected]]$files[[1]]$links$download     
    version <- ifelse(test = version == "latest", 
                      yes  = metadata[[selected]]$version,
                      no   = version)

  } else if (source == "github") {

    url <- ifelse(test = version == "latest", 
                  yes  = "https://api.github.com/repos/weecology/portalPredictions/releases/latest",
                  no   = paste0("https://api.github.com/repos/weecology/portalPredictions/releases/tags/", version))

    got <- GET(url = url)

    stop_for_status(x    = got, 
                    task = paste0("locate version `", version, "` on GitHub"))

    zipball_url <- content(got)$zipball_url      
 
    version <- ifelse(test = version == "latest", 
                      yes  = content(got)$tag_name, 
                      no   = version)

    local_version <- 

list.files(file.path(main, resources_sub, "portalPredictions"))

    if (version == local_version) {

      messageq("Local copy of archive is already version ", version, quiet = quiet)
      return(invisible( ))

    }

  } else {

    stop("`source` must be either 'zenodo' or 'github'")

  }
  
  messageq("Downloading archive version `", version, "` ...", quiet = quiet)

  temp  <- file.path(tempdir(), "portalPredictions.zip")
  final <- file.path(main, resources_sub, "portalPredictions")

  result <- tryCatch(
              expr  = download.file(url      = zipball_url, 
                                    destfile = temp, 
                                    quiet    = !verbose, 
                                    mode     = "wb"),
              error = function(x){NA})

  if (is.na(result)) {

    warning("Archive version `", version, "` could not be downloaded")
    return(invisible())

  }


  if (file.exists(final)) {

    old_files <- list.files(path         = final,
                            full.names   = TRUE,
                            all.files    = TRUE,
                            recursive    = TRUE,
                            include.dirs = FALSE)

    file.remove(old_files)

    unlink(x         = final, 
           recursive = TRUE)

  }

  folder_name <- unzip(temp, list = TRUE)$Name[1]

  temp_unzip <- file.path(main, resources_sub, folder_name)

  unzip(temp, exdir = file.path(main, resources_sub))

  Sys.sleep(pause)

  dir.create(final)

  file.copy(list.files(temp_unzip, full.names = TRUE), 
            final, 
            recursive = TRUE)

  Sys.sleep(pause)

  new_version       <- content(got)$tag_name

  version_file_path <- 
  write(new_version, file = version_file_path)

  unlink(temp_unzip, recursive = TRUE)
  file.remove(temp)

  invisible()

}
