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
#' @param overwrite \code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be printed.
#'
#' @note There are two calls to \code{link[base]{Sys.sleep}} for \code{pause} seconds each to allow for the file unzipping, copying, and such to catch up.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
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
                             overwrite     = FALSE,
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

    url <- ifelse(version == "latest", 
                  "https://api.github.com/repos/weecology/portalPredictions/releases/latest",
                  paste0("https://api.github.com/repos/weecology/portalPredictions/releases/tags/", version))

    got <- GET(url = url)

    stop_for_status(x    = got, 
                    task = paste0("locate version `", version, "` on GitHub"))

    zipball_url <- content(got)$zipball_url      
 
    version <- ifelse(version == "latest", content(got)$tag_name, version)

  } else {

    stop("`source` must be either 'zenodo' or 'github'")

  }
  

  temp         <- file.path(tempdir(), "portalPredictions.zip")
  final        <- file.path(main, resources_sub, "portalPredictions")
  version_file <- file.path(final, "version.txt")


  if (!overwrite & file.exists(version_file)) {

    existing_version <- scan(file  = version_file, 
                             what  = character(), 
                             quiet = TRUE)
  

    if (existing_version == version) {

      messageq("Existing local version (", existing_version, ") is up-to-date with remote version (", version, ") requested and `overwrite` is FALSE, download is skipped",
               quiet = quiet)
      return(invisible())

    }

  }


  messageq("Downloading archive version `", version, "` ...", quiet = quiet)

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

  write(x    = version, 
        file = version_file)

  Sys.sleep(pause)

  unlink(temp_unzip, recursive = TRUE)
  file.remove(temp)

  invisible()

}
