
#' @title Download Climate Forecasts
#'
#' @description Downloads climate forecasts, presently only from NMME (see \code{\link{NMME_urls}}) into the \code{<main>/raw} sub.
#'
#' @param main \code{character} value defining the main component of the portalcasting directory tree. 
#'
#' @param resources_sub \code{character} value defining the resources subdirectory of the portalcasting directory tree. 
#'
#' @param source \code{character} indicator of the source for the download. Only \code{"NMME"} presently available.
#'
#' @param data \code{character} vector of the data to be collected. Currently min, mean, and max temperatures and precipitation (\code{c("tasmin", "tasmean", "tasmax", "pr")}).
#'
#' @param version \code{Date}-coercible start of the climate cast. See \code{\link{NMME_urls}} (used as \code{start}).
#'
#' @param timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @param force \code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be generated.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @name download climate forecasts
#'
#' @export
#'
download_climate_forecasts <- function (main          = ".",
                                        resources_sub = "resources",
                                        version       = Sys.Date(), 
                                        source        = "NMME",
                                        data          = c("tasmin", "tasmean", "tasmax", "pr"),
                                        quiet         = FALSE,
                                        verbose       = FALSE,
                                        force         = FALSE,
                                        timeout       = getOption("timeout")) {

  return_if_null(x = version)

  timeout_backup <- getOption("timeout")
  on.exit(options(timeout = timeout_backup))
  options(timeout = timeout) 

  if (tolower(source) == "nmme") {

    dir.create(path         = file.path(main, resources_sub, source),
               showWarnings = FALSE)

    final        <- file.path(main, resources_sub, source)
    version_file <- file.path(final, "version.txt")


    if (!force & file.exists(version_file)) {

      existing_version <- scan(file  = version_file, 
                               what  = character(), 
                               quiet = TRUE)
  

      if (existing_version == version) {

        messageq("Existing local version is up-to-date with remote version (", version, ") requested and `force` is FALSE, download is skipped", quiet = quiet)
        return(invisible( ))

      }
 
    }

    messageq("Downloading climate forecasts version `", version, "` ...", quiet = quiet)
    
    mapply(FUN      = download.file,
           url      = NMME_urls(start = version, data = data),
           destfile = file.path(main, resources_sub, source, paste0(data, ".csv")), 
           mode     = "wb",
           quiet    = !verbose)

  } else {

    stop("`source` must be 'NMME'")

  }

  write(x    = as.character(version), 
        file = version_file)

  invisible( )

}

#' @title URLs for the Northwest Knowledge Network's North American Multi-Model Ensemble (NMME) climate forecasts
#'
#' @description Generate the URL for a specific request to the NMME API based on parameters. See arguments for specifics and \code{Details} for links. 
#'
#' @param start,end \code{Date} for the start and end of the cast.
#'
#' @param model \code{character} value of the model to use, one of \code{"ENSMEAN"}, (Multi-Model Mean), \code{"CMC1"} (CMC1-CanCM3), \code{"CMC2"} (CMC2-CanCM4), \code{"CFCSv2"} (NCEP-CFSv2), \code{"GFDL"} (GFDL-CM2.1), \code{"GFDL-FLOR"} (GFDL-FLOR), or \code{"NCAR"} (NCAR-CCSM4). Presently can only take one value.
#'
#' @param lat,lon \code{numeric} latitude and longitude values used to downscale the model. Presently can only take one value for each.
#'
#' @param freq \code{character} value of the frequency of the data, can be \code{"daily"} or \code{"XmonthAverage"}, where \code{"X"} is a number between \code{1} and \code{7}. Presently can only take one value.
#'
#' @param data \code{character} value of the type of data, one of \code{"tasmin"} (minimum temperature), \code{"tasmean"} (mean temperature), \code{"tasmax"} (maximum temperature), \code{"pr"} (precipitation), \code{"dps"} (dew point), \code{"rsds"} (shortwave radiation; sun intensity), \code{"was"} (wind speed).
#'
#' @details The \href{https://bit.ly/2MifqjM}{Northwest Knowledge Network} (NKN) at the University of Idaho provides a  \href{https://bit.ly/2tCP8NX}{simple API} to download downscaled climate forecasts using the \href{https://bit.ly/2Mdv8gd}{North American Multi-Model Ensemble} (NMME) set. 
#'
#' @return Named \code{character} vector of URLs, or \code{NULL} if \code{data}, \code{freq}, or \code{model} is \code{NULL}.
#'
#' @name NMME urls
#'
#' @export
#'
NMME_urls <- function (start = Sys.Date(), 
                       end   = as.Date("2050-01-01"),
                       model = "ENSMEAN", 
                       lat   = 31.9555, 
                       lon   = -109.0744, 
                       freq  = "daily",
                       data  = c("tasmin", "tasmean", "tasmax", "pr")) {

  return_if_null(data)
  return_if_null(freq)
  return_if_null(model)

  mods <- c("ENSMEAN", "CMC1", "CMC2", "CFCSv2", "GFDL", "GFDL-FLOR", "NCAR")

  if (any(!(model %in% mods))) {

    stop("climate forecast model not in available options")

  }

  datas <- c("tasmin", "tasmean", "tasmax", "pr", "dps", "rsds", "was")

  if (any(!(data %in% datas))) {

    stop("at least one climate forecast dataset not in available options")

  }

  freqs <- c("daily", paste0(1:7, "monthAverage"))

  if (any(!(freq %in% freqs))) {

    stop("frequency of predictions requested not available")

  }
  
  thredds     <- "https://tds-proxy.nkn.uidaho.edu/thredds/"
  nwcsc       <- "ncss/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/"
  nmme        <-  "bcsd-nmme/dailyForecasts/"
  catalog     <- paste0(thredds, nwcsc, nmme)

  nc_model    <- paste0("bcsd_nmme_metdata_", model, "_forecast_")
  nc_data     <- paste0(data, "_", freq, ".nc?var=", data)
  og_nc_data  <- paste0(nc_model, nc_data)

  start_time  <- paste0(start, "T00%3A00%3A00Z")
  end_time    <- paste0(end, "T00%3A00%3A00Z")
  locale      <- paste0("&latitude=", lat, "&longitude=", lon)
  times       <- paste0("&time_start=", start_time, "&time_end=", end_time)
  extension   <- "&accept=csv"
  specs       <- paste0(locale, times, extension)

  urls        <- paste0(catalog, og_nc_data, specs)
  names(urls) <- data
  urls

}




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
#' @param source \code{character} indicator of the source for the download. Either \code{"github"} (default) or \code{"zenodo"}.
#'
#' @param pause Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delay. 
#'
#' @param timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @param force \code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be generated.
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
                             force         = FALSE,
                             pause         = 30,
                             timeout       = getOption("timeout")) {


  return_if_null(x = version)

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

  if (!force & file.exists(version_file)) {

    existing_version <- scan(file  = version_file, 
                             what  = character(), 
                             quiet = TRUE)
  

    if (existing_version == version) {

      messageq("Existing local version is up-to-date with remote version (", version, ") requested and `force` is FALSE, download is skipped", quiet = quiet)
      return(invisible( ))

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
    return(invisible( ))

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

  file.rename(from = temp_unzip,
              to   = final)

  write(x    = version, 
        file = version_file)

  Sys.sleep(pause)

  unlink(temp_unzip, recursive = TRUE)
  file.remove(temp)

  invisible( )

}

