
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
#' @param overwrite \code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be printed.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'
#'   create_dir(main = "./portalcasting")
#'   download_climate_forecasts(main = "./portalcasting")
#'  } 
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
                                        overwrite     = FALSE,
                                        timeout       = getOption("timeout")) {

  return_if_null(version)

  timeout_backup <- getOption("timeout")
  on.exit(options(timeout = timeout_backup))
  options(timeout = timeout) 

  if (tolower(source) == "nmme") {

    dir.create(path         = file.path(main, resources_sub, source),
               showWarnings = FALSE)

    messageq("Downloading climate forcasts version `", version, "` ...", quiet = quiet)
    

    mapply(FUN      = download.file,
           url      = NMME_urls(start = version, data = data),
           destfile = file.path(main, resources_sub, source, paste0(data, ".csv")), 
           mode     = "wb",
           quiet    = !verbose)


  } else {

    stop("`source` must be 'NMME'")

  }

  invisible()

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
#' @examples
#'   NMME_urls()
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
  nc_data    <- paste0(data, "_", freq, ".nc?var=", data)
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

