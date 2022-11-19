download_file <- function(main          = ".",
                          specific      = "forecasts/casts_metadata.csv",
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

  if (source == "github") {

    url <- gh(paste0("/repos/weecology/portalPredictions/contents/", specific))$download_url

    got <- GET(url = url)

    stop_for_status(x    = got, 
                    task = paste0("locate version `", version, "` on GitHub"))

    zipball_url <- content(got)$zipball_url      
 
    version <- ifelse(version == "latest", content(got)$tag_name, version)

  } else {

    stop("`source` must be 'github'")

  }
  
  messageq("Downloading archive version `", version, "` ...", quiet = quiet)

  temp  <- file.path(tempdir(), "temp.csv")
  final <- file.path(main, resources_sub, "portalPredictions")

  result <- tryCatch(
              expr  = download.file(url      = url, 
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

  unlink(temp_unzip, recursive = TRUE)
  file.remove(temp)

  invisible()

}