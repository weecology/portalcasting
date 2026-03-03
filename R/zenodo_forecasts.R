#' @title Download Forecasts from Zenodo Production Archive
#'
#' @description Downloads the forecasts directory from the Portal Predictions production Zenodo archive (concept record 10553210). This provides the canonical, complete forecasts including metadata and forecast tables.
#'
#' @param recid `character` or `numeric` Zenodo concept record ID. Default is the production record.
#' @param outdir `character` path to the forecasts output directory. Typically `forecasts_path(main)`.
#' @param main `character` path to main directory. When provided, copies forecasts (and fits if present) to `resources/portal-forecasts/` so downstream functions that use the resource folder continue to work.
#' @param force `logical` indicator to re-download even if forecasts already exist.
#' @param quiet `logical` indicator if messages should be quieted.
#'
#' @return `character` path to the forecasts directory, invisibly.
#'
#' @family downloads
#'
#' @export
#'
download_zenodo_forecasts <- function(
    recid = 10553210,
    outdir = "forecasts",
    main = NULL,
    force = FALSE,
    quiet = FALSE) {

  zenodo_url <- "https://zenodo.org/api"

  get_latest_published_version <- function(record_id) {
    ua <- httr::user_agent("weecology/portal-forecasts")
    response <- httr::RETRY(
      "GET",
      sprintf("%s/records/%s", zenodo_url, record_id),
      ua,
      times = 5,
      pause_base = 2
    )
    httr::stop_for_status(response)
    concept_data <- httr::content(response, as = "parsed", type = "application/json")
    latest_link <- concept_data$links$latest
    latest_record_id <- as.numeric(strsplit(latest_link, "/")[[1]][6])
    return(latest_record_id)
  }

  latest_recid <- get_latest_published_version(recid)
  if (!force && dir.exists(outdir) && length(list.files(outdir)) > 0 && file.exists(file.path(outdir, "version.txt")) && identical(readLines(file.path(outdir, "version.txt"), n = 1, warn = FALSE), as.character(latest_recid))) {
    messageq("Forecasts directory already populated; use force = TRUE to re-download.", quiet = quiet)
    return(invisible(outdir))
  }

  temp_dir <- file.path(tempdir(), "portal_forecasts_temp")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  messageq("Downloading latest version from Zenodo (concept record: ", recid, ") ...", quiet = quiet)

  ua <- httr::user_agent("weecology/portal-forecasts")
  latest <- httr::RETRY(
    "GET",
    sprintf("%s/records/%s", zenodo_url, latest_recid),
    ua,
    times = 5,
    pause_base = 2
  )
  httr::stop_for_status(latest)
  latest_parsed <- httr::content(latest, as = "parsed", type = "application/json")

  archive_url <- latest_parsed$links$archive
  if (is.null(archive_url)) {
    stop("No archive link found for record ", latest_recid)
  }

  archive_path <- file.path(temp_dir, "portal-forecasts.zip")
  messageq("Downloading archive ...", quiet = quiet)
  resp <- httr::RETRY(
    "GET",
    archive_url,
    ua,
    httr::write_disk(archive_path, overwrite = TRUE),
    httr::progress(type = "down"),
    times = 5,
    pause_base = 2
  )
  httr::stop_for_status(resp)

  messageq("Extracting outer archive ...", quiet = quiet)
  utils::unzip(archive_path, exdir = temp_dir, overwrite = TRUE)

  inner_zip_files <- list.files(temp_dir, pattern = "portal-forecasts-.*\\.zip$", full.names = TRUE)
  if (length(inner_zip_files) == 0) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
    stop("Inner portal-forecasts archive not found after extraction")
  }

  messageq("Extracting inner archive ...", quiet = quiet)
  utils::unzip(inner_zip_files[1], exdir = temp_dir, overwrite = TRUE)

  temp_forecasts_path <- file.path(temp_dir, "forecasts")
  if (!dir.exists(temp_forecasts_path)) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
    stop("Forecasts directory not found in archive at: ", temp_forecasts_path)
  }

  messageq("Copying forecasts to ", outdir, " ...", quiet = quiet)
  outdir_parent <- dirname(outdir)
  dir.create(outdir_parent, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = temp_forecasts_path,
    to = outdir_parent,
    recursive = TRUE,
    overwrite = TRUE
  )
  writeLines(as.character(latest_recid), file.path(outdir, "version.txt"))

  if (!is.null(main)) {
    resource_base <- file.path(main, "resources", "portal-forecasts")
    dir.create(resource_base, recursive = TRUE, showWarnings = FALSE)
    subdirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
    for (sd in subdirs) {
      file.copy(from = sd,
                to = resource_base,
                recursive = TRUE,
                overwrite = TRUE)
    }
    messageq("Copied to resources/portal-forecasts for downstream use.", quiet = quiet)
  }

  unlink(temp_dir, recursive = TRUE, force = TRUE)

  messageq("Forecasts copied to: ", normalizePath(outdir, mustWork = FALSE), quiet = quiet)
  invisible(outdir)
}
