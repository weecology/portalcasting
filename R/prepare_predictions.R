#' @title Download the predictions files from the Portal Predictions repo
#'
#' @description Download the files into the predictions subdirectory in the
#'   the portalcasting directory tree from 
#'   \href{https://zenodo.org/record/3333770}{Zenodo} (default) or from the 
#'   \href{https://bit.ly/2Lo2xnQ}{portalPredictions repo}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator of whether the download messages 
#'   are returned to the console.
#' 
#' @param control A \code{list} of arguments to control the 
#'   filling of the predictions subdirectory. See 
#'   \code{\link{predictions_options}}. Arguments not specified assume default
#'   values.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' download_predictions()
#' }
#'
#' @export
#'
download_predictions <- function(tree = dirtree(), quiet = FALSE,
                                 control = list()){
  control <- do.call("predictions_control", control)
  verbose <- control$verbose
  if(control$version != "latest"){
    control$from_zenodo <- FALSE
  }
  releases <- get_releases(control$from_zenodo)
  if (control$version == "latest"){
    match_idx <- 1
  } else{
    grepmatch <- grepl("[0-9]+\\-[0-9]+\\-[0-9]", control$version)
    nchars <- sapply(strsplit(control$version, "-")[[1]], nchar)
    if (!(grepmatch & all(nchars == c(4,2,2)))) {
      stop("Invalid version number; given, ", control$version)
    }
    match_idx <- match(control$version, releases$version)
    if (length(match_idx) != 1 || is.na(match_idx)) {
      stop("Did not find a version of the data matching, ", control$version)
    }
  }
  download_url <- releases$zipball_url[match_idx]
  download_dest <- normalizePath(tempdir())
  download_path <- file.path(download_dest, "portalPredictions.zip")
  download_Npath <- normalizePath(download_path, mustWork = FALSE)
  if(!file.exists(download_Npath) | !control$use_existing_zip){
    msg <- paste0("Downloading files from ", releases$version[match_idx])
    messageq(msg, quiet)
    download.file(download_url, download_Npath, quiet = !verbose, mode = "wb")
    final_folder <- sub_paths(tree, "predictions")
    unzip_path <- file.path(download_dest, "unzips")
    unzip_Npath <- normalizePath(unzip_path, mustWork = FALSE)

    primary_folder <- unzip(download_Npath, list = TRUE)$Name[1]
    unzip(download_Npath, exdir = unzip_Npath)

    full_unzip_path <- file.path(unzip_Npath, list.files(unzip_Npath))
    full_unzip_Npath <- normalizePath(full_unzip_path)
    pred_path <- file.path(full_unzip_Npath, "predictions")
    pred_Npath <- normalizePath(pred_path)
    pred_files_path <- file.path(pred_Npath, list.files(pred_Npath))
    pred_files_Npath <- normalizePath(pred_files_path)

    fc <- file.copy(pred_files_Npath, final_folder, 
                    overwrite = control$update)
    yes <- list.files(pred_Npath)[fc]
    no <- list.files(pred_Npath)[!fc]
    nyes <- length(yes)
    nno <- length(no)
    if (nyes > 0){
      messageq(c("moved: ", yes), !verbose)
    }
    if (nno > 0){
      messageq(c("not moved: ", no), !verbose)
    }
    msg <- paste0(nyes, " predictions files moved, ", nno, " not")
    messageq(msg, quiet)
  } else if (file.exists(download_Npath) & control$use_existing_zip) {
    messageq("Using existing downloaded files", quiet)
    final_folder <- sub_paths(tree, "predictions")
    unzip_path <- file.path(download_dest, "unzips")
    unzip_Npath <- normalizePath(unzip_path, mustWork = FALSE)

    primary_folder <- unzip(download_Npath, list = TRUE)$Name[1]
    unzip(download_Npath, exdir = unzip_Npath)

    full_unzip_path <- file.path(unzip_Npath, list.files(unzip_Npath))
    full_unzip_Npath <- normalizePath(full_unzip_path)
    pred_path <- file.path(full_unzip_Npath, "predictions")
    pred_Npath <- normalizePath(pred_path)
    pred_files_path <- file.path(pred_Npath, list.files(pred_Npath))
    pred_files_Npath <- normalizePath(pred_files_path)

    fc <- file.copy(pred_files_Npath, final_folder, 
                    overwrite = control$update)
    yes <- list.files(pred_Npath)[fc]
    no <- list.files(pred_Npath)[!fc]
    nyes <- length(yes)
    nno <- length(no)
    if (nyes > 0){
      messageq(c("moved: ", yes), !verbose)
    }
    if (nno > 0){
      messageq(c("not moved: ", no), !verbose)
    }
    msg <- paste0(nyes, " predictions files moved, ", nno, " not")
    messageq(msg, quiet)
  } else{
    messageq("files not downloaded and do not exist", quiet)
  }
}

#' @title Get release information about Portal Predictions versions
#'
#' @description 
#'   \code{get_releases}: find and return releases from either
#'   \href{https://zenodo.org/record/3333770}{Zenodo} (default) or from the 
#'   \href{https://bit.ly/2Lo2xnQ}{portalPredictions repo}. \cr \cr
#'   \code{get_latest_zenodo_release}: retrieve latest Zenodo release \cr \cr  
#'   \code{get_github_releases}: retrieve all GitHub releases \cr \cr  
#'
#' @param from_zenodo \code{logical} indicator of whether or not the 
#'   predictions should come from Zenodo (they will come from GitHub if not).
#'
#' @return \code{data.frame} of releases with columns named
#'   \code{version} and \code{zipball_url}.
#'
#' @export
#' 
get_releases <- function(from_zenodo = TRUE){
  if(from_zenodo){
    out <- get_latest_zenodo_release()
  } else{
    out <- get_github_releases()
  }
  if(!is.data.frame(out)){
    out <- NULL
  }
  out
}

#' @rdname get_releases
#'
#' @export
#'
get_github_releases <- function(){
  pat <- Sys.getenv("GITHUB_PAT")
  if (identical(pat, "")) {
    github_auth <- NULL
  } else {
    github_auth <- authenticate(pat, "x-oauth-basic", "basic")
  }
  releases <- data.frame(tag_name = character(), zipball_url = character())
  match_text <- "next"
  page_idx <- 1
  while (match_text == "next" || match_text == "last") {
    github_path1 <- "https://api.github.com/repos/weecology/"
    github_path2 <- "portalPredictions/releases?page="
    github_path <- paste0(github_path1, github_path2, page_idx)
    resp <- GET(github_path, github_auth)
    status <- headers(resp)$status 
    rateleft <- headers(resp)$"x-ratelimit-remaining" 
    httptype <- http_type(resp)
    if (status == "403 Forbidden" && rateleft == "0"){
      stopmsg1a <- "Exceeded GitHub rate limit, please try again in an hour"
      stopmsg1b <- " or consult the documentation for details.\n"
      stopmsg1 <- paste0(stopmsg1a, stopmsg1b)
      stopmsg2 <- "https://developer.github.com/v3/#rate-limiting"
      stop(stopmsg1, stopmsg2)           
    } else if (httptype != "application/json") {
      stop("GitHub response was not in JSON format")
    } else if (status == "401 Unauthorized") {
      stop("Bad GitHub credentials")
    }
    addl <- fromJSON(content(resp, "text"))[, c("tag_name", "zipball_url")]
    releases <- rbind(releases, addl)
    page_idx <- page_idx + 1
    link_str <- headers(resp)$link
    pattern <- "^<.+>; rel=\"([a-z]+)\", <.+>; rel=\"([a-z]+)\"$"
    match_pos <- regexec(pattern, link_str)
    match_text <- regmatches(link_str, match_pos)[[1]][2]
  }
  names(releases) <- c("version", "zipball_url")
  releases
}

#' @rdname get_releases
#'
#' @export
#'
get_latest_zenodo_release <- function(){
  resp <- GET("https://zenodo.org/record/3333770")
  if (http_type(resp) != "text/html") {
      stop("Zenodo response was not in text format", call. = FALSE)
  }
  page_content <- content(resp, "text")
  pattern1 <- "https://zenodo.org/api/files/[0-9a-f\\-]+/"
  pattern2 <- "weecology/[0-9a-zA-z.\\-]+zip"
  pattern <- paste0(pattern1, pattern2)
  match_pos <- regexec(pattern, page_content)
  match_text <- regmatches(page_content, match_pos)
  if (length(match_text) != 1 || length(match_text[[1]]) <= 0) {
    stop("Not able to parse Zenodo for the download link", call. = FALSE)
  }
  zip_download_path <- match_text[[1]][1]
  pattern <- "([0-9]+\\-[0-9]+\\-[0-9]+)\\.zip"
  match_pos <- regexec(pattern, zip_download_path)
  match_text <- regmatches(zip_download_path, match_pos)
  if (length(match_text) != 1 || length(match_text[[1]]) <= 0) {
    stop("Not able to parse Zenodo for the version", call. = FALSE)
  }
  version <- match_text[[1]][2]
  path <- zip_download_path
  data.frame(version = version, zipball_url = path, stringsAsFactors = FALSE)
}



