#' @title Plot error as a function of lead time for each of a combination of
#'   species and model
#'
#' @description Plot the model-species matrix of error as a function of
#'   lead time.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param species \code{character} vector of the species codes (or 
#'   \code{"total"} for the total across species) to be selected from or 
#'   \code{NULL} to include all species and the total. Default set to 
#'   \code{rodent_spp(set = "evalplot")}, the only setting for which the
#'   plot is reliably coded/ 
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'   or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param ndates \code{integer} number of -cast issue dates to include.
#'
#' @export
#'
plot_err_lead_spp_mods <- function(tree = dirtree(), cast_type = "forecasts", 
                                   species = rodent_spp(set = "evalplot"),
                                   level = "Controls", ndates = 3){
 if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.null(species)){
    if (!("character" %in% class(species))){
      stop("`species` is not a character")
    }
    if (!all(species %in% rodent_spp("wtotal"))){
      stop("invalid entry in `species`")
    }   
  }
  if (!is.character(level)){
    stop("`level` is not a character")
  }
  if (length(level) > 1){
    stop("`level` can only be of length = 1")
  }
  if (level != "All" & level != "Controls"){
    stop("`level` must be 'All' or 'Controls'")
  }
  if (!is.character(cast_type)){
    stop("`cast_type` is not a character")
  }
  if (length(cast_type) > 1){
    stop("`cast_type` can only be of length = 1")
  }
  if (cast_type!= "forecasts" & cast_type != "hindcasts"){
    stop("`cast_type` can only be 'forecasts' or 'hindcasts'")
  }
  if (!is.numeric(ndates)){
    stop("`ndates` is not numeric")
  }
  if (length(ndates) > 1){
    stop("`ndates` can only be of length = 1")
  }
  if (ndates < 1 | ndates %% 1 != 0){
    stop("`ndates` is not a positive integer")
  }

  casts <- read_casts(tree, cast_type = cast_type) %>%
           select_casts(species = species, level = level) %>%
           append_observed_to_cast(tree)

  lpath <- file_path(tree, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath, stringsAsFactors = FALSE) 
  sptab <- na_conformer(sptab, "speciescode")

  uspecies <- unique(casts$species)
  nspecies <- length(uspecies)
  umodels <- unique(casts$model)
  nmodels <- length(umodels)
  if(is.null(ndates)){
    udates <- unique(casts$date)
  } else{
    posdates <- unique(casts$date)
    nposdates <- length(posdates)
    nleads <- rep(NA, nposdates)
    for(i in 1:nposdates){
      incl <- casts$date == posdates[i] & is.na(casts$error) == FALSE
      nleads[i] <- length(unique(casts$lead[incl]))
    }
    posdates2 <- sort(posdates[nleads > 2], decreasing = TRUE)
    udates <- posdates2[c(1, 24, 48, 72)[1:ndates]]
  }
  nudates <- length(udates)
  cols <- viridis(nudates, 1, 0, 0.75)

  par(fig = c(0, 1, 0, 1), mar = c(0.5, 0, 0, 0.5))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n")
  mtext(side = 1, "Lead time (new moons)", line = -0.75)
  mtext(side = 2, "Forecast error", line = -1)
  text(1.39, 1.11, "Forecast Issue ", xpd = TRUE, cex = 0.75)
  text(1.39, 1.095, "Date", xpd = TRUE, cex = 0.75)
  ys <- seq(1.08, by = -0.013, length.out = nudates)
  text(1.39, ys, udates, xpd = TRUE, cex = 0.75, col = cols)

  leads <- casts$lead[which(is.na(casts$error) == FALSE)]
  xrange <- c(max(leads), min(leads))

  incl <- which(casts$species %in% uspecies & casts$model %in% umodels &
                casts$date %in% udates)
  casts <- casts[incl, ]

  rowc <- 1 
  for(i in 1:nspecies){
    colc <- 0
    in_i <- which(casts$species == uspecies[i])
    casts_i <- casts[in_i, ]
    ymin <- min(c(0, min(casts_i$error, na.rm = TRUE)))
    ymax <- max(c(0, max(casts_i$error, na.rm = TRUE)))
    yrange <- c(ymin, ymax)
    for(j in 1:nmodels){
      in_ij <- which(casts$species == uspecies[i] & casts$model == umodels[j])
      casts_ij <- casts[in_ij, ]
      x1 <- 0.07 + (j - 1) * 0.8 * (1/nmodels)
      x2 <- x1 + 0.8 * (1/nmodels)
      y1 <- 0.04 + (i - 1) * 0.92 * (1/nspecies)
      y2 <- y1 + 0.92 * (1/nspecies)
      par(mar = c(0.5, 0, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
      nnonna <- length(which(is.na(casts_ij$error) == FALSE))
      if (nnonna == 0){
        plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",
             bty = "n")
      }else{
        colc <- colc + 1
        plot(casts_ij$lead, casts_ij$error, xaxt = "n", yaxt = "n", ylab = "", 
             xlab = "", type = "n", xlim = xrange, ylim = yrange, bty = "L")
        abline(h = 0, lwd = 2)
        if(rowc == 1){
          axis(1, tck = -0.04, labels = FALSE)
          axis(1, las = 1, cex.axis = 0.65, lwd = 0, line = -1)
        }
        if(colc == 1){
          axis(2, tck = -0.04, labels = FALSE)
          axis(2, las = 1, cex.axis = 0.65, lwd = 0, line = -0.625)
        }
        for(k in 1:nudates){
          casts_ijk <- casts_ij[which(casts_ij$date == udates[k]), ]
          plotx <- casts_ijk$lead[order(casts_ijk$lead)]
          ploty <- casts_ijk$error[order(casts_ijk$lead)]
          plotx <- plotx[-which(is.na(ploty))]
          ploty <- ploty[-which(is.na(ploty))]
          points(plotx, ploty, type = "o", pch = 16, col = cols[k], 
                 cex = 0.75)

        }
      }
      if (rowc == nspecies){
        mtext(side = 3, umodels[j], cex = 0.75, font = 2, line = 0.5) 
      }
      if(j == nmodels){
        par(mar = c(0, 0, 0, 0), fig = c(x2, 0.9, y1, y2), new = TRUE)   
        plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",
             bty = "n")   
        if (uspecies[i] == "total"){
          spt <- "Total Rodents"
          spf <- 2
        } else{
          spptextmatch <- which(sptab[ , "speciescode"] == uspecies[i])
          spt <- sptab[spptextmatch, "scientificname"]
          spf <- 4
        }  
        text(0.75, 1, spt, font = spf, cex = 0.55, xpd = TRUE, srt = 270)
      }
    }
    rowc <- rowc + 1
  }

}

#' @title Select the most abundant species from a forecast or hindcast
#'
#' @description Given a forecast or hindcast, determine the most abundant
#'   species predicted. Currently only reliable for forecasts.
#'
#' @param topx \code{integer}-conformable numeric value for the top number of
#'   species to select.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param species \code{character} vector of the species codes (or 
#'   \code{"total"} for the total across species) to be selected from or 
#'   \code{NULL} to include all species and the total.
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'   or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'   file in the predictions subdirectory. If \code{NULL} (default), the
#'   most recently made -cast is selected. 
#'
#' @param model \code{character} value of the name (or \code{"Ensemble"}) of
#'   the model to be plotted.
#'
#' @param lead \code{integer}-conformable lead of the newmoon number used to
#'   select the data plotted. 
#'
#' @param from_date \code{Date} to be used as a reference of when to count
#'   the \code{lead} from. If \code{NULL} (default), for 
#'   \code{cast_type = "forecasts"}, \code{from_date = cast_date} and 
#'   \code{plot_cast_point} is not yet reliable for 
#'   \code{cast_type = "hindcasts"}.
#'
#' @return \code{character} vector of length \code{topx} of the species codes
#'   (see \code{\link{rodent_spp}}) of the selected species.
#'
#' @export
#'
select_most_ab_spp <- function(topx = 3, tree = dirtree(), 
                               species = rodent_spp(),
                               level = "Controls", cast_type = "forecasts", 
                               cast_date = NULL, model = "Ensemble", 
                               lead = 1, from_date = NULL){

  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.null(species)){
    if (!("character" %in% class(species))){
      stop("`species` is not a character")
    }
    if (!all(species %in% rodent_spp("wtotal"))){
      stop("invalid entry in `species`")
    }   
  }
  if (!is.character(level)){
    stop("`level` is not a character")
  }
  if (length(level) > 1){
    stop("`level` can only be of length = 1")
  }
  if (level != "All" & level != "Controls"){
    stop("`level` must be 'All' or 'Controls'")
  }
  if (!is.character(cast_type)){
    stop("`cast_type` is not a character")
  }
  if (length(cast_type) > 1){
    stop("`cast_type` can only be of length = 1")
  }
  if (cast_type!= "forecasts" & cast_type != "hindcasts"){
    stop("`cast_type` can only be 'forecasts' or 'hindcasts'")
  }
  if (!is.null(cast_date)){
    if (!("Date" %in% class(cast_date))){
      stop("`cast_date` is not of class Date")
    }
    if (length(cast_date) > 1){
      stop("`cast_date` can only be of length = 1")
    }
  } else{
    cast_date <- most_recent_cast(tree, cast_type)
  }
  if (!("character" %in% class(model))){
    stop("`model` is not a character")
  }
  if (length(model) > 1){
    stop("`model` can only be of length = 1")
  }
  if (!is.numeric(lead)){
    stop("`lead` is not numeric")
  }
  if (length(lead) > 1){
    stop("`lead` can only be of length = 1")
  }
  if (lead < 1 | lead %% 1 != 0){
    stop("`lead` is not a positive integer")
  }
  if (is.null(from_date)){
    if(cast_type == "forecasts"){
      from_date <- cast_date
    }
  }
  if (!("Date" %in% class(from_date))){
    stop("`from_date` is not of class Date")
  }
  if (length(from_date) > 1){
    stop("`from_date` can only be of length = 1")
  }
  if (!is.numeric(topx)){
    stop("`topx` is not numeric")
  }
  if (length(topx) > 1){
    stop("`topx` can only be of length = 1")
  }
  if (topx < 1 | topx %% 1 != 0){
    stop("`topx` is not a positive integer")
  }

  metadata <- read_data(tree, "metadata")
  obs <- read_data(tree, tolower(level))
  moons <- read_data(tree, "moons")
  nmdates <- as.Date(as.character(moons$newmoondate))
  most_recent_nm_spot <- max(which(nmdates <= from_date))
  most_recent_nm_number <- moons$newmoonnumber[most_recent_nm_spot]
  cast_nms_io <- metadata$rodent_forecast_newmoons > most_recent_nm_number
  to_include <- which(cast_nms_io)[lead]
  newmoonnumber <- metadata$rodent_forecast_newmoons[to_include]

  pred <- read_cast(tree, cast_type = cast_type, cast_date = cast_date) %>%
          select_casts(species = species, level = level, model = model,
                      newmoonnumber = newmoonnumber)  
  pred$species[order(pred$estimate, decreasing = TRUE)[1:topx]]
}

#' @title Plot forecast or hindcast predictions for a given point in time 
#'   across multiple species.
#'
#' @description Plot the point value with confidence interval for a step in
#'   a forecast or hindcast across multiple species. Currently only reliable
#'   for forecasts.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param species \code{character} vector of the species codes (or 
#'   \code{"total"} for the total across species) to be plotted or 
#'   \code{NULL} (default) to plot all species and the total.
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'   or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'   file in the predictions subdirectory. If \code{NULL} (default), selects
#'   the most recent -casts (if \code{with_census = FALSE}) or the most 
#'   recent -casts with a subsequent census (if \code{with_census = TRUE}).
#'
#' @param model \code{character} value of the name (or \code{"Ensemble"}) of
#'   the model to be plotted.
#'
#' @param lead \code{integer}-conformable lead of the newmoon number used to
#'   select the data plotted. 
#'
#' @param from_date \code{Date} to be used as a reference of when to count
#'   the \code{lead} from. If \code{NULL} (default), for 
#'   \code{cast_type = "forecasts"}, \code{from_date = cast_date} and 
#'   \code{plot_cast_point} is not yet reliable for 
#'   \code{cast_type = "hindcasts"}.
#'
#' @param with_census \code{logical} toggle if the plot should include the
#'   observed data collected during the predicted census.
#'
#' @export
#'
plot_cast_point <- function(tree = dirtree(), species = NULL,
                               level = "Controls", cast_type = "forecasts", 
                               cast_date = NULL, model = "Ensemble", 
                               lead = 1, from_date = NULL, 
                               with_census = FALSE){

  if (length(with_census) > 1){
    stop("`with_census` can only be of length = 1")
  }
  if (!is.logical(with_census)){
    stop("`with_census` is not logical")
  }
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.null(species)){
    if (!("character" %in% class(species))){
      stop("`species` is not a character")
    }
    if (!all(species %in% rodent_spp("wtotal"))){
      stop("invalid entry in `species`")
    }   
  }
  if (!is.character(level)){
    stop("`level` is not a character")
  }
  if (length(level) > 1){
    stop("`level` can only be of length = 1")
  }
  if (level != "All" & level != "Controls"){
    stop("`level` must be 'All' or 'Controls'")
  }
  if (!is.character(cast_type)){
    stop("`cast_type` is not a character")
  }
  if (length(cast_type) > 1){
    stop("`cast_type` can only be of length = 1")
  }
  if (cast_type!= "forecasts" & cast_type != "hindcasts"){
    stop("`cast_type` can only be 'forecasts' or 'hindcasts'")
  }
  if (!is.null(cast_date)){
    if (!("Date" %in% class(cast_date))){
      stop("`cast_date` is not of class Date")
    }
    if (length(cast_date) > 1){
      stop("`cast_date` can only be of length = 1")
    }
  } else{
    cast_date <- most_recent_cast(tree, cast_type, with_census)
  }
  if (!("character" %in% class(model))){
    stop("`model` is not a character")
  }
  if (length(model) > 1){
    stop("`model` can only be of length = 1")
  }
  if (!is.numeric(lead)){
    stop("`lead` is not numeric")
  }
  if (length(lead) > 1){
    stop("`lead` can only be of length = 1")
  }
  if (lead < 1 | lead %% 1 != 0){
    stop("`lead` is not a positive integer")
  }
  if (is.null(from_date)){
    if(cast_type == "forecasts"){
      from_date <- cast_date
    }
  }
  if (!("Date" %in% class(from_date))){
    stop("`from_date` is not of class Date")
  }
  if (length(from_date) > 1){
    stop("`from_date` can only be of length = 1")
  }


  moons <- read_data(tree, "moons")
  casts <- read_cast(tree, cast_type = cast_type, cast_date = cast_date)

  if (with_census){
    obs <- read_data(tree, tolower(level))
    lead <- 1
    most_recent_c_date <- most_recent_census(tree)
    cdates <- as.Date(as.character(moons$censusdate))
    most_recent_c_spot <- which(cdates == most_recent_c_date) 
    newmoonnumber <- moons$newmoonnumber[most_recent_c_spot]
    nmdate <- as.Date(as.character(moons$newmoondate[most_recent_c_spot]))
    newmoonmonth <- as.numeric(format(nmdate, "%m"))
    newmoonyear <- as.numeric(format(nmdate, "%Y"))
    if (length(which(casts$newmoonnumber == newmoonnumber)) == 0){
      stop("census not available for requested date")
    }
  } else{
    nmdates <- as.Date(as.character(moons$newmoondate))
    most_recent_nm_spot <- max(which(nmdates <= from_date))
    most_recent_nm_number <- moons$newmoonnumber[most_recent_nm_spot]
    metadata <- read_data(tree, "metadata")
    cast_nms_io <- metadata$rodent_forecast_newmoons > most_recent_nm_number
    to_plot <- which(cast_nms_io)[lead]
    newmoonnumber <- metadata$rodent_forecast_newmoons[to_plot]
    newmoonmonth <- metadata$rodent_forecast_months[to_plot]
    newmoonyear <- metadata$rodent_forecast_years[to_plot]
  }

  pred <- select_casts(casts, species, level, model, newmoonnumber)  

  pred <- pred[order(pred$estimate, decreasing = TRUE), ]
  nspp <- nrow(pred)
  rangey <- c(nspp + 0.25, 0.75)
  rangex <- c(0, max(pred$UpperPI))

  main_date <- paste(month(newmoonmonth, TRUE), newmoonyear, sep = " ")
  main <- paste0(main_date, ": " , level)

  par(mar = c(3.5, 9.5, 2, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", yaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)
  mtext("Abundance", side = 1, cex = 1.5, line = 2.5)
  mtext("Species", side = 2, cex = 1.5, line = 8.25)
  plotcastpoint_yaxis(tree = tree, species = pred$species)
  mtext(main, side = 3, cex = 1.25, line = 0.5, at = 0, adj = 0)
  
  for(i in 1:nspp){
    low <- pred$LowerPI[i]
    up <- pred$UpperPI[i]
    est <- pred$estimate[i]
    vbars <- i + (0.015 * nspp * c(-1, 1))
    points(c(low, up), rep(i, 2), type = "l", lwd = 2)
    points(rep(low, 2), vbars, type = "l", lwd = 2)
    points(rep(up, 2), vbars, type = "l", lwd = 2)
    points(est, i, pch = 16, col = "white", cex = 1.25)
    points(est, i, lwd = 2, cex = 1.25)

    if (with_census){
      nmmatch <- which(obs$newmoonnumber == newmoonnumber)
      spmatch <- pred$species[i]
      spmatch[spmatch == "NA"] <- "NA."
      obsi <- obs[nmmatch, spmatch]
      points(obsi, i, pch = 16, col = rgb(0, 0.4, 0.9, 0.8), cex = 1.25)
      points(obsi, i, pch = 1, col = rgb(0.2, 0.2, 0.2, 0.8), cex = 1.25)
    }
  }
}

#' @title Expand the names with formating for a multi-species -cast plot y 
#'   axis
#'
#' @description Add the y-axis of formatted species names to a 
#'   \code{\link{plot_cast_point}}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}. Used to access the species list.
#'
#' @param species \code{character} vector of the species codes (or 
#'   \code{"total"} for the total across species) being plotted.
#'
#' @return \code{list} of \code{text} and \code{font} elements. 
#'
#' @export
#'
plotcastpoint_yaxis <- function(tree = dirtree(), species = "total"){

  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.null(species)){
    if (!("character" %in% class(species))){
      stop("`species` is not a character")
    }
    if (!all(species %in% rodent_spp("wtotal"))){
      stop("invalid entry in `species`")
    }   
  }
  lpath <- file_path(tree, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath, stringsAsFactors = FALSE) 
  sptab <- na_conformer(sptab, "speciescode")
  nspp <- length(species)
  for(i in 1:nspp){
    if (species[i] == "total"){
      lab_text <- "Total"
      lab_font <- 1
    } else{
      sppmatch <- which(sptab[ , "speciescode"] == species[i])
      lab_text <- sptab[sppmatch , "scientificname"]
      lab_font <- 3
    }
    axis(2, at = i, labels = lab_text, font = lab_font, las = 1, 
         cex.axis = 0.65, tck = 0, line = -0.5, lwd = 0)
    axis(2, at = i, labels = FALSE, las = 1, 
         cex.axis = 0.65, tck = -0.01)
  }
}


#' @title Visualize a forecast or hindcast
#'
#' @description Plot an observed timeseries and forecast or hindcast 
#'   timeseries with a prediction interval. Currently only reliable for 
#'   forecasts.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param species \code{character} value of the species code or \code{"total"}
#'   for the total across species.
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'   or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'   file in the predictions subdirectory. 
#'
#' @param model \code{character} value of the name (or \code{"Ensemble"}) of
#'   the model to be plotted.
#'
#' @param start_newmoon \code{integer}-conformable newmoon number used as the
#'   the minimum x value for the plot. 
#'
#' @export
#'
plot_cast_ts <- function(tree = dirtree(), species = "total", 
                         level = "Controls", cast_type = "forecasts", 
                         cast_date = NULL, model = "Ensemble", 
                         start_newmoon = 300){

  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.character(species)){
    stop("`species` is not a character")
  }
  if (length(species) > 1){
    stop("`species` can only be of length = 1")
  }
  if (!(species %in% rodent_spp("wtotal"))){
    stop("invalid entry in `species`")
  }   
  if (!is.character(level)){
    stop("`level` is not a character")
  }
  if (length(level) > 1){
    stop("`level` can only be of length = 1")
  }
  if (level != "All" & level != "Controls"){
    stop("`level` must be 'All' or 'Controls'")
  }
  if (!is.character(cast_type)){
    stop("`cast_type` is not a character")
  }
  if (length(cast_type) > 1){
    stop("`cast_type` can only be of length = 1")
  }
  if (cast_type!= "forecasts" & cast_type != "hindcasts"){
    stop("`cast_type` can only be 'forecasts' or 'hindcasts'")
  }
  if (!is.null(cast_date)){
    if (!("Date" %in% class(cast_date))){
      stop("`cast_date` is not of class Date")
    }
    if (length(cast_date) > 1){
      stop("`cast_date` can only be of length = 1")
    }
  } else{
    cast_date <- most_recent_cast(tree, cast_type)
  }
  if (!("character" %in% class(model))){
    stop("`model` is not a character")
  }
  if (length(model) > 1){
    stop("`model` can only be of length = 1")
  }
  if (!is.numeric(start_newmoon)){
    stop("`start_newmoon` is not numeric")
  }
  if (length(start_newmoon) > 1){
    stop("`start_newmoon` can only be of length = 1")
  }
  if(start_newmoon < 1 | start_newmoon %% 1 != 0){
    stop("`start_newmoon` is not a positive integer")
  }

  obs <- read_data(tree, tolower(level))
  pred <- read_cast(tree, cast_type = cast_type, cast_date = cast_date) %>%
          select_casts(species = species, level = level, model = model)   

  species_o <- species
  if (species == "NA"){
    species_o <- "NA."
  }    
  obs_x <- obs[ , "newmoonnumber"]
  obs_y <- obs[ , species_o]
  obs_xf <- obs_x[length(obs_x)]
  obs_yf <- obs_y[length(obs_y)]
  pred_x <- pred[ , "newmoonnumber"]
  pred_ym <- pred[ , "estimate"]
  pred_x2 <- c(obs_xf, pred_x)
  pred_ym2 <- c(obs_yf, pred_ym)
  pred_yl <- pred[ , "LowerPI"] 
  pred_yu <- pred[ , "UpperPI"]
  pred_px <- c(obs_xf, pred_x, pred_x[length(pred_x):1], obs_xf)
  pred_py <- c(obs_yf, pred_yl, pred_yu[length(pred_x):1], obs_yf)

  rangex <- c(max(c(start_newmoon, min(obs_x))), max(pred_x))
  rangey <- c(min(c(obs_y, pred_yl)), max(c(obs_y, pred_yu)))
  ylab <- plotcastts_ylab(tree, species)

  par(mar = c(3, 4.5, 1, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", xaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)
  mtext(ylab$text, side = 2, font = ylab$font, cex = 1.5, line = 3)
  plotcastts_xaxis(tree, rangex)

  points(obs_x, obs_y, type = "l")
  polygon(pred_px, pred_py, col = rgb(0.6757, 0.8438, 0.8984), border = NA)
  points(pred_x2, pred_ym2, type = "l", col = rgb(0, 0, 1))
  
}


#' @title Add a -cast plot x-axis
#'
#' @description Add the x-axis in \code{\link{plot_cast_ts}}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}. Used to access the species list.
#'
#' @param rangex \code{integer}-conformable vector of two values corresponding
#'   to the minimum and maximum newmoonnumbers plotted. 
#'
#' @export
#'
plotcastts_xaxis <- function(tree, rangex){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.numeric(rangex)){
    stop("`rangex` is not numeric")
  }
  if (length(rangex) != 2){
    stop("`start_newmoon` can only be of length = 2")
  }
  if(any(rangex < 1) | any(rangex %% 1 != 0)){
    stop("`rangex` is not a positive integer")
  }

  moons <- read_data(tree, "moons")
  minx <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[1]])
  maxx <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[2]])
  minx_yr <- as.numeric(format(as.Date(minx), "%Y"))
  maxx_yr <- as.numeric(format(as.Date(maxx), "%Y"))
  minx_yr2 <- ceiling(minx_yr/ 5) * 5
  maxx_yr2 <- floor(maxx_yr/ 5) * 5
  yrs <- seq(minx_yr2, maxx_yr2, 5)
  txt <- yrs
  nyd <- paste0(yrs, "-01-01")
  dx <- as.Date(as.character(moons$newmoondate))
  dy <- moons$newmoonnumber
  dmod <- lm(dy ~ dx)
  loc <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(1, at = loc, labels = txt)

  yrs <- seq(minx_yr, maxx_yr, 1)
  nyd <- paste0(yrs, "-01-01")
  loc <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(1, at = loc, labels = FALSE, tck = -0.005)
}

#' @title Expand the name with formating for a -cast plot y label
#'
#' @description Produce a list with \code{text} and \code{font} elements to
#'   control the y-axis label in \code{\link{plot_cast_ts}}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}. Used to access the species list.
#'
#' @param species \code{character} value of the species code or \code{"total"}
#'   for the total across species.
#'
#' @return \code{list} of \code{text} and \code{font} elements. 
#'
#' @export
#'
plotcastts_ylab <- function(tree = dirtree(), species = "total"){

  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.character(species)){
    stop("`species` is not a character")
  }
  if (length(species) > 1){
    stop("`species` can only be of length = 1")
  }
  if (!(species %in% rodent_spp("wtotal"))){
    stop("invalid entry in `species`")
  }   

  lab <- list(text = "", font = 1)
  lpath <- file_path(tree, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath, stringsAsFactors = FALSE) 
  sptab <- na_conformer(sptab, "speciescode")
  if (species == "total"){
    lab$text <- "Total Abundance"
    lab$font <- 1
  } else{
    sppmatch <- which(sptab[ , "speciescode"] == species)
    lab$text <- sptab[sppmatch , "scientificname"]
    lab$font <- 3
  }
  lab
}