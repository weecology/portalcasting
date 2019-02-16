#' @title Plot forecast or hindcast predictions for a given point in time 
#'   across multiple species.
#'
#' @description Plot the point value with confidence interval for a step in
#'   a forecast or hindcast across multiple species.
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
#' @param castdate \code{Date} the predictions were made. Used to select the
#'   file in the predictions subdirectory. 
#'
#' @param model \code{character} value of the name (or \code{"Ensemble"}) of
#'   the model to be plotted.
#'
#' @param lead \code{integer}-conformable lead of the newmoon number used to
#'   select the data plotted. 
#'
#' @export
#'
plot_species_casts <- function(tree = dirtree(), species = NULL,
                             level = "Controls",
                             cast_type = "forecasts", castdate = today(),
                             model = "Ensemble", lead = 1){

  metadata <- read_data(tree, "metadata")
  obs <- read_data(tree, tolower(level))
  newmoonnumber <- metadata$rodent_forecast_newmoons[lead]
  pred <- read_casts(tree, casttype = casttype, castdate = castdate) %>%
          select_casts(species = species, level = level, model = model,
                       newmoonnumber = newmoonnumber)  

  pred <- pred[order(pred$estimate, decreasing = TRUE), ]
  nspp <- nrow(pred)
  rangey <- c(nspp + 0.25, 0.75)
  rangex <- c(0, max(pred$UpperPI))

  par(mar = c(3.5, 9.5, 1, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", yaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)
  mtext("Abundance", side = 1, cex = 1.5, line = 2.5)
  mtext("Species", side = 2, cex = 1.5, line = 8.25)
  sppcastsplot_yaxis(tree = tree, species = pred$species)

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
  }
}

#' @title Expand the names with formating for a multi-species -cast plot y 
#'   axis
#'
#' @description Add the y-axis of formatted species names to a 
#'   \code{\link{plot_species_casts}}.
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
sppcastsplot_yaxis <- function(tree = dirtree(), species = "total"){

  lpath <- file_path(tree, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath, stringsAsFactors = FALSE) 
  nasppname <- which(is.na(sptab[ , "speciescode"]) == TRUE)
  if (length(nasppname) == 1){
    sptab[nasppname, "speciescode"] <- "NA"
  }
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
#' @param casttype \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param castdate \code{Date} the predictions were made. Used to select the
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
plot_cast <- function(tree = dirtree(), species = "total", level = "Controls",
                      casttype = "forecasts", castdate = today(),
                      model = "Ensemble", start_newmoon = 300){

  obs <- read_data(tree, tolower(level))
  pred <- read_casts(tree, casttype = casttype, castdate = castdate) %>%
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
  ylab <- castplot_ylab(tree, species)

  par(mar = c(3, 4.5, 1, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", xaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)
  mtext(ylab$text, side = 2, font = ylab$font, cex = 1.5, line = 3)
  castplot_xaxis(tree, rangex)

  points(obs_x, obs_y, type = "l")
  polygon(pred_px, pred_py, col = rgb(0.6757, 0.8438, 0.8984), border = NA)
  points(pred_x2, pred_ym2, type = "l", col = rgb(0, 0, 1))
  
}


#' @title Add a -cast plot x-axis
#'
#' @description Add the x-axis in \code{\link{plot_cast}}.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}. Used to access the species list.
#'
#' @param rangex \code{integer}-conformable vector of two values corresponding
#'   to the minimum and maximum newmoonnumbers plotted. 
#'
#' @export
#'
castplot_xaxis <- function(tree, rangex){
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
#'   control the y-axis label in \code{\link{plot_cast}}.
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
castplot_ylab <- function(tree = dirtree(), species = "total"){
  lab <- list(text = "", font = 1)
  lpath <- file_path(tree, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath, stringsAsFactors = FALSE) 
  nasppname <- which(is.na(sptab[ , "speciescode"]) == TRUE)
  if (length(nasppname) == 1){
    sptab[nasppname, "speciescode"] <- "NA"
  }
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