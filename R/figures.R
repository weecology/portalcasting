


#' @title Plot error as a function of lead time for each of a combination of
#'  species and model
#'
#' @description Plot the model-species matrix of error as a function of
#'  lead time.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param species \code{character} vector of the species codes (or 
#'  \code{"total"} for the total across species) to be selected from or 
#'  \code{NULL} to include all species and the total. Default set to 
#'  \code{rodent_spp(set = "evalplot")}, the only setting for which the
#'  plot is reliably coded/ 
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'  or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'  to select the file in the predictions subdirectory. Currently only 
#'  reliably coded for \code{"forecast"}.
#'
#' @param ndates \code{integer} number of -cast issue dates to include.
#'
#' @param models \code{character} value(s) of the name(s) (or 
#'  \code{"Ensemble"}) of the model(s) of interest. If \code{NULL}, all
#'  models with available -casts are returned.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   plot_err_lead_spp_mods()
#' }
#'
#' @export
#'
plot_err_lead_spp_mods <- function(main = ".", cast_type = "forecast", 
                                   species = rodent_species(set = "evalplot"),
                                   level = "Controls", ndates = 3,
                                   models = model_names(set = "wEnsemble")){

  casts <- read_casts(main, cast_type = cast_type) %>%
           select_casts(species = species, level = level, models = models)
  if (NROW(casts) == 0){
    stop("no casts available for specified inputs")
  } 
  casts <- append_observed_to_cast(casts, main)
 
  lp <- file_paths(main, "raw/PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE) %>% 
           na_conformer("speciescode")

  uspecies <- as.character(unique(casts$species))
  ntspecies <- uspecies[!grepl("total", uspecies)]
  ntspeciesm <- sptab[ , "speciescode"] %in% ntspecies
  ntspeciesn <- sort(sptab[ntspeciesm , "scientificname"], decreasing = TRUE)
  ntspeciesm2 <- rep(NA, length(ntspeciesn))
  for(i in 1:length(ntspeciesm2)){
    ntspeciesm2[i] <- which(sptab[ , "scientificname"] == ntspeciesn[i])
  }
  ntspecies <- sptab[ntspeciesm2, "speciescode"]
  tspecies <- sort(uspecies[grepl("total", uspecies)])
  uspecies <- c(tspecies, ntspecies)
  nspecies <- length(uspecies)
  umodels <- unique(casts$model)
  nemodels <- sort(umodels[!grepl("Ensemble", umodels)])
  emodels <- sort(umodels[grepl("Ensemble", umodels)])
  umodels <- c(nemodels, emodels)
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
    udates <- posdates2[c(1, 12, 24, 48, 72)[1:ndates]]
  }
  nudates <- length(udates)
  cols <- viridis(nudates, 1, 0, 0.75)

  incl <- which(casts$species %in% uspecies & casts$model %in% umodels &
                casts$date %in% udates)
  casts <- casts[incl, ]
  leads <- casts$lead[which(is.na(casts$error) == FALSE)]
  xrange <- c(max(leads) + 1, 0)

  umodels <- unique(casts$model)
  nmodels <- length(umodels)

  par(fig = c(0, 1, 0, 1), mar = c(0.5, 0, 0, 0.5))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n")
  mtext(side = 1, "Lead time (new moons)", line = -0.75)
  mtext(side = 2, "Forecast error", line = -1)
  text(1.39, 1.11, "Forecast Issue ", xpd = TRUE, cex = 0.75)
  text(1.39, 1.095, "Date", xpd = TRUE, cex = 0.75)
  ys <- seq(1.08, by = -0.013, length.out = nudates)
  text(1.39, ys, udates, xpd = TRUE, cex = 0.75, col = cols)

  rowc <- 1 
  for(i in 1:nspecies){
    colc <- 0
    in_i <- which(casts$species == uspecies[i])
    casts_i <- casts[in_i, ]
    ymin <- min(c(0, min(casts_i$error, na.rm = TRUE)))
    ymax <- max(c(0, max(casts_i$error, na.rm = TRUE)))
    ymin <- ymin - 0.05 * (ymax - ymin)
    ymax <- ymax + 0.05 * (ymax - ymin)
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
        axis(1, tck = -0.03, labels = FALSE)
        axis(2, tck = -0.03, labels = FALSE)
        if(rowc == 1){
          axis(1, las = 1, cex.axis = 0.65, lwd = 0, line = -1)
        }
        if(colc == 1){
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



#' @title Plot RMSE and coverage as a function of model for each species
#'
#' @description Plot the RMSE and coverage by species matrix of cast error 
#'  as a function of model.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param species \code{character} vector of the species codes (or 
#'  \code{"total"} for the total across species) to be selected from or 
#'  \code{NULL} to include all species and the total. Default set to 
#'  \code{rodent_spp(set = "evalplot")}, the only setting for which the
#'  plot is reliably coded. 
#'
#' @param level \code{character} value of the treatment type of interest 
#'  (\code{"All"} or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'  to select the file in the predictions subdirectory. Currently only 
#'  reliably coded for \code{"hindcast"}.
#'
#' @param cast_dates \code{Date}s the predictions were made. Used to select 
#'  the files in the predictions subdirectory. Can be length 1 or more and if 
#'  \code{NULL} (default), selects the most recent -casts.
#'
#' @param min_observed \code{integer} value for the minimum number of observed
#'  values needed for a -cast to be retained in the output table. Default is
#'  \code{1}, which returns all -casts with any observations. To include all
#'  -casts (even those without any evaluations), set to \code{0}. 
#'
#' @param models \code{character} value(s) of the name(s) (or 
#'  \code{"Ensemble"}) of the model(s) of interest. If \code{NULL}, all
#'  models with available -casts are returned.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   plot_cov_RMSE_mod_spp()
#'  }
#'
#' @export
#'
plot_cov_RMSE_mod_spp <- function(main = ".", cast_type = "hindcast", 
                                  species = rodent_species(set = "evalplot"),
                                  level = "Controls", cast_dates = NULL, 
                                  min_observed = 1, 
                                  models = model_names(set = "wEnsemble")){

  if (is.null(cast_dates)){
    cast_dates <- most_recent_cast(main, cast_type)
  }

  casts <- read_casts(main = main, cast_type = cast_type, 
                      cast_dates = cast_dates) %>%
           select_casts(species = species, level = level, models = models) 
  if (NROW(casts) == 0){
    stop("no casts available for specified inputs")
  }  
  errs <- append_observed_to_cast(casts, main) %>%
          measure_cast_error(min_observed = min_observed)

  lp <- file_paths(main, "raw/PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE) %>% 
           na_conformer("speciescode")

  uspecies <- as.character(unique(errs$species))
  ntspecies <- uspecies[!grepl("total", uspecies)]
  ntspeciesm <- sptab[ , "speciescode"] %in% ntspecies
  ntspeciesn <- sort(sptab[ntspeciesm , "scientificname"], decreasing = TRUE)
  ntspeciesm2 <- rep(NA, length(ntspeciesn))
  for(i in 1:length(ntspeciesm2)){
    ntspeciesm2[i] <- which(sptab[ , "scientificname"] == ntspeciesn[i])
  }
  ntspecies <- sptab[ntspeciesm2, "speciescode"]
  tspecies <- sort(uspecies[grepl("total", uspecies)])
  uspecies <- c(tspecies, ntspecies)
  nspecies <- length(uspecies)
  umodels <- as.character(unique(errs$model))
  nemodels <- sort(umodels[!grepl("Ensemble", umodels)])
  emodels <- sort(umodels[grepl("Ensemble", umodels)])
  umodels <- c(nemodels, emodels)
  nmodels <- length(umodels)


  par(fig = c(0, 1, 0, 1), mar = c(0.5, 0, 0, 0.5))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n")

  x1 <- 0
  x2 <- 0.48
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = umodels, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)
  x1 <- 0.49
  x2 <- 0.97
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = umodels, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)

  for(i in 1:nspecies){

    x1 <- 0
    x2 <- 0.48
    y1 <- 0.05 + (i - 1) * 0.95 * (1/nspecies)
    y2 <- y1 + 0.94 * (1/nspecies)
    par(mar = c(0, 2.5, 1.5, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", 
         xlab = "", xlim = c(0.5, nmodels + 0.5), ylim = c(0,1), bty = "L")
    axis(2, at = seq(0, 1, 0.2), cex.axis = 0.6, las = 1, line = -0.5, 
         lwd = 0)
    axis(2, at = seq(0, 1, 0.2), labels = FALSE, tck = -0.025)
    mtext(side = 2, "Coverage", line = 1.5, cex = 0.75)
    axis(1, at = 1:nmodels, labels = FALSE, tck = -0.025)
    for(j in 1:nmodels){
      in_ij <- which(errs$species == uspecies[i] & errs$model == umodels[j])

      ys <- errs$coverage[in_ij]
      ys2 <- runif(length(ys), ys - 0.005, ys + 0.005)
      xs <- runif(length(ys), j - 0.05, j + 0.05)
      quants <- quantile(ys, seq(0, 1, 0.25))
      points(rep(j, 2), quants[c(1, 5)], type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[1], 2), type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[5], 2), type = "l")
      rect(j - 0.1, quants[2], j + 0.1, quants[4], col = "white")
      points(c(j - 0.1, j + 0.1), rep(quants[3], 2), type = "l", lwd = 2)
      points(xs, ys2, col = rgb(0.3, 0.3, 0.3, 0.4), pch = 1, cex = 0.5)
    }
    
    abline(h = 0.90, lwd = 2, lty = 3)

    in_i <- which(errs$species == uspecies[i])
    errs_i <- errs[in_i, ]
    ymax <- max(max(errs_i$RMSE, na.rm = TRUE))
    x1 <- 0.49
    x2 <- 0.97
    y1 <- 0.05 + (i - 1) * 0.95 * (1/nspecies)
    y2 <- y1 + 0.94 * (1/nspecies)
    par(mar = c(0, 2.5, 1.5, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", bty = "L",
         xlab = "", xlim = c(0.5, nmodels + 0.5), ylim = c(0, ymax))
    axis(2, cex.axis = 0.6, las = 1, line = -0.5, lwd = 0)
    axis(2, labels = FALSE, tck = -0.025)
    mtext(side = 2, "RMSE", line = 1.625, cex = 0.75)
    axis(1, at = 1:nmodels, labels = FALSE, tck = -0.025)
    for(j in 1:nmodels){
      in_ij <- which(errs$species == uspecies[i] & errs$model == umodels[j])

      ys <- errs$RMSE[in_ij]
      ys2 <- runif(length(ys), ys - 0.005, ys + 0.005)
      xs <- runif(length(ys), j - 0.05, j + 0.05)
      quants <- quantile(ys, seq(0, 1, 0.25))
      points(rep(j, 2), quants[c(1, 5)], type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[1], 2), type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[5], 2), type = "l")
      rect(j - 0.1, quants[2], j + 0.1, quants[4], col = "white")
      points(c(j - 0.1, j + 0.1), rep(quants[3], 2), type = "l", lwd = 2)
      points(xs, ys2, col = rgb(0.3, 0.3, 0.3, 0.4), pch = 1, cex = 0.5)
    }
    par(mar = c(0, 0, 0, 0), fig = c(0.97, 1, y1, y2), new = TRUE)   
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
    text(0.9, 1, spt, font = spf, cex = 0.8, xpd = TRUE, srt = 270)
  }

}