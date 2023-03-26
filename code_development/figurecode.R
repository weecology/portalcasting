
#' @title Plot the Forecast Coverage and RMSE 
#'
#' @description Plot the coverage (fraction of predictions within the CI) and RMSE (root mean squared error) of each model among multiple species.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), the table will be efficiently (as defined by the inputs) loaded and trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input, otherwise, all relevant casts will be plotted. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting plotting, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param historic_end_newmoonnumbers \code{integer} (or integer \code{numeric}) newmoon number(s) of the forecast origin. Default value is \code{NULL}, which equates to no selection.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param datasets \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{\link[portalr]{forecasting_species}}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @export
#'
plot_casts_cov_RMSE <- function (main                        = ".", 
                                 settings                    = directory_settings( ), 
                                 cast_ids                    = NULL, 
                                 cast_tab                    = NULL, 
                                 historic_end_newmoonnumbers = NULL, 
                                 models                      = NULL, 
                                 datasets                    = "controls", 
                                 species                     = NULL,
                                 quiet                       = FALSE) {


  if (is.null(cast_tab)) {

    cast_choices <- select_casts(main                        = main, 
                                 settings                    = settings, 
                                 cast_ids                    = cast_ids,
                                 historic_end_newmoonnumbers = historic_end_newmoonnumbers, 
                                 models                      = models, 
                                 datasets                    = datasets, 
                                 species                     = species, 
                                 quiet                       = quiet)

    if (NROW(cast_choices) == 0) {

      stop("no casts available for requested plot")

    } else {

      cast_tab <- read_cast_tabs(main     = main, 
                                 settings = settings,
                                 cast_ids = cast_choices$cast_id)
      cast_tab <- add_obs_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
      cast_tab$covered <- cast_tab$obs >= cast_tab$lower_pi & cast_tab$obs <= cast_tab$upper_pi 
      cast_tab$error   <- cast_tab$estimate - cast_tab$obs


    }

  }


  cast_ids                      <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models                        <- ifnull(models, "AutoArima")
  dataset                       <- ifnull(datasets, unique(cast_tab$dataset)[1])
  species                       <- ifnull(species, "DM") 
  historic_end_newmoonnumbers   <- ifnull(historic_end_newmoonnumbers, unique(cast_tab$historic_end_newmoonnumber)) 
  cast_id_in                    <- cast_tab$cast_id %in% cast_ids
  model_in                      <- cast_tab$model %in% models
  dataset_in                    <- cast_tab$dataset == dataset
  species_in                    <- cast_tab$species %in% species
  historic_end_newmoonnumber_in <- cast_tab$historic_end_newmoonnumber %in% historic_end_newmoonnumbers
  all_in                        <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no casts available for requested plot")

  }

  cast_tab <- cast_tab[all_in, ]

  lp    <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")


  nmodels <- length(models) 
  nspecies <- length(species)


  cast_level_errs <- measure_cast_level_error(cast_tab = cast_tab)


  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
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
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = models, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)
  x1 <- 0.49
  x2 <- 0.97
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = models, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)

  for (i in 1:nspecies){

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
      in_ij <- which(cast_level_errs$species == species[i] & 
                     cast_level_errs$model == models[j])

      ys <- na.omit(cast_level_errs$coverage[in_ij])
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
    
    abline(h = 0.95, lwd = 2, lty = 3)

    in_i <- which(cast_level_errs$species == species[i])
    cast_level_errs_i <- cast_level_errs[in_i, ]
    ymax <- max(max(cast_level_errs_i$RMSE, na.rm = TRUE))
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
      in_ij <- which(cast_level_errs$species == species[i] & 
                     cast_level_errs$model == models[j])

      ys <- na.omit(cast_level_errs$RMSE[in_ij])
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
    if (species[i] == "total"){
      spt <- "Total Rodents"
      spf <- 2
    } else{
      spptextmatch <- which(sptab[ , "speciescode"] == species[i])
      spt <- sptab[spptextmatch, "scientificname"]
      spf <- 4
    }  
    text(0.9, 1, spt, font = spf, cex = 0.8, xpd = TRUE, srt = 270)
  }


}


#' @title Plot the Forecast Error as a Function of Lead Time 
#'
#' @description Plot the raw error (estimate - observation) as a function of lead time across model runs from different forecast origins for multiple models and multiple species (or total) within a data set.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), the table will be efficiently (as defined by the inputs) loaded and trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input, otherwise, all relevant casts will be plotted. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting plotting, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param historic_end_newmoonnumbers \code{integer} (or integer \code{numeric}) newmoon number(s) of the forecast origin. Default value is \code{NULL}, which equates to no selection.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param datasets \code{character} value of the rodent data set(s) to include. Default value is \code{"controls"}.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{\link[portalr]{forecasting_species}}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @return \code{NULL}. Plot is generated.
#'
#' @export
#'
plot_casts_err_lead <- function (main                        = ".", 
                                 settings                    = directory_settings( ), 
                                 cast_ids                    = NULL, 
                                 cast_tab                    = NULL, 
                                 historic_end_newmoonnumbers = NULL, 
                                 models                      = NULL, 
                                 datasets                    = "controls", 
                                 species                     = NULL,
                                 quiet                       = FALSE) {


  if (is.null(cast_tab)) {

    cast_choices <- select_casts(main                        = main, 
                                 settings                    = settings, 
                                 cast_ids                    = cast_ids,
                                 historic_end_newmoonnumbers = historic_end_newmoonnumbers, 
                                 models                      = models, 
                                 datasets                    = datasets, 
                                 species                     = species, 
                                 quiet                       = quiet)

    if (NROW(cast_choices) == 0) {

      stop("no casts available for requested plot")

    } else {

      cast_tab <- read_cast_tabs(main     = main, 
                                 settings = settings,
                                 cast_ids = cast_choices$cast_id)
      cast_tab <- add_obs_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
      cast_tab <- add_err_to_cast_tab(main     = main,  
                                      settings = settings,
                                      cast_tab = cast_tab)
      cast_tab <- add_covered_to_cast_tab(main     = main,  
                                          settings = settings,
                                          cast_tab = cast_tab)

    }

  }


  cast_ids                      <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models                        <- ifnull(models, "AutoArima")
  dataset                       <- ifnull(datasets, unique(cast_tab$dataset)[1])
  species                       <- ifnull(species, "DM") 
  historic_end_newmoonnumbers   <- ifnull(historic_end_newmoonnumbers, unique(cast_tab$historic_end_newmoonnumber)) 
  cast_id_in                    <- cast_tab$cast_id %in% cast_ids
  model_in                      <- cast_tab$model %in% models
  dataset_in                    <- cast_tab$dataset == dataset
  species_in                    <- cast_tab$species %in% species
  historic_end_newmoonnumber_in <- cast_tab$historic_end_newmoonnumber %in% historic_end_newmoonnumbers
  all_in                        <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no casts available for requested plot")

  }

  cast_tab <- cast_tab[all_in, ]

  lp    <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")


  nmodels <- length(models) 
  nspecies <- length(species)

  if (nmodels == 1 & nspecies == 1) {

    yy <- round(cast_tab$error, 3)
    yrange <- range(c(0, yy), na.rm = TRUE)
    xrange <- c(max(cast_tab$lead_time_newmoons) + 0.25, 0)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(bty = "L", mar = c(4, 4.5, 3, 1))

    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         ylim = yrange, xlim = xrange)
    abline(h = 0, lty = 3, lwd = 2, col = grey(0.6))
    ucast_ids <- unique(cast_tab$cast_id)
    ncast_ids <- length(ucast_ids)
    cols <- viridis(ncast_ids, 0.8, 0, 0.75)
    for(k in 1:ncast_ids){
      matches <- which(cast_tab$cast_id == ucast_ids[k])
      x <- cast_tab$lead[matches]
      y <- cast_tab$error[matches]
      x <- x[!is.na(y)]
      y <- y[!is.na(y)]
      points(x, y, type = "o", pch = 16, col = cols[k], cex = 1)
    }
    axis(1, cex.axis = 1.25)
    axis(2, cex.axis = 1.25, las = 1)
    mtext(side = 1, "Lead time (new moons)", cex = 1.5, line = 2.75)
    mtext(side = 2, "Forecast error", cex = 1.75, line = 3)

  } else {

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(fig = c(0, 1, 0, 1), 
        mar = c(0.5, 0, 0, 0.5))

    plot(x    = 1, 
         y    = 1, 
         type = "n", 
         xaxt = "n", 
         yaxt = "n", 
         ylab = "", 
         xlab = "", 
         bty  = "n")

    mtext(side = 1, 
          text = "Lead time (new moons)", 
          cex  = 1.25, 
          line = -0.5)
    mtext(side = 2,
          text = "Forecast error", 
          cex  = 1.25, 
          line = -1)

    x1 <- seq(0.04, 0.96 - 0.92/nmodels, 0.92/nmodels)
    x2 <- seq(0.04 + 0.92/nmodels, 0.96, 0.92/nmodels)
    y1 <- seq(0.04, 0.96 - 0.92/nspecies, 0.92/nspecies)
    y2 <- seq(0.04 + 0.92/nspecies, 0.96, 0.92/nspecies)

    for(j in 1:nspecies){

      species_in <- cast_tab$species %in% species[j]
      yy         <- round(cast_tab$error[species_in], 3)
      yrange     <- range(c(0, yy), na.rm = TRUE)

      for(i in 1:nmodels){

        cast_id_in <- cast_tab$cast_id %in% cast_ids


        dataset_in                     <- cast_tab$dataset == dataset
        model_in                       <- cast_tab$model %in% models[i]
        species_in                     <- cast_tab$species %in% species[j]
        historic_end_newmoonnumbers_in <- cast_tab$historic_end_newmoonnumber %in% historic_end_newmoonnumbers

        all_in <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumbers_in

        pcast_tab <- cast_tab[all_in, ]

        par(bty = "L", 
            mar = c(0.5, 0.5, 0.25, 0.25), 
            fig = c(x1[i], x2[i], y1[j], y2[j]),
            new = TRUE)

        xrange <- c(max(pcast_tab$lead_time_newmoons) + 0.25, 0)

        plot(x    = 1, 
             y    = 1, 
             type = "n", 
             xlab = "", 
             ylab = "", 
             xaxt = "n", 
             yaxt = "n",
             ylim = yrange, 
             xlim = xrange)

        abline(h   = 0, 
               lty = 3, 
               lwd = 2, 
               col = grey(0.6))

        ucast_ids <- unique(pcast_tab$cast_id)
        ncast_ids <- length(ucast_ids)

        cols <- viridis(n     = ncast_ids, 
                        alpha = 0.8, 
                        begin = 0, 
                        end   = 0.75)

        for (k in 1:ncast_ids) {

          matches <- which(pcast_tab$cast_id == ucast_ids[k])

          x <- pcast_tab$lead[matches]
          y <- pcast_tab$error[matches]
          x <- x[!is.na(y)]
          y <- y[!is.na(y)]

          points(x    = x, 
                 y    = y, 
                 type = "o", 
                 pch  = 16, 
                 col  = cols[k], 
                 cex  = 0.5)

        }

        xaxl <- ifelse(j == 1, TRUE, FALSE)
        xat1 <- seq(0, max(pcast_tab$lead), 2)
        xat2 <- seq(0, max(pcast_tab$lead), 1)
        yaxl <- ifelse(i == 1, TRUE, FALSE)

        axis(side     = 1, 
             tck      = -0.06, 
             labels   = FALSE, 
             at       = xat1)
        axis(side     = 1, 
             tck      = -0.06, 
             labels   = xaxl, 
             at       = xat1, 
             cex.axis = 0.6, 
             line     = -0.9, 
             lwd      = 0)
        axis(side     = 1, 
             tck      = -0.04, 
             labels   = FALSE, 
             at       = xat2)
        axis(side     = 2, 
             tck      = -0.05, 
             labels   = FALSE)
        axis(side     = 2, 
             tck      = -0.05, 
             labels   = yaxl, 
             cex.axis = 0.6, 
             las      = 1, 
             line     = -0.55, 
             lwd      = 0)

        if (j == nspecies) {

          mod_name <- models[i]

          mtext(side = 3, 
                text = mod_name, 
                cex  = 0.85, 
                font = 2, 
                line = 0.5) 

        }

        if (i == nmodels) {

          par(mar = c(0, 0, 0, 0), 
              fig = c(x2[i], 1, y1[j], y2[j]),
              new = TRUE)   

          plot(x    = 1, 
               y    = 1, 
               type = "n", 
               xaxt = "n", 
               yaxt = "n", 
               ylab = "", 
               xlab = "",
               bty  = "n")   

          if (species[j] == "total") {

            spt <- "Total Abundance"
            spf <- 2

          } else {

            spptextmatch <- which(sptab[ , "speciescode"] == species[j])
            spt <- sptab[spptextmatch, "scientificname"]
            spf <- 4

          }  

          text(0.75, 1, spt, font = spf, cex = 0.55, xpd = TRUE, srt = 270)

        }

      }

    }

  }  

  invisible( )

}



