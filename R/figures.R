#' @title Plot the Forecast Error as a Function of Lead Time 
#'
#' @description Plot the raw error (estimate - observation) as a function of lead time across model runs from different forecast origins for multiple models and multiple species (or total) within a data set.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting plotting, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param historic_end_newmoonnumbers \code{integer} (or integer \code{numeric}) newmoon number(s) of the forecast origin. Default value is \code{NULL}, which equates to no selection.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param datasets \code{character} value of the rodent data set(s) to include. Default value is \code{"controls"}.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{\link[portalr]{forecasting_species}}.
#'
#' @return \code{NULL}. Plot is generated.
#'
#' @export
#'
plot_casts_err_lead <- function (main                        = ".", 
                                 cast_ids                    = NULL, 
                                 historic_end_newmoonnumbers = NULL, 
                                 models                      = NULL, 
                                 datasets                    = NULL, 
                                 species                     = NULL) {

  settings <- read_directory_settings(main = main)

  evals    <- read_casts_evaluations(main)

  models                        <- ifnull(models, "AutoArima")
  datasets                      <- ifnull(datasets, "controls")
  species                       <- ifnull(species, "DM") 
  cast_ids                      <- ifnull(cast_ids, unique(evals$cast_id))
  historic_end_newmoonnumbers   <- ifnull(historic_end_newmoonnumbers, unique(evals$historic_end_newmoonnumber)) 

  cast_id_in                    <- evals$cast_id %in% cast_ids
  model_in                      <- evals$model %in% models
  dataset_in                    <- evals$dataset == datasets
  species_in                    <- evals$species %in% species
  historic_end_newmoonnumber_in <- evals$historic_end_newmoonnumber %in% historic_end_newmoonnumbers
  all_in                        <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no evaluations available for requested plot")

  }

  evals_in <- evals[all_in, ]

  nmodels <- length(models) 
  nspecies <- length(species)

  if (nmodels == 1 & nspecies == 1) {

    yy <- round(evals_in$error, 3)
    yrange <- range(c(0, yy), na.rm = TRUE)
    xrange <- c(max(evals_in$lead_time_newmoons) + 0.25, 0)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(bty = "L", mar = c(4, 4.5, 3, 1))

    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         ylim = yrange, xlim = xrange)
    abline(h = 0, lty = 3, lwd = 2, col = grey(0.6))
    ucast_ids <- unique(evals_in$cast_id)
    ncast_ids <- length(ucast_ids)
    cols <- viridis(ncast_ids, 0.8, 0, 0.75)
    for(k in 1:ncast_ids){
      matches <- which(evals_in$cast_id == ucast_ids[k])
      x <- evals_in$lead_time_newmoons[matches]
      y <- evals_in$error[matches]
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

    x1 <- seq(0.04, 0.96 - 0.92/max(c(2, nmodels)), 0.92/max(c(2, nmodels)))
    x2 <- seq(0.04 + 0.92/max(c(2, nmodels)), 0.96, 0.92/max(c(2, nmodels)))
    y1 <- seq(0.04, 0.96 - 0.92/max(c(2, nspecies)), 0.92/max(c(2, nspecies)))
    y2 <- seq(0.04 + 0.92/max(c(2, nspecies)), 0.96, 0.92/max(c(2, nspecies)))

    for(j in 1:nspecies){

      species_in <- evals_in$species %in% species[j]
      yy         <- round(evals_in$error[species_in], 3)
      yrange     <- range(c(0, yy), na.rm = TRUE)

      for(i in 1:nmodels){

        cast_id_in <- evals_in$cast_id %in% cast_ids


        dataset_in                     <- evals_in$dataset %in% datasets
        model_in                       <- evals_in$model %in% models[i]
        species_in                     <- evals_in$species %in% species[j]
        historic_end_newmoonnumbers_in <- evals_in$historic_end_newmoonnumber %in% historic_end_newmoonnumbers

        all_in <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumbers_in

        pevals_in <- evals_in[all_in, ]

        par(bty = "L", 
            mar = c(0.5, 0.5, 0.25, 0.25), 
            fig = c(x1[i], x2[i], y1[j], y2[j]),
            new = TRUE)

        xrange <- c(max(pevals_in$lead_time_newmoons) + 0.25, 0)

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

        ucast_ids <- unique(pevals_in$cast_id)
        ncast_ids <- length(ucast_ids)

        cols <- viridis(n     = ncast_ids, 
                        alpha = 0.8, 
                        begin = 0, 
                        end   = 0.75)

        for (k in 1:ncast_ids) {

          matches <- which(pevals_in$cast_id == ucast_ids[k])

          x <- pevals_in$lead[matches]
          y <- pevals_in$error[matches]
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
        xat1 <- seq(0, max(pevals_in$lead), 2)
        xat2 <- seq(0, max(pevals_in$lead), 1)
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

            spt <- species[j]
            spf <- 4

          }  

          text(0.75, 1, spt, font = spf, cex = 0.55, xpd = TRUE, srt = 270)

        }

      }

    }

  }  

  invisible( )

}



#' @title Plot the Forecast Coverage and RMSE 
#'
#' @description Plot the coverage (fraction of predictions within the CI) and RMSE (root mean squared error) of each model among multiple species.
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
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param datasets \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{\link[portalr]{forecasting_species}}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @export
#'
plot_casts_cov_RMSE <- function (main                        = ".", 
                                 cast_ids                    = NULL, 
                                 historic_end_newmoonnumbers = NULL, 
                                 models                      = NULL, 
                                 datasets                    = NULL, 
                                 species                     = NULL) {

  settings <- read_directory_settings(main = main)

  evals    <- read_casts_evaluations(main)

  models                        <- ifnull(models, "AutoArima")
  datasets                      <- ifnull(datasets, "controls")
  species                       <- ifnull(species, "DM") 
  cast_ids                      <- ifnull(cast_ids, unique(evals$cast_id))
  historic_end_newmoonnumbers   <- ifnull(historic_end_newmoonnumbers, unique(evals$historic_end_newmoonnumber)) 

  cast_id_in                    <- evals$cast_id %in% cast_ids
  model_in                      <- evals$model %in% models
  dataset_in                    <- evals$dataset == datasets
  species_in                    <- evals$species %in% species
  historic_end_newmoonnumber_in <- evals$historic_end_newmoonnumber %in% historic_end_newmoonnumbers
  all_in                        <- cast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no evaluations available for requested plot")

  }

  evals_in <- evals[all_in, ]

  nmodels <- length(models) 
  nspecies <- length(species)

  ucasts <- unique(evals_in$cast_id)
  ncasts <- length(ucasts)

  cast_RMSE     <- numeric(ncasts)
  cast_coverage <- numeric(ncasts)
  cast_model    <- character(ncasts)
  cast_species  <- character(ncasts)

  for (i in 1:ncasts) {

    cast_RMSE[i]     <- sqrt(mean(evals_in$error[evals_in$cast_id == ucasts[i]] ^ 2, na.rm = TRUE))
    cast_coverage[i] <- mean(evals_in$covered[evals_in$cast_id == ucasts[i]], na.rm = TRUE)
    cast_model[i]    <- evals_in$model[evals_in$cast_id == ucasts[i]][1]
    cast_species[i]  <- evals_in$species[evals_in$cast_id == ucasts[i]][1]

  }


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
      in_ij <- which(cast_species == species[i] & 
                     cast_model == models[j])

      ys <- na.omit(cast_coverage[in_ij])
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

    in_i <- which(cast_species == species[i])
    ymax <- max(cast_RMSE[in_i], na.rm = TRUE)
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
      in_ij <- which(cast_species == species[i] & 
                     cast_model == models[j])

      ys <- na.omit(cast_RMSE[in_ij])
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
      spt <- species[j]
      spf <- 4
    }  
    text(0.9, 1, spt, font = spf, cex = 0.8, xpd = TRUE, srt = 270)
  }


  invisible( )

}




#' @title Plot Predictions for a Given Point in Time Across Multiple Species
#'
#' @description Plot the point value with confidence interval for a time point across multiple species. Casts can be selected either by supplying a \code{cast_id} number or any combination of \code{dataset}, \code{model}, and \code{historic_end_newmoonnumber}, which filter the available casts in unison. This plot type can only handle output from a single cast, so if multiple casts still remain, the one with the highest number is selected. To be more certain about cast selection, use the \code{cast_id} input.
#'
#' @details The resulting plot shows predictions as points (open white circles) with error, where the point represents the \code{estimate} and the bounds of the error are \code{lower_pi} and \code{upper_pi} in the \code{cast_table} saved output from a model. \cr
#'  As of \code{portalcasting v0.9.0}, this represents the mean and the 95\% prediction interval. If \code{with_census = TRUE}, the observations from the associated moon are plotted as blue filled squares. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param historic_end_newmoonnumber \code{integer} (or integer \code{numeric}) newmoon number of the forecast origin. Default value is \code{NULL}, which equates to no selection.
#'
#' @param model \code{character} value of the name of the model to include. Default value is \code{NULL}, which equates to no selection.
#'
#' @param dataset \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param newmoonnumber \code{integer} (or integer \code{numeric}) newmoon number for the plot. 
#'
#' @param cast_id \code{integer} (or integer \code{numeric}) value representing the cast of interest, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param species \code{character} vector of the species codes (or \code{"total"} for the total across species) to be plotted or \code{NULL} (default) to plot all species in \code{dataset}. 
#' 
#' @param highlight_sp \code{character} vector of the species codes (or \code{"total"} for the total across species) to be highlighted or \code{NULL} (default) to not highlight anything.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast group to include.
#'
#' @param with_census \code{logical} toggle if the plot should include the observed data collected during the predicted census.
#'
#' @return \code{NULL}. Plot is generated.
#'
#' @export
#'
plot_cast_point <- function (main                       = ".", 
                             cast_id                    = NULL, 
                             cast_groups                = NULL,
                             dataset                    = NULL, 
                             model                      = NULL, 
                             historic_end_newmoonnumber = NULL, 
                             species                    = forecasting_species(total = TRUE), 
                             highlight_sp               = NULL,
                             newmoonnumber              = NULL, 
                             with_census                = FALSE) {

  settings <- read_directory_settings(main = main)

  casts_meta <- select_casts(main                        = main, 
                             cast_ids                    = cast_id,
                             historic_end_newmoonnumbers = historic_end_newmoonnumber, 
                             models                      = model, 
                             datasets                    = dataset, 
                             species                     = species)

  if (with_census) {

    moons          <- read_newmoons(main = main)
    rodents_all    <- read_rodents_table(main = main, dataset = "all")

    newmoons_census  <- rodents_all$newmoonnumber[!is.na(rodents_all$total)]

    newmoonnumber  <- ifnull(newmoonnumber, max(newmoons_census))

    casts_last_census <- newmoonnumber >= casts_meta$forecast_start_newmoonnumber & newmoonnumber <= casts_meta$forecast_end_newmoonnumber

    casts_meta        <- casts_meta[casts_last_census, ]

  }

  if (NROW(casts_meta) == 0) {

    stop("no casts available for requested plot")

  }

  if (NROW(casts_meta) > 1) {

    which_max  <- which.max(casts_meta$cast_id)
    match_max  <- casts_meta$model      == casts_meta$model[which_max] &
                  casts_meta$dataset    == casts_meta$dataset[which_max] &
                  casts_meta$cast_group == casts_meta$cast_group[which_max]
    casts_meta <- casts_meta[match_max, ]

  }

  newmoonnumber <- ifnull(newmoonnumber, min(casts_meta$forecast_start_newmoonnumber))
  dataset       <- ifnull(dataset, unique(casts_meta$dataset))
  species       <- ifnull(species, unique(casts_meta$species))

  species  <- species[species %in% unique(casts_meta$species)]
  nspecies <- length(species)

  for (i in 1:nspecies) {
    cast_tab_i <- read_cast_tab(main     = main,
                                cast_id  = casts_meta$cast_id[i])
    cast_tab_i <- cast_tab_i[cast_tab_i$newmoonnumber == newmoonnumber, ]
    if (i == 1) {
      casts_tab <- cast_tab_i
    } else {
      casts_tab <- rbind(casts_tab, cast_tab_i)
    }
  }
  

  casts_tab <- casts_tab[order(casts_tab$estimate, decreasing = TRUE), ]

  max_obs <- NA
  if (with_census) {

    obs           <- read_rodents_table(main     = main, 
                                        dataset  = gsub("dm_", "", gsub("_interp", "", dataset)))
    colnames(obs) <- gsub("\\.", "", colnames(obs))

    newmoonnumber <- ifnull(newmoonnumber, unique(obs$newmoonnumber))
    obs           <- obs[obs$newmoonnumber %in% newmoonnumber, species, drop = FALSE]

    if (NROW(obs) == 0) {

      stop("no observations available for requested plot") 

    } 

    max_obs <- max(as.numeric(obs), na.rm = TRUE)

  } 

  rangex  <- c(0, max(c(casts_tab$upper_pi, max_obs), na.rm = TRUE))
  rangey  <- c(nspecies + 0.25, 0.75)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(3.5, 9.5, 2, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", yaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)

  mtext("Abundance", side = 1, cex = 1.5, line = 2.5)
  mtext("Species", side = 2, cex = 1.5, line = 8.25)
  lpath <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath) 
  sptab <- na_conformer(sptab, "speciescode")

  for (i in 1:nspecies) {

    if (casts_tab$species[i] == "total") {

      lab_text <- "Total"
      lab_font <- 1

    } else {

      sppmatch <- which(sptab[ , "speciescode"] == casts_tab$species[i])
      lab_text <- sptab[sppmatch , "scientificname"]
      lab_font <- 3

    }

    axis(2, at = i, labels = lab_text, font = lab_font, las = 1, 
         cex.axis = 0.65, tck = 0, line = -0.5, lwd = 0)
    axis(2, at = i, labels = FALSE, las = 1, 
         cex.axis = 0.65, tck = -0.01)

  }

  for (i in 1:nspecies) {

    low   <- max(c(casts_tab$lower_pi[i], 0))
    up    <- casts_tab$upper_pi[i]
    est   <- casts_tab$estimate[i]
    vbars <- i + (0.015 * nspecies * c(-1, 1))

    if (!is.null(highlight_sp) && species[i] %in% highlight_sp) {

      col = rgb(0.2, 0.5, 0.9)
      lwd = 4

    } else {

      col = "black"
      lwd = 2

    }

    points(c(low, up), rep(i, 2), type = "l", col = col, lwd = lwd)
    points(rep(low, 2), vbars, type = "l", col = col, lwd = lwd)
    points(rep(up, 2), vbars, type = "l", col = col, lwd = lwd)
    points(est, i, pch = 16, col = "white", cex = 1.25)
    points(est, i, lwd = lwd, col = col, cex = 1.25)

   if (with_census) {

      spmatch       <- casts_tab$species[i]
      nasp          <- spmatch == "NA"
      spmatch[nasp] <- "NA."
      obsi          <- obs[ , spmatch]
      points(obsi, i, pch = 15, col = rgb(0, 0.4, 0.9, 0.8), cex = 1.25)
      points(obsi, i, pch = 0, col = rgb(0.2, 0.2, 0.2, 0.8), cex = 1.25)

    }   

  }

  invisible( )

}



#' @title Visualize a Time Series Cast of a Species
#'
#' @description Plot an observed timeseries and cast timeseries with a prediction interval. \cr
#'  Casts can be selected either by supplying a \code{cast_id} number or any combination of \code{dataset}, \code{model}, and \code{historic_end_newmoonnumber}, which filter the available casts in unison. This plot type can only handle output from a single cast, so if multiple casts still remain, the one with the highest number is selected. To be more certain about cast selection, use the \code{cast_id} input.
#'  Casts can be selected either by supplying a \code{cast_id} number or any combination of \code{dataset}, \code{model}, and \code{historic_end_newmoonnumber}, which filter the available casts in unison. This plot type can only handle output from a single cast, so if multiple casts still remain, the one with the highest number is selected. To be more certain about cast selection, use the \code{cast_id} input.
#'
#' @details The resulting plot shows observations as a solid black line and predictions as a blue polygon with the bounds represent the error given by \code{lower_pi} and \code{upper_pi} and the bisecting blue line representing the \code{estimate} in the \code{cast_table} saved output from a model. \cr
#'  As of \code{portalcasting v0.9.0}, this represents the mean and the 95\% prediction interval. \cr
#'  Observations that occurred after the cast are shown connected directly to the pre-cast observation data (as the black solid line).
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param historic_end_newmoonnumber \code{integer} (or integer \code{numeric}) newmoon number of the forecast origin. Default value is \code{NULL}, which equates to no selection.
#'
#' @param model \code{character} value of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to model. 
#'
#' @param dataset \code{character} value of the rodent data set to include. Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param cast_id \code{integer} (or integer \code{numeric}) value representing the cast of interest, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param species \code{character} value of the species codes (or \code{"total"} for the total across species) to be plotted. \code{NULL} (default) also gives the total.
#'
#' @param historic_start_newmoonnumber \code{integer} (or integer \code{numeric}) newmoon number for the beginning of the x-axis of the plot. \cr
#'  Does not influence the fit of the models, just the presentation. 
#'
#' @param cast_group \code{integer} (or integer \code{numeric}) value of the cast group to include. Default value is \code{NULL}, which equates to no selection with respect to \code{cast_group}.
#'
#' @return \code{NULL}. Plot is generated.
#'
#' @export
#'
plot_cast_ts <- function (main                         = ".", 
                          cast_id                      = NULL, 
                          cast_group                   = NULL,
                          dataset                      = NULL, 
                          model                        = NULL, 
                          historic_start_newmoonnumber = NULL, 
                          historic_end_newmoonnumber   = NULL, 
                          species                      = NULL) {

  settings <- read_directory_settings(main = main)

  casts_meta <- select_casts(main                        = main, 
                             cast_ids                    = cast_id,
                             historic_end_newmoonnumbers = historic_end_newmoonnumber, 
                             models                      = model, 
                             datasets                    = dataset, 
                             species                     = species)

  if (NROW(casts_meta) > 1) {

    which_max  <- which.max(casts_meta$cast_id)
    casts_meta <- casts_meta[which_max, ]

  }

  if (NROW(casts_meta) == 0) {

    stop("no casts available for requested plot")

  }

  dataset <- casts_meta$dataset
  species <- casts_meta$species
  model   <- casts_meta$moon
  cast_id <- casts_meta$cast_id

  historic_start_newmoonnumber <- casts_meta$historic_start_newmoonnumber
  historic_end_newmoonnumber   <- casts_meta$historic_end_newmoonnumber

  obs     <- read_rodents_table(main     = main, 
                                dataset  = dataset)


  preds <- read_cast_tab(main     = main, 
                         cast_id  = cast_id)



  match_sp  <- (preds$species %in% species)

  colnames  <- c("newmoonnumber", "estimate", "lower_pi", "upper_pi")
  match_col <- (colnames(preds) %in% colnames)
  preds     <- preds[match_sp, match_col]

  max_moon <- max(preds$newmoonnumber)
  rangex   <- c(historic_start_newmoonnumber, max_moon)
  obs_sp   <- obs[ , species]
  obs_nm   <- obs[ , "newmoonnumber"]
  maxy     <- max(c(preds$upper_pi, obs_sp), na.rm = TRUE)
  rangey   <- c(0, maxy)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(3, 4.5, 2, 1))
  plot(x    = 1, 
       y    = 1, 
       type = "n", 
       bty  = "L", 
       xlab = "", 
       ylab = "", 
       xaxt = "n", 
       las  = 1, 
       xlim = rangex, 
       ylim = rangey)

  moons <- read_newmoons(main     = main)
  minx     <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[1]])
  maxx     <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[2]])
  minx_yr  <- as.numeric(format(as.Date(minx), "%Y"))
  maxx_yr  <- as.numeric(format(as.Date(maxx), "%Y"))
  minx_yr2 <- ceiling(minx_yr/ 5) * 5
  maxx_yr2 <- floor(maxx_yr/ 5) * 5
  yrs      <- seq(minx_yr2, maxx_yr2, 5)
  txt      <- yrs
  nyd      <- paste0(yrs, "-01-01")
  dx       <- as.Date(as.character(moons$newmoondate))
  dy       <- moons$newmoonnumber 
  dmod     <- lm(dy ~ dx)
  loc      <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(side   = 1, 
       at     = loc, 
       labels = txt)
  yrs      <- seq(minx_yr, maxx_yr, 1)
  nyd      <- paste0(yrs, "-01-01")
  loc      <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(side   = 1, 
       at     = loc, 
       labels = FALSE, 
       tck    = -0.005)  

  lab   <- list(text = "", font = 1)
  lp    <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp)
  sptab <- na_conformer(sptab, "speciescode")

  if (species == "total") {

    lab$text <- "Total Abundance"
    lab$font <- 1

  } else {

    sppmatch <- which(sptab[ , "speciescode"] == species)
    lab$text <- sptab[sppmatch , "scientificname"]
    lab$font <- 3

  }

  mtext(lab$text, side = 2, font = lab$font, cex = 1.5, line = 3)

  o_x   <- obs_nm
  o_y   <- obs_sp
  p_y_m <- preds[ , "estimate"]
  p_y_l <- preds[ , "lower_pi"]
  p_y_u <- preds[ , "upper_pi"]
  p_x   <- preds[ , "newmoonnumber"]

  for (i in 1:length(p_y_l)) {

    p_y_l[i] <- max(c(0, p_y_l[i]))

  }

  first_pred <- min(p_x)
  last_o_x   <- max(o_x[!is.na(o_y) & o_x < first_pred])
  last_o_y   <- o_y[o_x == last_o_x]

  poly_x <- c(last_o_x, p_x, p_x[length(p_x):1], last_o_x)
  poly_y <- c(last_o_y, p_y_l, p_y_u[length(p_y_u):1], last_o_y)

  polygon(x      = poly_x, 
          y      = poly_y, 
          col    = rgb(0.6757, 0.8438, 0.8984), 
          border = NA)
  points(x    = c(last_o_x, p_x), 
         y    = c(last_o_y, p_y_m), 
         type = "l", 
         lwd  = 2, 
         lty  = 1,
         col  = rgb(0.2, 0.5, 0.9))

  o_x_1 <- o_x[!is.na(o_y) & o_x < first_pred]
  o_y_1 <- o_y[!is.na(o_y) & o_x < first_pred]
  n_o_1 <- length(o_x_1)
  o_x_2 <- c(o_x_1[n_o_1], o_x[!is.na(o_y) & o_x >= first_pred])
  o_y_2 <- c(o_y_1[n_o_1], o_y[!is.na(o_y) & o_x >= first_pred])

  points(o_x_1, o_y_1, type = "l", lwd = 2)
  points(o_x_2, o_y_2, type = "l", lwd = 2)

  actual_obs <- !is.na(o_y)
  points(o_x[actual_obs], o_y[actual_obs], pch = 16, col = "white")
  points(o_x[actual_obs], o_y[actual_obs], pch = 1, col = 1, lwd = 2)

}