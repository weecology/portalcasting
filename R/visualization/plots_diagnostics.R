#' @title Visualize Portalcasting Data and Forecasts
#'
#' @description `plot_forecasts_error_lead`: lots the raw error (estimate - observation) as a function of lead time across model runs from different forecast origins for multiple models and multiple species (or total) within a data set. \cr
#'              `plot_covariates`: plots an observed timeseries and forecast timeseries of the covariates used. \cr
#'              `plot_forecast_ts`: plots an observed timeseries and forecast timeseries with a prediction interval. Observations that occurred after the forecast are shown connected directly to the pre-cast observation data (as the black solid line with points).\cr
#'              `plot_forecast_point`: plots the point value with confidence interval for a time point across multiple species. Casts can be selected either by supplying a `forecast_id` number or any combination of `dataset`, `model`, and `historic_end_newmoonnumber`, which filter the available forecasts in unison. This plot type can only handle output from a single forecast, so if multiple forecasts still remain, the one with the highest number is selected. To be more certain about forecast selection, use the `forecast_id` input.  \cr
#'              `plot_forecasts_cov_RMSE`: plots the coverage (fraction of predictions within the CI) and RMSE (root mean squared error) of each model among multiple species.
#'
#' @details Casts can be selected either by supplying a `forecast_id` number or any combination of `dataset`, `model`, and `historic_end_newmoonnumber`, which filter the available forecasts in unison. This plot type can only handle output from a single forecast, so if multiple forecasts still remain, the one with the highest number is selected. To be more certain about forecast selection, use the `forecast_id` input. \cr 
#'          As of `portalcasting v0.9.0`, the line and bands in `plot_forecast_ts` and point and bars in `plot_forecast_point` represent the mean and the 95 percent prediction interval. \cr
#'
#' @param forecast_id,forecasts_ids `integer` (or integer `numeric`) values representing the forecasts of interest for restricting plotting, as indexed within the directory in the `casts` sub folder. See the forecasts metadata file (`forecasts_metadata.csv`) for summary information. `forecast_id` can only be length-1 or `NULL`, whereas `forecasts_ids` is not length-restricted.
#'
#' @param forecasts_evaluations `data.frame` of forecast evaluations, as returned from [`evaluate_forecasts`]. If `NULL` (default), will try to read via [`read_forecasts_evaluations`].
#'
#' @param forecasts_metadata `data.frame` of forecast metadata. If `NULL` (default), will try to read via [`read_forecasts_metadata`].
#'
#' @param historic_start_newmoonnumber `integer` (or integer `numeric`) newmoon number for the beginning of the x-axis of the plot. \cr
#'        Does not influence the fit of the models, just the presentation. 
#'
#' @param historic_end_newmoonnumber,historic_end_newmoonnumbers `integer` (or integer `numeric`) newmoon number(s) of the forecast origin. Default value is `NULL`, which equates to no selection. `historic_end_newmoonnumber` can only be length-1 or `NULL`, whereas `historic_end_newmoonnumbers` is not length-restricted.
#'
#' @param model,models `character` value(s) of the name of the model to include. Default value is `NULL`, which equates to no selection with respect to `model` or `models`. `model` can only be length-1 or `NULL`, whereas `models` is not length-restricted.
#'
#' @param dataset,datasets `character` value of the rodent data set(s) to include. `dataset` can only be length-1 or `NULL`, whereas `datasets` is not length-restricted.
#'
#' @param newmoonnumber `integer` (or integer `numeric`) newmoon number for the plot. 
#'
#' @param species `character` vector of the species code(s) or `"total"` for the total across species) to be plotted `NULL` translates to the species defined by [`forecasting_species`][`portalr::forecasting_species`] called by [`prefab_species`].
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param to_plot `character` of the covariate to plot, restricted to column names in the covariates table (see [`read_covariates`]).
#' 
#' @param highlight_sp `character` vector of the species codes (or `"total"` for the total across species) to be highlighted or `NULL` (default) to not highlight anything.
#'
#' @param with_census `logical` toggle if the plot should include the observed data collected during the predicted census.
#'
#' @return `NULL`. Plot is generated.
#'
#' @name plots
#'
#' @aliases figures
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "figures")
#'    setup_production(main = main1)
#'
#'    plot_covariates(main = main1)
#'
#'    portalcast(main = main1, models = "AutoArima")
#'
#'    ids <- select_forecasts(main     = main3, 
#'                            species  = c("DM", "PP", "total"),
#'                            models   = c("AutoArima", "ESSS", "pevGARCH", "nbGARCH", "jags_RW"),
#'                            datasets = c("all", "controls"))$forecast_id
#'    nids         <- length(ids)
#'    nsample_ids  <- 1000
#'    forecasts_ids <- ids[round(seq(1, nids, length.out = nsample_ids))]
#'    evaluate_forecasts(main         = main3, 
#'                       forecasts_ids = forecasts_ids) 
#'
#'    plot_forecast_ts(main = main1)
#'    plot_forecast_point(main = main1)
#'    plot_forecasts_error_lead(main = main1)
#'    plot_forecasts_cov_RMSE(main    = main1, 
#'                            models  = "AutoArima", 
#'                            species = "DM")
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL


#' @rdname plots
#'
#' @export
#'
plot_forecasts_error_lead <- function (main                        = ".", 
                                       forecasts_ids               = NULL, 
                                       forecasts_evaluations       = NULL, 
                                       historic_end_newmoonnumbers = NULL, 
                                       models                      = NULL, 
                                       datasets                    = NULL, 
                                       species                     = NULL) {

  settings <- read_directory_settings(main = main)

  evals <- ifnull(forecasts_evaluations, read_forecasts_evaluations(main))
  fm <- read_forecasts_metadata(main = main)

  evals$historic_end_newmoonnumber <- fm$historic_end_newmoonnumber[match(evals$forecast_id, fm$forecast_id)]
  evals$model                      <- fm$model[match(evals$forecast_id, fm$forecast_id)]
  evals$dataset                    <- fm$dataset[match(evals$forecast_id, fm$forecast_id)]
  evals$species                    <- fm$species[match(evals$forecast_id, fm$forecast_id)]
  evals$lead_time_newmoons         <- evals$newmoonnumber - evals$historic_end_newmoonnumber
  

  models                        <- ifnull(models, "AutoArima")
  datasets                      <- ifnull(datasets, "controls")
  species                       <- ifnull(species, "DM") 
  forecasts_ids                  <- ifnull(forecasts_ids, unique(evals$forecast_id))
  historic_end_newmoonnumbers   <- ifnull(historic_end_newmoonnumbers, unique(evals$historic_end_newmoonnumber)) 

  forecast_id_in                <- evals$forecast_id %in% forecasts_ids
  model_in                      <- evals$model %in% models
  dataset_in                    <- evals$dataset == datasets
  species_in                    <- evals$species %in% species
  historic_end_newmoonnumber_in <- evals$historic_end_newmoonnumber %in% historic_end_newmoonnumbers
  all_in                        <- forecast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no evaluations available for requested plot")

  }

  evals_in <- evals[all_in, ]

  nmodels <- length(models) 
  nspecies <- length(species)

  species_names_table  <- rodent_species(path = resources_path(main = main), set = "forecasting", type = "table", total = TRUE)

  if (nmodels == 1 & nspecies == 1) {

    yy <- round(evals_in$error, 3)
    yrange <- range(c(-1, yy, 1), na.rm = TRUE)
    xrange <- c(max(evals_in$lead_time_newmoons) + 0.25, 0)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(bty = "L", mar = c(4, 4.5, 3, 1))

    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         ylim = yrange, xlim = xrange)
    abline(h = 0, lty = 3, lwd = 2, col = grey(0.6))
    uforecast_ids <- unique(evals_in$forecast_id)
    nforecast_ids <- length(uforecast_ids)
    cols <- viridis(nforecast_ids, 0.8, 0, 0.75)
    for(k in 1:nforecast_ids){
      matches <- which(evals_in$forecast_id == uforecast_ids[k])
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
      yrange     <- range(c(-1, yy, 1), na.rm = TRUE)

      for(i in 1:nmodels){

        forecast_id_in <- evals_in$forecast_id %in% forecasts_ids


        dataset_in                     <- evals_in$dataset %in% datasets
        model_in                       <- evals_in$model %in% models[i]
        species_in                     <- evals_in$species %in% species[j]
        historic_end_newmoonnumbers_in <- evals_in$historic_end_newmoonnumber %in% historic_end_newmoonnumbers

        all_in <- forecast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumbers_in

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

        uforecast_ids <- unique(pevals_in$forecast_id)
        nforecast_ids <- length(uforecast_ids)

        cols <- viridis(n     = nforecast_ids, 
                        alpha = 0.8, 
                        begin = 0, 
                        end   = 0.75)

        for (k in 1:nforecast_ids) {

          matches <- which(pevals_in$forecast_id == uforecast_ids[k])

          x <- pevals_in$lead_time_newmoons[matches]
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
        xat1 <- seq(0, max(pevals_in$lead_time_newmoons), 2)
        xat2 <- seq(0, max(pevals_in$lead_time_newmoons), 1)
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

          lab   <- list(text = species_names_table$g_species[species_names_table$code == species[j]], 
                        font = ifelse(species == "total", 2, 4))

          text(0.75, 1, lab$text, font = lab$font, cex = 0.55, xpd = TRUE, srt = 270)

        }

      }

    }

  }  

  invisible( )

}


#' @rdname plots
#'
#' @export
#'
plot_forecasts_cov_RMSE <- function (main                        = ".", 
                                     forecasts_metadata          = NULL, 
                                     forecasts_ids               = NULL, 
                                     forecasts_evaluations       = NULL, 
                                     historic_end_newmoonnumbers = NULL, 
                                     models                      = NULL, 
                                     datasets                    = NULL, 
                                     species                     = NULL) {

  settings <- read_directory_settings(main = main)

  evals <- ifnull(forecasts_evaluations, read_forecasts_evaluations(main))

  forecasts_meta <- select_forecasts(main                        = main, 
                                     forecasts_metadata          = forecasts_metadata,
                                     forecasts_ids               = forecasts_ids,
                                     historic_end_newmoonnumbers = historic_end_newmoonnumbers, 
                                     models                      = models, 
                                     datasets                    = datasets, 
                                     species                     = species)

  evals$historic_end_newmoonnumber <- forecasts_meta$historic_end_newmoonnumber[match(evals$forecast_id, forecasts_meta$forecast_id)]
  evals$model                      <- forecasts_meta$model[match(evals$forecast_id, forecasts_meta$forecast_id)]
  evals$dataset                    <- forecasts_meta$dataset[match(evals$forecast_id, forecasts_meta$forecast_id)]
  evals$species                    <- forecasts_meta$species[match(evals$forecast_id, forecasts_meta$forecast_id)]


  datasets                      <- unique(forecasts_meta$dataset)[unique(forecasts_meta$dataset) %in% unique(evals$dataset)]
  species                       <- unique(forecasts_meta$species)[unique(forecasts_meta$species) %in% unique(evals$species)]
  models                        <- unique(forecasts_meta$model)[unique(forecasts_meta$model) %in% unique(evals$model)]
  forecasts_ids                  <- unique(forecasts_meta$forecast_id)[unique(forecasts_meta$forecast_id) %in% unique(evals$forecast_id)]
  historic_end_newmoonnumbers   <- unique(forecasts_meta$historic_end_newmoonnumber)[unique(forecasts_meta$historic_end_newmoonnumber) %in% unique(evals$historic_end_newmoonnumber)]

  forecast_id_in                <- evals$forecast_id %in% forecasts_ids
  model_in                      <- evals$model %in% models
  dataset_in                    <- evals$dataset %in% datasets
  species_in                    <- evals$species %in% species
  historic_end_newmoonnumber_in <- evals$historic_end_newmoonnumber %in% historic_end_newmoonnumbers
  all_in                        <- forecast_id_in & model_in & dataset_in & species_in & historic_end_newmoonnumber_in

  if (sum(all_in) == 0) {

    stop("no evaluations available for requested plot")

  }

  evals_in <- evals[all_in, ]

  nmodels <- length(models) 
  nspecies <- length(species)

  ucasts <- unique(evals_in$forecast_id)
  ncasts <- length(ucasts)

  forecast_RMSE     <- numeric(ncasts)
  forecast_coverage <- numeric(ncasts)
  forecast_model    <- character(ncasts)
  forecast_species  <- character(ncasts)

  for (i in 1:ncasts) {

    forecast_RMSE[i]     <- sqrt(mean(evals_in$error[evals_in$forecast_id == ucasts[i]] ^ 2, na.rm = TRUE))
    forecast_coverage[i] <- mean(evals_in$covered[evals_in$forecast_id == ucasts[i]], na.rm = TRUE)
    forecast_model[i]    <- evals_in$model[evals_in$forecast_id == ucasts[i]][1]
    forecast_species[i]  <- evals_in$species[evals_in$forecast_id == ucasts[i]][1]

  }

  species_names_table  <- rodent_species(path = file.path(main, settings$subdirectories$resources), set = "forecasting", type = "table", total = TRUE)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n")

  for (i in 1:nspecies){

    x1 <- 0
    x2 <- 0.5
    y1 <- 0 + (i - 1) * 1 * (1/nspecies)
    y2 <- y1 + 1 * (1/nspecies)
    par(mar = c(2, 3.5, 0.5, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", 
         xlab = "", xlim = c(0.5, nmodels + 0.5), ylim = c(0,1), bty = "L")
    axis(2, at = seq(0, 1, 0.2), cex.axis = 1.125, las = 1, line = -0.5, 
         lwd = 0)
    axis(2, at = seq(0, 1, 0.2), labels = FALSE, tck = -0.0125)
    axis(2, at = seq(-0.5, 1.5, 0.1), labels = FALSE, tck = 0)
    mtext(side = 2, "Coverage", line = 2.25, cex = 1.5)
    if (nmodels > 1) {
      axis(1, at = 1:nmodels, labels = FALSE, tck = -0.025)
    }
    for(j in 1:nmodels){
      in_ij <- which(forecast_species == species[i] & 
                     forecast_model == models[j])

      ys <- na.omit(forecast_coverage[in_ij])
      ys2 <- runif(length(ys), ys - 0.005, ys + 0.005)
      xs <- runif(length(ys), j - 0.05, j + 0.05)
      quants <- quantile(ys, seq(0, 1, 0.25))
      points(rep(j, 2), quants[c(1, 5)], type = "l", lwd = 2)
      points(c(j - 0.02, j + 0.02), rep(quants[1], 2), type = "l", lwd = 2)
      points(c(j - 0.02, j + 0.02), rep(quants[5], 2), type = "l", lwd = 2)
      rect(j - 0.1, quants[2], j + 0.1, quants[4], col = "white", lwd = 2)
      points(c(j - 0.1, j + 0.1), rep(quants[3], 2), type = "l", lwd = 3)
      points(xs, ys2, col = grey(0.2, 0.4), pch = 1, cex = 1)

      if (nmodels > 1) {
        axis(1, at = 1:nmodels, labels = models, cex.axis = 1.125, xpd = TRUE)
      }
    }
    
    abline(h = 0.95, lwd = 2, lty = 3)

    in_i <- which(forecast_species == species[i])
    ymax <- max(c(0, forecast_RMSE[in_i]), na.rm = TRUE)
    x1 <- 0.5
    x2 <- 1
    y1 <- 0 + (i - 1) * 1 * (1/nspecies)
    y2 <- y1 + 1 * (1/nspecies)
    par(mar = c(2, 5, 0.5, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)

    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", bty = "L",
         xlab = "", xlim = c(0.5, nmodels + 0.5), ylim = c(0, ymax))
    axis(2, cex.axis = 1.125, las = 1, line = -0.5, lwd = 0)
    axis(2, labels = FALSE, tck = -0.0125)
    mtext(side = 2, "RMSE", line = 3, cex = 1.5)
    if (nmodels > 1) {
      axis(1, at = 1:nmodels, labels = FALSE, tck = -0.025)
    }
    for(j in 1:nmodels){
      in_ij <- which(forecast_species == species[i] & 
                     forecast_model == models[j])

      ys <- na.omit(forecast_RMSE[in_ij])
      ys2 <- runif(length(ys), ys - 0.005, ys + 0.005)
      xs <- runif(length(ys), j - 0.05, j + 0.05)
      quants <- quantile(ys, seq(0, 1, 0.25))
      points(rep(j, 2), quants[c(1, 5)], type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[1], 2), type = "l")
      points(c(j - 0.02, j + 0.02), rep(quants[5], 2), type = "l")
      rect(j - 0.1, quants[2], j + 0.1, quants[4], col = "white")
      points(c(j - 0.1, j + 0.1), rep(quants[3], 2), type = "l", lwd = 2)
      points(xs, ys2, col = rgb(0.3, 0.3, 0.3, 0.4), pch = 1, cex = 0.5)
      axis(2, labels = FALSE, tck = 0, at = seq(min(c(0, ys)) - 2, max(c(1,ys)) + 2, 0.1))

      if (nmodels > 1) {
        axis(1, at = 1:nmodels, labels = models, cex.axis = 1.125, xpd = TRUE)
      }

    }
    if (nspecies > 1) {
      par(mar = c(0, 0, 0, 0), fig = c(0.95, 1, y1, y2), new = TRUE)   
      plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",
           bty = "n") 

      lab   <- list(text = species_names_table$g_species[species_names_table$code == species[j]], 
                    font = ifelse(species == "total", 1, 2))

      text(0.9, 1, lab$text, font = lab$font, cex = 1.5, xpd = TRUE, srt = 270)
    }
  }


  invisible( )

}
