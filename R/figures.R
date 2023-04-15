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
#' @param forecast_group `integer` (or integer `numeric`) value of the forecast group to include. Default value is `NULL`, which equates to no selection with respect to `forecast_group`.
#'
#' @param forecast_id,forecast_ids `integer` (or integer `numeric`) values representing the forecasts of interest for restricting plotting, as indexed within the directory in the `casts` sub folder. See the forecasts metadata file (`forecasts_metadata.csv`) for summary information. `forecast_id` can only be length-1 or `NULL`, whereas `forecast_ids` is not length-restricted.
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
#' @param species `character` vector of the species code(s) or `"total"` for the total across species) to be plotted `NULL` translates to the species defined by [`portalr::forecasting_species`].
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
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "figures")
#'    setup_production(main = main1)
#'
#'    plot_covariates(main = main1)
#'
#'    portalcast(main = main1, models = "AutoArima")
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
                                       forecast_ids                = NULL, 
                                       historic_end_newmoonnumbers = NULL, 
                                       models                      = NULL, 
                                       datasets                    = NULL, 
                                       species                     = NULL) {

  settings <- read_directory_settings(main = main)

  evals    <- read_forecasts_evaluations(main)

  models                        <- ifnull(models, "AutoArima")
  datasets                      <- ifnull(datasets, "controls")
  species                       <- ifnull(species, "DM") 
  forecast_ids                  <- ifnull(forecast_ids, unique(evals$forecast_id))
  historic_end_newmoonnumbers   <- ifnull(historic_end_newmoonnumbers, unique(evals$historic_end_newmoonnumber)) 

  forecast_id_in                <- evals$forecast_id %in% forecast_ids
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
    yrange <- range(c(0, yy), na.rm = TRUE)
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
      yrange     <- range(c(0, yy), na.rm = TRUE)

      for(i in 1:nmodels){

        forecast_id_in <- evals_in$forecast_id %in% forecast_ids


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
                                     forecast_ids                = NULL, 
                                     historic_end_newmoonnumbers = NULL, 
                                     models                      = NULL, 
                                     datasets                    = NULL, 
                                     species                     = NULL) {

  settings <- read_directory_settings(main = main)

  evals    <- read_forecasts_evaluations(main = main)

  forecasts_meta <- select_forecasts(main                        = main, 
                                     forecast_ids                = forecast_ids,
                                     historic_end_newmoonnumbers = historic_end_newmoonnumbers, 
                                     models                      = models, 
                                     datasets                    = datasets, 
                                     species                     = species)


  datasets                      <- unique(forecasts_meta$dataset)[unique(forecasts_meta$dataset) %in% unique(evals$dataset)]
  species                       <- unique(forecasts_meta$species)[unique(forecasts_meta$species) %in% unique(evals$species)]
  models                        <- unique(forecasts_meta$model)[unique(forecasts_meta$model) %in% unique(evals$model)]
  forecast_ids                  <- unique(forecasts_meta$forecast_id)[unique(forecasts_meta$forecast_id) %in% unique(evals$forecast_id)]
  historic_end_newmoonnumbers   <- unique(forecasts_meta$historic_end_newmoonnumber)[unique(forecasts_meta$historic_end_newmoonnumber) %in% unique(evals$historic_end_newmoonnumber)]

  forecast_id_in                <- evals$forecast_id %in% forecast_ids
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
    ymax <- max(forecast_RMSE[in_i], na.rm = TRUE)
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
      axis(2, labels = FALSE, tck = 0, at = seq(min(ys) - 2, max(ys) + 2, 0.1))

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

#' @rdname plots
#'
#' @export
#'
plot_forecast_point <- function (main                       = ".", 
                                 forecast_id                = NULL, 
                                 dataset                    = NULL, 
                                 model                      = NULL, 
                                 historic_end_newmoonnumber = NULL, 
                                 species                    = NULL, 
                                 highlight_sp               = NULL,
                                 newmoonnumber              = NULL, 
                                 with_census                = FALSE) {

  settings <- read_directory_settings(main = main)

  forecasts_meta <- select_forecasts(main                        = main, 
                                     forecast_ids                = forecast_id,
                                     historic_end_newmoonnumbers = historic_end_newmoonnumber, 
                                     models                      = model, 
                                     datasets                    = dataset, 
                                     species                     = species)


  if (with_census) {

    moons          <- read_newmoons(main = main)
    rodents_all    <- read_rodents_dataset(main = main, dataset = "all")

    newmoons_census  <- rodents_all$newmoonnumber[!is.na(rodents_all$total)]

    newmoonnumber    <- ifnull(newmoonnumber, max(newmoons_census))

    forecasts_last_census <- newmoonnumber >= forecasts_meta$forecast_start_newmoonnumber & newmoonnumber <= forecasts_meta$forecast_end_newmoonnumber

    forecasts_meta        <- forecasts_meta[forecasts_last_census, ]

  }

  if (NROW(forecasts_meta) == 0) {

    stop("no forecasts available for requested plot")

  }

  if (NROW(forecasts_meta) > 1) {

    which_max  <- which.max(forecasts_meta$forecast_id)
    match_max  <- forecasts_meta$model      == forecasts_meta$model[which_max] &
                  forecasts_meta$dataset    == forecasts_meta$dataset[which_max] &
                  forecasts_meta$forecast_group == forecasts_meta$forecast_group[which_max]
    forecasts_meta <- forecasts_meta[match_max, ]

  }

  newmoonnumber <- ifnull(newmoonnumber, min(forecasts_meta$forecast_start_newmoonnumber))
  dataset       <- ifnull(dataset, unique(forecasts_meta$dataset))
  species       <- ifnull(species, unique(forecasts_meta$species))

  species  <- species[species %in% unique(forecasts_meta$species)]
  nspecies <- length(species)

  for (i in 1:nspecies) {
    forecast_tab_i <- read_forecast_table(main     = main,
                                          forecast_id  = forecasts_meta$forecast_id[i])
    forecast_tab_i <- forecast_tab_i[forecast_tab_i$newmoonnumber == newmoonnumber, ]
    if (i == 1) {
      forecasts_table <- forecast_tab_i
    } else {
      forecasts_table <- rbind(forecasts_table, forecast_tab_i)
    }
  }
  

  forecasts_table <- forecasts_table[order(forecasts_table$estimate, decreasing = TRUE), ]

  max_obs <- NA
  if (with_census) {

    obs           <- read_rodents_dataset(main     = main, 
                                        dataset  = gsub("dm_", "", gsub("_interp", "", dataset)))

    newmoonnumber <- ifnull(newmoonnumber, unique(obs$newmoonnumber))
    obs           <- obs[obs$newmoonnumber %in% newmoonnumber, species, drop = FALSE]

    if (NROW(obs) == 0) {

      stop("no observations available for requested plot") 

    } 

    max_obs <- max(as.numeric(obs), na.rm = TRUE)

  } 

  rangex  <- c(0, max(c(forecasts_table$upper_pi, max_obs), na.rm = TRUE))
  rangey  <- c(nspecies + 0.25, 0.75)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(3.5, 8, 0, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", yaxt= "n", xaxt = "n", 
       las = 1, xlim = rangex, ylim = rangey)

  mtext("Abundance", side = 1, cex = 1.75, line = 2.5)
  species_names_table  <- rodent_species(path = file.path(main, settings$subdirectories$resources), set = "forecasting", type = "table", total = TRUE)


  for (i in 1:nspecies) {

    lab   <- list(text = species_names_table$g_species[species_names_table$code == forecasts_table$species[i]], 
                  font = ifelse(species == "total", 1, 3))

    axis(2, at = i, labels = lab$text, font = lab$font, las = 1, 
         cex.axis = 1.125, tck = 0, line = -0.5, lwd = 0)
    axis(2, at = i, labels = FALSE, las = 1, 
         cex.axis = 0.65, tck = -0.01)

  }
  axis(1, cex.axis = 1.25)
  axis(1, labels = FALSE, at = seq(-1 * rangex[2], rangex[2] * 2, 1), tck = 0)
  for (i in 1:nspecies) {

    low   <- max(c(forecasts_table$lower_pi[i], 0))
    up    <- forecasts_table$upper_pi[i]
    est   <- forecasts_table$estimate[i]
    vbars <- i + (0.015 * nspecies * c(-1, 1))

    if (!is.null(highlight_sp) && forecasts_table$species[i] %in% highlight_sp) {

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

      spmatch       <- forecasts_table$species[i]
      obsi          <- obs[ , spmatch]
      points(obsi, i, pch = 15, col = rgb(0, 0.4, 0.9, 0.8), cex = 1.25)
      points(obsi, i, pch = 0, col = rgb(0.2, 0.2, 0.2, 0.8), cex = 1.25)

    }   

  }

  invisible( )

}

#' @rdname plots
#'
#' @export
#'
plot_forecast_ts <- function (main                         = ".", 
                              forecast_id                  = NULL, 
                              forecast_group               = NULL,
                              dataset                      = NULL, 
                              model                        = NULL, 
                              historic_start_newmoonnumber = NULL, 
                              historic_end_newmoonnumber   = NULL, 
                              species                      = NULL) {

  settings <- read_directory_settings(main = main)

  forecasts_meta <- select_forecasts(main                        = main, 
                                     forecast_ids                = forecast_id,
                                     historic_end_newmoonnumbers = historic_end_newmoonnumber, 
                                     models                      = model, 
                                     datasets                    = dataset, 
                                     species                     = species)

  if (NROW(forecasts_meta) > 1) {

    which_max  <- which.max(forecasts_meta$forecast_id)
    forecasts_meta <- forecasts_meta[which_max, ]

  }

  if (NROW(forecasts_meta) == 0) {

    stop("no forecasts available for requested plot")

  }

  dataset     <- forecasts_meta$dataset
  species     <- forecasts_meta$species
  model       <- forecasts_meta$model
  forecast_id <- forecasts_meta$forecast_id

  historic_start_newmoonnumber <- forecasts_meta$historic_start_newmoonnumber
  historic_end_newmoonnumber   <- forecasts_meta$historic_end_newmoonnumber

  obs     <- read_rodents_dataset(main     = main, 
                                  dataset  = dataset)


  preds <- read_forecast_table(main        = main, 
                               forecast_id = forecast_id)



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
  par(mar = c(2.5, 5.25, 2, 1))
  plot(x    = 1, 
       y    = 1, 
       type = "n", 
       bty  = "L", 
       xlab = "", 
       ylab = "", 
       xaxt = "n",
       yaxt = "n", 
       xlim = rangex, 
       ylim = rangey)

  moons    <- read_newmoons(main     = main)
  minx     <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[1]])
  maxx     <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[2]])
  minx_yr  <- as.numeric(format(as.Date(minx), "%Y")) - 10 
  maxx_yr  <- as.numeric(format(as.Date(maxx), "%Y")) + 10
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
       labels = txt, 
       cex.axis    = 1.5)
  yrs      <- seq(minx_yr, maxx_yr, 1)
  nyd      <- paste0(yrs, "-01-01")
  loc      <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(side   = 1, 
       at     = loc, 
       labels = FALSE, 
       tck    = -0.005)  
  axis(side   = 2, 
       cex.axis    = 1.5, las = 1)


  species_names_table  <- rodent_species(path = file.path(main, settings$subdirectories$resources), set = "forecasting", type = "table", total = TRUE)

  lab   <- list(text = species_names_table$g_species[species_names_table$code == species], 
                font = ifelse(species == "total", 1, 3))

  mtext(lab$text, side = 2, font = lab$font, cex = 2, line = 3.5)

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


#' @rdname plots
#'
#' @export
#'
plot_covariates <- function (main    = ".",
                             to_plot = "ndvi") {

  settings <- read_directory_settings(main = main)

  covariates <- read_covariates(main = main)
  moons      <- read_newmoons(main = main)

  print_names <- c(ndvi          = "NDVI", 
                   mintemp       = "Min. Temperature",  
                   meantemp      = "Mean Temperature",  
                   maxtemp       = "Max. Temperature",  
                   precipitation = "Precipitation",  
                   warm_precip   = "Warm Precip.",
                   ordii         = "D. ordii",
                   cos2pifoy     = "cos Fourier",
                   sin2pifoy     = "sin Fourier")

  if (any((!to_plot %in% names(print_names)))) {

    not_in <- to_plot[!to_plot %in% names(print_names)]

    stop("requested covariates ", paste(paste0("`", not_in, "`"), collapse = ", "), " not in covariates table")

  }

  rangex   <- range(covariates$newmoonnumber)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(2.5, 5, 0.5, 1))

  nto_plot <- length(to_plot)

  in_historic <- covariates$newmoonnumber %in% moons$newmoonnumber[moons$newmoondate < settings$time$forecast_start]
  in_forecast <- covariates$newmoonnumber %in% moons$newmoonnumber[moons$newmoondate >= settings$time$forecast_start]

  for (i in 1:nto_plot) {

    yvals    <- covariates[ , to_plot[i]]
    miny     <- min(0, min(yvals))
    maxy     <- 1.1 * max(yvals)

    ploty1   <- (i - 1) / nto_plot
    ploty2   <- i / nto_plot

    par(fig = c(0, 1, ploty1, ploty2), new = ifelse(i == 1, FALSE, TRUE))

    plot(x    = 1, 
         y    = 1, 
         type = "n", 
         bty  = "L", 
         xlab = "", 
         ylab = "", 
         xaxt = "n",
         yaxt = "n", 
         xlim = rangex, 
         ylim = c(miny, maxy))

    axis(side     = 2, 
         cex.axis = 1.25, 
         las      = 1)

    mtext(text = print_names[names(print_names) == to_plot[i]], side = 2, line = 3.5, cex = 1.5)

    minx     <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[1]])
    maxx     <- as.character(moons$newmoondate[moons$newmoonnumber == rangex[2]])
    minx_yr  <- as.numeric(format(as.Date(minx), "%Y")) - 10 
    maxx_yr  <- as.numeric(format(as.Date(maxx), "%Y")) + 10
    minx_yr2 <- ceiling(minx_yr/ 5) * 5
    maxx_yr2 <- floor(maxx_yr/ 5) * 5
    yrs      <- seq(minx_yr2, maxx_yr2, 5)
    txt      <- FALSE
    if (i == 1) {
      txt    <- yrs
    }
    nyd      <- paste0(yrs, "-01-01")
    dx       <- as.Date(as.character(moons$newmoondate))
    dy       <- moons$newmoonnumber 
    dmod     <- lm(dy ~ dx)
    loc      <- predict(dmod, newdata = list(dx = as.Date(nyd)))
    axis(side   = 1, 
         at     = loc, 
         labels = txt, 
         cex.axis    = 1.5)
    yrs      <- seq(minx_yr, maxx_yr, 1)
    nyd      <- paste0(yrs, "-01-01")
    loc      <- predict(dmod, newdata = list(dx = as.Date(nyd)))
    axis(side   = 1, 
         at     = loc, 
         labels = FALSE, 
         tck    = -0.005)  

    points(x    = covariates$newmoonnumber[in_historic],
           y    = covariates[in_historic, to_plot[i]],
           type = "l",
           lwd  = 2)

    points(x    = covariates$newmoonnumber[in_forecast],
           y    = covariates[in_forecast, to_plot[i]],
           type = "l",
           lwd  = 2,
           lty  = 3)

  }



}