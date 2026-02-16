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
plot_forecast_point <- function (main                       = ".", 
                                 forecasts_metadata         = NULL, 
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
                                     forecasts_metadata          = forecasts_metadata,
                                     forecasts_ids               = forecast_id,
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
                              forecasts_metadata           = NULL, 
                              forecast_id                  = NULL, 
                              dataset                      = NULL, 
                              model                        = NULL, 
                              historic_start_newmoonnumber = NULL, 
                              historic_end_newmoonnumber   = NULL, 
                              species                      = NULL) {

  settings <- read_directory_settings(main = main)

  forecasts_meta <- select_forecasts(main                        = main, 
                                     forecasts_metadata          = forecasts_metadata,
                                     forecasts_ids               = forecast_id,
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
