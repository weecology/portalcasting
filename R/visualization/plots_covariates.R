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
