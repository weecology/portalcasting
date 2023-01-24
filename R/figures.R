
#' @title Plot the Forecast Coverage and RMSE 
#'
#' @description Plot the coverage (fraction of predictions within the CI) and RMSE (root mean squared error) of each model among multiple species.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), the table will be efficiently (as defined by the inputs) loaded and trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input, otherwise, all relevant casts will be plotted. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting plotting, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param end_moons \code{integer} (or integer \code{numeric}) newmoon numbers of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param dataset \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param ensemble \code{logical} indicator of if an ensemble should be included. Presently only the unweighted average. See \code{\link{ensemble_casts}}.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{base_species}.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = "AutoArima", end_moons = 515:520)
#'   plot_casts_cov_RMSE()
#' }
#'
#' @export
#'
plot_casts_cov_RMSE <- function (main           = ".", 
                                 settings       = directory_settings(),
                                 cast_ids       = NULL, 
                                 cast_tab       = NULL, 
                                 end_moons      = NULL, 
                                 models         = NULL, 
                                 ensemble       = TRUE, 
                                 dataset        = NULL, 
                                 species        = NULL) {

  if (length(models) == 1 && models != "ensemble") {

    ensemble <- FALSE

  }

  if (is.null(cast_tab)) {

    cast_choices <- select_casts(main           = main, 
                                 settings       = settings,
                                 cast_ids       = cast_ids, 
                                 models         = models, 
                                 end_moons      = end_moons, 
                                 datasets       = dataset)

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
      cast_tab <- add_lead_to_cast_tab(main     = main,  
                                       settings = settings,
                                       cast_tab = cast_tab)
      cast_tab <- add_covered_to_cast_tab(main     = main,  
                                          settings = settings,
                                          cast_tab = cast_tab)

    }

  }

  # patch
  colnames(cast_tab)[colnames(cast_tab) %in% c("data_set", "dataset")] <- "dataset"
  # patch

  # patch
  cast_tab$dataset <- gsub("dm_", "", gsub("_interp", "", cast_tab$dataset))
  # patch

  cast_ids                <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models                  <- ifnull(models, unique(cast_tab$model))
  dataset          <- ifnull(dataset, unique(cast_tab$dataset)[1])
  species                 <- ifnull(species, eval_species()) 
  end_moons               <- ifnull(end_moons, unique(cast_tab$end_moon)) 
  cast_id_in              <- cast_tab$cast_id %in% cast_ids
  model_in                <- cast_tab$model %in% models
  dataset_in       <- cast_tab$dataset == dataset
  species_in              <- cast_tab$species %in% species
  end_moon_in             <- cast_tab$end_moon %in% end_moons
  all_in                  <- cast_id_in & model_in & dataset_in & species_in & end_moon_in

  if (sum(all_in) == 0) {

    stop("no casts available for requested plot")

  }

  cast_tab <- cast_tab[all_in, ]


  lp    <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")

  if (ensemble) {

    ecast_tab <- data.frame()

    for (i in 1:length(end_moons)) {

      ecast_tab <- rbind(ecast_tab, 
                         ensemble_casts(main     = main, 
                                        settings = settings,
                                        cast_tab = cast_tab,
                                        end_moon = end_moons[i],
                                        models   = models, 
                                        dataset  = dataset,
                                        species  = species))

    }

    ecast_tab <- ecast_tab[ , -which(colnames(ecast_tab) == "var")]
    models    <- c(models, as.character(unique(ecast_tab$model)))
    cast_tab  <- cast_tab[ , colnames(cast_tab) %in% colnames(ecast_tab)]
    cast_tab  <- rbind(cast_tab, ecast_tab)

  }

  cast_level_errs <- measure_cast_level_error(cast_tab = cast_tab)
  nmodels <- length(models)
  nspecies <- length(species)


  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(fig = c(0, 1, 0, 1), mar = c(0.5, 0, 0, 0.5))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n")
  models2 <- gsub("ensemble_unwtavg", "Ensemble", models)
  x1 <- 0
  x2 <- 0.48
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = models2, cex = 0.7, 
       xpd = TRUE, srt = 45, adj = 1)
  x1 <- 0.49
  x2 <- 0.97
  y1 <- 0.0
  y2 <- 0.05
  par(mar = c(0, 2.5, 0, 0.5), fig = c(x1, x2, y1, y2), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
       bty = "n", xlim = c(0.5, nmodels + 0.5), ylim = c(0, 1))
  text(x = 1:nmodels, y = rep(0.9, nmodels), labels = models2, cex = 0.7, 
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
#' @description Plot the raw error (estimate - observation) as a function of lead time across model runs from different forecast origins (\code{end_moons}) for multiple models and multiple species (or total) within a data set.
#'
#' @details A pre-loaded table of casts can be input, but if not (default), the table will be efficiently (as defined by the inputs) loaded and trimmed. \cr 
#'  The casts can be trimmed specifically using the \code{cast_ids} input, otherwise, all relevant casts will be plotted. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param cast_ids \code{integer} (or integer \code{numeric}) values representing the casts of interest for restricting plotting, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param end_moons \code{integer} (or integer \code{numeric}) newmoon numbers of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param cast_tab Optional \code{data.frame} of cast table outputs. If not input, will be loaded.
#'
#' @param models \code{character} value(s) of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. \code{NULL} translates to all \code{models} in the table.
#'
#' @param dataset \code{character} value of the rodent data set to include. Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param ensemble \code{logical} indicator of if an ensemble should be included. Presently only the unweighted average. See \code{\link{ensemble_casts}}.
#'
#' @param species \code{character} vector of the species code(s) or \code{"total"} for the total across species) to be plotted \code{NULL} translates to the species defined by \code{base_species}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = "AutoArima", end_moons = 515:520)
#'   plot_casts_err_lead()
#' }
#'
#' @export
#'
plot_casts_err_lead <- function (main           = ".", 
                                 settings       = directory_settings(), 
                                 cast_ids       = NULL, 
                                 cast_tab       = NULL, 
                                 end_moons      = NULL, 
                                 models         = NULL, 
                                 ensemble       = TRUE, 
                                 dataset        = "controls", 
                                 species        = NULL) {

  if (length(models) == 1 && models != "ensemble") {

    ensemble <- FALSE

  }

  if (is.null(cast_tab)) {

    cast_choices <- select_casts(main           = main, 
                                 settings       = settings,
                                 cast_ids       = cast_ids, 
                                 models         = models, 
                                 end_moons      = end_moons, 
                                 datasets       = dataset)

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
      cast_tab <- add_lead_to_cast_tab(main     = main,  
                                       settings = settings,
                                       cast_tab = cast_tab)
      cast_tab <- add_covered_to_cast_tab(main     = main,  
                                          settings = settings,
                                          cast_tab = cast_tab)

    }

  }

  # patch
  colnames(cast_tab)[colnames(cast_tab) %in% c("data_set", "dataset")] <- "dataset"
  # patch

  # patch
  cast_tab$dataset <- gsub("_interp", "", cast_tab$dataset)
  # patch

  cast_ids                <- ifnull(cast_ids, unique(cast_tab$cast_id))
  models                  <- ifnull(models, unique(cast_tab$model))
  dataset          <- ifnull(dataset, unique(cast_tab$dataset)[1])
  species                 <- ifnull(species, eval_species()) 
  end_moons               <- ifnull(end_moons, unique(cast_tab$end_moon)) 
  cast_id_in              <- cast_tab$cast_id %in% cast_ids
  model_in                <- cast_tab$model %in% models
  dataset_in       <- cast_tab$dataset == dataset
  species_in              <- cast_tab$species %in% species
  end_moon_in             <- cast_tab$end_moon %in% end_moons
  all_in                  <- cast_id_in & model_in & dataset_in & species_in & end_moon_in

  if (sum(all_in) == 0) {

    stop("no casts available for requested plot")

  }

  cast_tab <- cast_tab[all_in, ]


  lp    <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE)  
  sptab <- na_conformer(sptab, "speciescode")


  if (ensemble) {

    ecast_tab <- data.frame()

    for (i in 1:length(end_moons)) {

      ecast_tab <- rbind(ecast_tab, 
                         ensemble_casts(main     = main, 
                                        settings = settings,
                                        cast_tab = cast_tab,
                                        end_moon = end_moons[i],
                                        models   = models, 
                                        dataset  = dataset,
                                        species  = species))

    }



    ecast_tab <- ecast_tab[ , -which(colnames(ecast_tab) == "var")]
    models    <- c(models, as.character(unique(ecast_tab$model)))
    cast_tab  <- cast_tab[ , colnames(cast_tab) %in% colnames(ecast_tab)]
    cast_tab  <- rbind(cast_tab, ecast_tab)

  }

  nmodels <- length(models) 
  nspecies <- length(species)

  if (nmodels == 1 & nspecies == 1) {

    yy <- round(cast_tab$error, 3)
    yrange <- range(c(0, yy), na.rm = TRUE)
    xrange <- c(max(cast_tab$lead) + 0.25, 0)

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
    models <- gsub("ensemble_unwtavg", "Ensemble", models)

    if (species == "total") {

      spp <- "total abundance"
      title <- paste0(models, ", ", dataset, ", ", spp)

    } else {
      sppmatch <- which(sptab[ , "speciescode"] == species)
      spp <- sptab[sppmatch , "scientificname"]
      title <- eval(substitute(
                      expression(
                        paste(models_i, ", ", dataset, ", ", italic(spp))), 
                      env = list(spp = spp, dataset = dataset,  
                                 models_i = models)))

    }

    mtext(text = title, 
          side = 3, 
          cex  = 1.25, 
          line = 0.5, 
          at   = xrange[1], 
          adj  = 0)

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

        if (tolower(models[i]) == "ensemble_unwtavg") {

          cast_id_in <- cast_tab$cast_id %in% as.numeric(paste0(9999, cast_ids))

        } else {

          cast_id_in <- cast_tab$cast_id %in% cast_ids

        }

        dataset_in <- cast_tab$dataset == dataset
        model_in          <- cast_tab$model %in% models[i]
        species_in        <- cast_tab$species %in% species[j]
        end_moon_in       <- cast_tab$end_moon %in% end_moons

        all_in <- cast_id_in & model_in & dataset_in & species_in & end_moon_in

        pcast_tab <- cast_tab[all_in, ]

        par(bty = "L", 
            mar = c(0.5, 0.5, 0.25, 0.25), 
            fig = c(x1[i], x2[i], y1[j], y2[j]),
            new = TRUE)

        xrange <- c(max(pcast_tab$lead) + 0.25, 0)

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
          mod_name <- gsub("ensemble_unwtavg", "Ensemble", mod_name)

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

  invisible()

}





#' @title Plot Predictions for a Given Point in Time Across Multiple Species
#'
#' @description Plot the point value with confidence interval for a time point across multiple species. Casts can be selected either by supplying a \code{cast_id} number or any combination of \code{dataset}, \code{model}, and \code{end_moon}, which filter the available casts in unison. This plot type can only handle output from a single cast, so if multiple casts still remain, the one with the highest number is selected. To be more certain about cast selection, use the \code{cast_id} input.
#'
#' @details The resulting plot shows predictions as points (open white circles) with error, where the point represents the \code{estimate} and the bounds of the error are \code{lower_pi} and \code{upper_pi} in the \code{cast_table} saved output from a model. \cr
#'  As of \code{portalcasting v0.9.0}, this represents the mean and the 95\% prediction interval. If \code{with_census = TRUE}, the observations from the associated moon are plotted as blue filled squares. 
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param model \code{character} value of the name of the model to include. Default value is \code{NULL}, which equates to no selection with respect to \code{model}. Also available is \code{"Ensemble"}, which combines the models via \code{\link{ensemble_casts}}. 
#'
#' @param dataset \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param moon \code{integer} (or integer \code{numeric}) newmoon number for the plot. 
#'
#' @param cast_id \code{integer} (or integer \code{numeric}) value representing the cast of interest, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param species \code{character} vector of the species codes (or \code{"total"} for the total across species) to be plotted or \code{NULL} (default) to plot all species in \code{dataset}. 
#' 
#' @param highlight_sp \code{character} vector of the species codes (or \code{"total"} for the total across species) to be highlighted or \code{NULL} (default) to not highlight anything.
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast group to combine with an ensemble. If \code{NULL} (default), the most recent cast group is ensembled. 
#'
#' @param with_census \code{logical} toggle if the plot should include the observed data collected during the predicted census.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   plot_cast_point()
#' }
#'
#' @export
#'
plot_cast_point <- function (main         = ".", 
                             cast_id      = NULL, 
                             cast_groups  = NULL,
                             dataset      = NULL, 
                             model        = NULL, 
                             end_moon     = NULL, 
                             species      = NULL, 
                             highlight_sp = NULL,
                             moon         = NULL, 
                             with_census  = FALSE, 
                             settings     = directory_settings(),
                             quiet        = FALSE) {

  moons <- read_moons(main     = main, 
                      settings = settings)

  last_census_date       <- as.Date(max(moons$censusdate, na.rm = TRUE))
  which_last_census_moon <- which(moons$censusdate == last_census_date)
  last_census_moon       <- moons$newmoonnumber[which_last_census_moon]
  which_last_moon        <- max(which(moons$newmoondate < Sys.Date()))
  last_moon              <- moons$newmoonnumber[which_last_moon]
  alt_moon               <- ifelse(with_census, last_census_moon, last_moon + 1)
  moon                   <- ifnull(moon, alt_moon)

  model  <- ifnull(model, "Ensemble")
  model2 <- model

  if (!is.null(model) && tolower(model) == "ensemble") {

    model2 <- NULL

  }

  casts_meta <- select_casts(main      = main, 
                             settings  = settings,
                             cast_ids  = cast_id,
                             end_moons = end_moon, 
                             models    = model2, 
                             datasets  = dataset, 
                             quiet     = quiet)

  if (with_census) {

    casts_meta_moon1  <- casts_meta$end_moon + 1
    casts_meta_moon2  <- casts_meta$end_moon + casts_meta$lead_time
    casts_last_census <- last_census_moon >= casts_meta_moon1 & last_census_moon <= casts_meta_moon2 
    casts_meta        <- casts_meta[casts_last_census, ]

  }

  if (NROW(casts_meta) > 1) {

    which_max  <- which.max(casts_meta$cast_id)
    casts_meta <- casts_meta[which_max, ]

  }

  if (NROW(casts_meta) == 0) {

    stop("no casts available for requested plot")

  }

  max_obs <- 0
  if (with_census) {

    obs           <- read_rodents_table(main           = main, 
                                        settings       = settings,
                                        dataset = gsub("dm_", "", gsub("_interp", "", casts_meta$dataset)))
    colnames(obs) <- gsub("\\.", "", colnames(obs))
    sp_col        <- is_sp_col(obs, nadot = TRUE, total = TRUE)
    species       <- ifnull(species, colnames(obs)[sp_col])
    moon          <- ifnull(moon, unique(obs$moon))
    obs           <- obs[obs$newmoonnumber %in% moon, species, drop = FALSE]

    if (NROW(obs) == 0) {

      stop("no observations available for requested plot") 

    } 

    max_obs <- max(as.numeric(obs), na.rm = TRUE)

  }

  dataset <- gsub("_interp", "", casts_meta$dataset)

  if (!is.null(model) && tolower(model) == "ensemble") {

    preds   <- ensemble_casts(main        = main, 
                              cast_groups = cast_groups,
                              end_moon    = casts_meta$end_moon, 
                              dataset     = dataset, 
                              species     = species)

  } else {

    preds <- read_cast_tab(main     = main, 
                           settings = settings,
                           cast_id  = casts_meta$cast_id)

  }

  preds      <- na_conformer(preds, "species")
  species    <- ifnull(species, unique(preds$species))
  match_sp   <- (preds$species %in% species)
  moon       <- ifnull(moon, unique(preds$moon))
  match_moon <- (preds$moon %in% moon)
  colnames   <- c("moon", "species", "estimate", "lower_pi", "upper_pi")
  match_col  <- (colnames(preds) %in% colnames)
  preds      <- preds[match_sp & match_moon, match_col]

  moon_month          <- as.numeric(format(as.Date(moons$newmoondate[moons$newmoonnumber == moon]), "%m"))
  moon_year           <- as.numeric(format(as.Date(moons$newmoondate[moons$newmoonnumber == moon]), "%Y"))
  title_date          <- paste(month.abb[moon_month], moon_year, sep = " ")
  dataset_name <- dataset
  model_name          <- ifnull(model, casts_meta$model)
  title               <- paste0(title_date, ", " , model_name, ", ", dataset_name)

  preds   <- preds[order(preds$estimate, decreasing = TRUE), ]
  species <- preds$species
  nspp    <- length(species)
  rangey  <- c(nspp + 0.25, 0.75)
  rangex  <- c(0, max(c(preds$upper_pi, max_obs), na.rm = TRUE))

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(3.5, 9.5, 2, 1))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", yaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)
  mtext("Abundance", side = 1, cex = 1.5, line = 2.5)
  mtext("Species", side = 2, cex = 1.5, line = 8.25)
  mtext(title, side = 3, cex = 1.25, line = 0.5, at = 0, adj = 0)

  lpath <- file.path(main, settings$subdirectories$resources, "PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lpath) 
  sptab <- na_conformer(sptab, "speciescode")

  for (i in 1:nspp) {

    if (species[i] == "total") {

      lab_text <- "Total"
      lab_font <- 1

    } else {

      sppmatch <- which(sptab[ , "speciescode"] == species[i])
      lab_text <- sptab[sppmatch , "scientificname"]
      lab_font <- 3

    }

    axis(2, at = i, labels = lab_text, font = lab_font, las = 1, 
         cex.axis = 0.65, tck = 0, line = -0.5, lwd = 0)
    axis(2, at = i, labels = FALSE, las = 1, 
         cex.axis = 0.65, tck = -0.01)

  }

  for (i in 1:nspp) {

    low   <- max(c(preds$lower_pi[i], 0))
    up    <- preds$upper_pi[i]
    est   <- preds$estimate[i]
    vbars <- i + (0.015 * nspp * c(-1, 1))

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

      spmatch       <- preds$species[i]
      nasp          <- spmatch == "NA"
      spmatch[nasp] <- "NA."
      obsi          <- obs[ , spmatch]
      points(obsi, i, pch = 15, col = rgb(0, 0.4, 0.9, 0.8), cex = 1.25)
      points(obsi, i, pch = 0, col = rgb(0.2, 0.2, 0.2, 0.8), cex = 1.25)

    }   

  }

  invisible()

}



#' @title Visualize a Time Series Cast of a Species
#'
#' @description Plot an observed timeseries and cast timeseries with a prediction interval. \cr
#'  Casts can be selected either by supplying a \code{cast_id} number or any combination of \code{dataset}, \code{model}, and \code{end_moon}, which filter the available casts in unison. This plot type can only handle output from a single cast, so if multiple casts still remain, the one with the highest number is selected. To be more certain about cast selection, use the \code{cast_id} input.
#'
#' @details The resulting plot shows observations as a solid black line and predictions as a blue polygon with the bounds represent the error given by \code{lower_pi} and \code{upper_pi} and the bisecting blue line representing the \code{estimate} in the \code{cast_table} saved output from a model. \cr
#'  As of \code{portalcasting v0.9.0}, this represents the mean and the 95\% prediction interval. \cr
#'  Observations that occurred after the cast are shown connected directly to the pre-cast observation data (as the black solid line).
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number of the forecast origin. Default value is \code{NULL}, which equates to no selection with respect to \code{end_moon}.
#'
#' @param model \code{character} value of the name of the model to include. Default value is \code{NULL}, which equates to \code{"Ensemble"} or an unweighted combination of the most recent cast group's models via  \code{\link{ensemble_casts}}. 
#'
#' @param dataset \code{character} value of the rodent data set to include Default value is \code{NULL}, which equates to no selection with respect to \code{dataset}.
#'
#' @param cast_id \code{integer} (or integer \code{numeric}) value representing the cast of interest, as indexed within the directory in the \code{casts} sub folder. See the casts metadata file (\code{casts_metadata.csv}) for summary information.
#'
#' @param species \code{character} value of the species codes (or \code{"total"} for the total across species) to be plotted. \code{NULL} (default) also gives the total.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number for the beginning of the x-axis of the plot. \cr
#'  Does not influence the fit of the models, just the presentation. 
#'
#' @param cast_groups \code{integer} (or integer \code{numeric}) value of the cast group to combine with an ensemble. If \code{NULL} (default), the most recent cast group is ensembled. 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{NULL}. Plot is generated.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast(models = c("AutoArima", "NaiveArima"), end_moons = 515:520)
#'   plot_cast_ts()
#' }
#'
#' @export
#'
plot_cast_ts <- function (main        = ".", 
                          settings    = directory_settings(), 
                          cast_id     = NULL, 
                          cast_groups = NULL,
                          dataset     = NULL, 
                          model       = NULL, 
                          end_moon    = NULL, 
                          species     = NULL, 
                          start_moon  = 217, 
                          quiet       = FALSE) {

  casts_meta <- select_casts(main      = main, 
                             settings  = settings, 
                             cast_ids  = cast_id,
                             end_moons = end_moon, 
                             models    = model, 
                             datasets  = dataset, 
                             quiet     = quiet)

  if (NROW(casts_meta) > 1) {

    which_max  <- which.max(casts_meta$cast_id)
    casts_meta <- casts_meta[which_max, ]

  }

  if (NROW(casts_meta) == 0) {

    stop("no casts available for requested plot")

  }

  obs           <- read_rodents_table(main     = main, 
                                      settings = settings, 
                                        dataset = gsub("dm_", "", gsub("_interp", "", casts_meta$dataset)))
  colnames(obs) <- gsub("\\.", "", colnames(obs))

  sp_col  <- is_sp_col(obs, nadot = TRUE, total = TRUE)
  species <- ifnull(species, colnames(obs)[sp_col])

  mod_control <- read_model_controls(main = main, settings = settings)
  model   <- ifnull(model, names(mod_control)[1])
  temp_species <- mod_control[[model]]$species
  if (length(temp_species) == 1 && temp_species == "all") {
    species <- species
  } else {
    species <- species[species %in% temp_species]
  }
  if (length(species) > 1) {
    if ("total" %in% species) {
      species <- "total"
    } else {
      species <- species[1]
    }

  }

  if (!all(species %in% colnames(obs))) {

    stop("observations not available for requested species")

  }

  obs     <- obs[ , c("newmoonnumber", species)]
  dataset <- gsub("dm_", "", gsub("_interp", "", casts_meta$dataset))

  if (!is.null(model) && tolower(model) == "ensemble") {

    preds   <- ensemble_casts(main        = main, 
                              cast_groups = cast_groups,
                              end_moon    = casts_meta$end_moon, 
                              dataset     = dataset, 
                              species     = species)

  } else {

    preds <- read_cast_tab(main     = main, 
                           settings = settings,
                           cast_id  = casts_meta$cast_id)

  }

  if (!(species %in% preds$species)) {

    stop("cast not available for requested species")

  }

  match_sp  <- (preds$species %in% species)
  colnames  <- c("moon", "estimate", "lower_pi", "upper_pi")
  match_col <- (colnames(preds) %in% colnames)
  preds     <- preds[match_sp, match_col]

  max_moon <- max(preds$moon)
  rangex   <- c(start_moon, max_moon)
  obs_sp   <- obs[ , species]
  obs_nm   <- obs[ , "newmoonnumber"]
  maxy     <- max(c(preds$UpperPI, obs_sp), na.rm = TRUE)
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

  moons <- read_moons(main     = main, 
                      settings = settings)
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
  p_x   <- preds[ , "moon"]

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

  # pull the nice model name from the model controls yaml file based on settings list

  model_name   <- ifnull(model, casts_meta$model)
  dataset_name <- dataset


  title        <- paste0(model_name, ", ", dataset_name)

  mtext(text = title, side = 3, cex = 1.25, line = 0.5, at = 217, adj = 0)

}