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
                                        dataset = gsub("_interp", "", casts_meta$dataset))
  colnames(obs) <- gsub("\\.", "", colnames(obs))

  sp_col  <- is_sp_col(obs, nadot = TRUE, total = TRUE)
  species <- ifnull(species, colnames(obs)[sp_col])
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