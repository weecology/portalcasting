plot_cast_ts <- function(main = ".", species = NULL, cast_id = NULL, 
                         start_moon = 300, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(species)
  cast_meta <- read_cast_metadata(main, arg_checks)
  cast_id <- ifnull(cast_id, max(cast_meta$cast_id))
  return_if_null(cast_id)
  pcast <- cast_meta[cast_meta$cast_id == cast_id, ]
  if(NROW(pcast) == 0){
    stop("cast_id not in cast metadata table")
  }
  obs <- read_rodents_table(main, pcast$data_set, arg_checks)
  obs_sp <- obs[ , species]
  obs_nm <- obs[ , "moon"]
  preds <- read_cast_tab(main, cast_id, arg_checks)
  which_sp <- which(preds$species == species)
  preds <- preds[which_sp, ]

  max_moon <- max(preds$moon)
  rangex <- c(start_moon, max_moon)
  rangey <- c(0, max(c(preds$UpperPI, obs_sp), na.rm = TRUE))
  plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "", xaxt= "n", 
       las = 1, xlim = rangex, ylim = rangey)

  moons <- read_moons(main, arg_checks)
  minx <- as.character(moons$moondate[moons$moon == rangex[1]])
  maxx <- as.character(moons$moondate[moons$moon == rangex[2]])
  minx_yr <- as.numeric(format(as.Date(minx), "%Y"))
  maxx_yr <- as.numeric(format(as.Date(maxx), "%Y"))
  minx_yr2 <- ceiling(minx_yr/ 5) * 5
  maxx_yr2 <- floor(maxx_yr/ 5) * 5
  yrs <- seq(minx_yr2, maxx_yr2, 5)
  txt <- yrs
  nyd <- paste0(yrs, "-01-01")
  dx <- as.Date(as.character(moons$moondate))
  dy <- moons$moon
  dmod <- lm(dy ~ dx)
  loc <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(1, at = loc, labels = txt)
  yrs <- seq(minx_yr, maxx_yr, 1)
  nyd <- paste0(yrs, "-01-01")
  loc <- predict(dmod, newdata = list(dx = as.Date(nyd)))
  axis(1, at = loc, labels = FALSE, tck = -0.005)  

  lab <- list(text = "", font = 1)
  lp <- file_paths(main, "raw/PortalData/Rodents/Portal_rodent_species.csv")
  sptab <- read.csv(lp, stringsAsFactors = FALSE) %>% 
           na_conformer("speciescode")
  if (species == "total"){
    lab$text <- "Total Abundance"
    lab$font <- 1
  } else{
    sppmatch <- which(sptab[ , "speciescode"] == species)
    lab$text <- sptab[sppmatch , "scientificname"]
    lab$font <- 3
  }
  mtext(lab$text, side = 2, font = lab$font, cex = 1.5, line = 3)

  o_x <- obs_nm
  o_y <- obs_sp
  p_y_m <- preds[ , "estimate"]
  p_y_l <- preds[ , "lower_pi"]
  p_y_u <- preds[ , "upper_pi"]
  p_x <- preds[ , "moon"]
  for(i in 1:length(p_y_l)){
    p_y_l[i] <- max(c(0, p_y_l[i]))
  }
  first_pred <- min(p_x)
  last_o_x <- max(o_x[!is.na(o_y) & o_x < first_pred])
  last_o_y <- o_y[o_x == last_o_x]

  poly_x <- c(last_o_x, p_x, p_x[length(p_x):1], last_o_x)
  poly_y <- c(last_o_y, p_y_l, p_y_u[length(p_y_u):1], last_o_y)
  polygon(poly_x, poly_y, col = rgb(0.6757, 0.8438, 0.8984), border = NA)
  points(c(last_o_x, p_x), c(last_o_y, p_y_m), type = "l", lwd = 2, lty = 3,
         col = rgb(0.2, 0.4, 0.9))

  o_x_1 <- o_x[!is.na(o_y) & o_x < first_pred]
  o_y_1 <- o_y[!is.na(o_y) & o_x < first_pred]
  n_o_1 <- length(o_x_1)
  o_x_2 <- c(o_x_1[n_o_1], o_x[!is.na(o_y) & o_x >= first_pred])
  o_y_2 <- c(o_y_1[n_o_1], o_y[!is.na(o_y) & o_x >= first_pred])
  points(o_x_1, o_y_1, type = "l", lwd = 2)
  points(o_x_2, o_y_2, type = "l", lwd = 2, lty = 2)

}



read_cast_tab <- function(main = ".", cast_id = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(cast_id)
  lpath <- paste0("casts/cast_id_", cast_id, "_cast_tab.csv")
  cpath <- file_paths(main, lpath, arg_checks)
  if(!file.exists(cpath)){
    stop("cast_id does not have a cast_table")
  }
  read.csv(cpath, stringsAsFactors = FALSE) 

}