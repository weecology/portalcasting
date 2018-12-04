windows(14, 8)

tree = dirtree()
moons = prep_moons()
tmnt_type = "all"
species = "total"
fdate = "2018-07-20"
cast_type = "hindcasts"
model = "Ensemble"
hist_end = 453
xrange = c(217, 520) 
yrange = NULL
add_observes = TRUE

hist_local_path <- paste0("data/", tmnt_type, ".csv")
hist_path <- file_path(tree, hist_local_path)
hist <- read.csv(hist_path, stringsAsFactors = FALSE)

fcast_local_path <- paste0("predictions/", fdate, cast_type, ".csv")
fcast_path <- file_path(tree, fcast_local_path)
fcast <- read.csv(fcast_path, stringsAsFactors = FALSE)

if (is.null(hist_end)){
  hist_end <- max(hist$newmoonnumber)
}

hist_matches <- which(hist$newmoonnumber <= hist_end)
hist_x <- hist[hist_matches, "newmoonnumber"]
hist_y <- hist[hist_matches, species]
level <- switch(tmnt_type, "controls" = "Controls", "all" = "All")
matches <- which(fcast$model == model & fcast$species == species & 
                 fcast$level == level & fcast$initial_newmoon == hist_end)
fcast_x <- fcast[matches, "newmoonnumber"]
fcast_ye <- fcast[matches, "estimate"]
fcast_yl <- fcast[matches, "LowerPI"]
fcast_yu <- fcast[matches, "UpperPI"]

if (is.null(xrange)){
  xrange <- c(min(hist_x), max(fcast_x))
}
if (is.null(yrange)){
  yrange <- c(min(c(0, hist_y, fcast_yl)), max(c(hist_y, fcast_yu)))
}
par(mar = c(3, 6, 2, 2))
nullplot(xrange, yrange)

points(hist_x, hist_y, type = "l", lwd = 2)
predx <- c(fcast_x, flip(fcast_x), fcast_x[1])
predy <- c(fcast_yl, flip(fcast_yu), fcast_yl[1])
polygon(predx, predy, border = NA, col = rgb(0.5, 0.6, 0.9))
lastx <- max(hist_x)
lasty <- hist_y[hist_x == lastx]
fcastx <- c(lastx, fcast_x)
fcasty <- c(lasty, fcast_ye)
points(fcastx, fcasty, type = "l", lwd = 3, lty = 3)


axis(2, cex.axis = 1.5, las = 1)
mtext(side = 2, line = 3.75, cex = 2.75, "Total abundance")

dates <- moons[moons$newmoonnumber %in% xrange, "newmoondate"]
yr_dates <- seq.Date(min(dates), max(dates), "year")
yr5_dates <- yr_dates[which(as.numeric(format(yr_dates, "%Y")) %% 5 == 0)]
label_text <- format(yr5_dates, "%Y")
label_pos <- predict(lm(xrange ~ dates), newdata = list(dates = yr5_dates))
axis(1, cex.axis = 1.5, labels = label_text, at = label_pos)
tck_pos <- predict(lm(xrange ~ dates), newdata = list(dates = yr_dates)) 
axis(1, labels = FALSE, at = tck_pos, tck = -0.008)

if (cast_type == "hindcasts" & add_observes){
  hist2 <- hist
  hist2_matches <- which(hist2$newmoonnumber %in% fcast_x)
  hist2_x <- hist[hist2_matches, "newmoonnumber"]
  hist2_y <- hist[hist2_matches, species]
  hist2_x2 <- c(lastx, hist2_x)
  hist2_y2 <- c(lasty, hist2_y)
  points(hist2_x2, hist2_y2, type = "l", lwd = 3, col = rgb(0.7, 0.7, 0.7))
}









